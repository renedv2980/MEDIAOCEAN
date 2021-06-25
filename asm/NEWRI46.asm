*          DATA SET NEWRI46    AT LEVEL 025 AS OF 06/28/06                      
*          DATA SET NEWRI46    AT LEVEL 045 AS OF 07/10/92                      
*PHASE T32046A,+0                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE NETACC                                                                 
*INCLUDE NEPACC                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE COVAIL                                                                 
         TITLE 'T32046 - PNG REPORT PHASE'                                      
T32046   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW46**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T32046,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4         ANETWS4=WORKING STORAGE                       
         USING MYD,R7                                                           
         MVC   NBACLI,ANETWS1     CLIENT RECORD IN WS1                          
         ST    R2,RELO                                                          
         MVC   DTITLE,=C' **MYD**'                                              
         CLI   DOWNOPT,C'Y'        PXZ TESTIF DOWNLOAD/NO HEADING               
         BE    SKIPH                                                            
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         SPACE 1                                                                
SKIPH    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
*                                                                               
         BAS   RE,INITRTN          COVAIL AND BUFFALO                           
         MVC   AGYCODE,=C'SJ'      FOR SJR                                      
         MVC   BRDCODE(3),=C'PXZ'     FOR SJR                                   
         CLC   NBSELAGY,=C'SJ'                                                  
         BE    RP3                                                              
         MVC   AGYCODE,=C'03'      FOR DF                                       
         MVC   BRDCODE,=C'0303'     FOR DF                                      
         CLC   NBSELAGY,=C'DF'                                                  
         BE    RP3                                                              
         MVC   AGYCODE,=C'01'                                                   
         MVC   BRDCODE,=C'0301'                                                 
         CLC   NBSELAGY,=C'DU'      FOR DU                                      
         BE    RP3                                                              
         CLC   NBSELAGY,=C'H9'      FOR H9                                      
         BE    RP3                                                              
         DC    H'0'                                                             
*                                                                               
RP3      GOTO1 DATCON,DMCB,(5,TODAT),(X'20',TODAT)         YYMMDD               
         MVC   FULL(2),TODAT                    AND WE NEED MMDDYY              
         MVC   TODAT(4),TODAT+2                                                 
         MVC   TODAT+4(2),FULL                                                  
         MVI   NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         MVI   NBSEQ,C'P'          X'94' KEY,ESTIMATE HIGH                      
         MVI   NBDATA,C'U'                                                      
         MVI   CLTSV,0                                                          
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBVALDAT                                                  
         BNE   RP10                                                             
         L     R1,ATWA                                                          
         USING T320FFD,R1                                                       
         MVI   NBUSER+2,C'B'                                                    
         CLI   SPLCAL,X'40'                                                     
         BNH   *+10                                                             
         MVC   NBUSER+2(1),SPLCAL                                               
         MVC   NBPEROVR(1),SPLCAL                                               
         DROP  R1                                                               
         MVC   NUMMONS,=F'12'      GET 12 MONTHS                                
         MVI   PERTYPE,C'M'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         MVC   NUMMONS,=F'6'      AND ONLY USE FIRST SIX                        
         GOTO1 DATCON,DMCB,(2,MONLIST),(X'20',RQCHARP)  GET REQUEST PER         
*                                                                               
         CLC   RQCHARP+2(2),=C'12'    IF DECEMBER/BROADCAST MONTHS SO           
         BE    *+14                                                             
         CLC   RQCHARP+2(2),=C'06'  IF JUNE/BROADCAST                           
         BNE   RP20                                                             
         GOTO1 ADDAY,DMCB,RQCHARP,(X'20',RQCHARP),F'8'  BUMP TO                 
*                                                    FIRST HALF                 
RP20     CLC   RQCHARP+2(2),=C'06'              FIRST OR LAST 6 MONTHS          
         BH    *+12                                 YY01/YY02                   
         MVI   RQCHARP+2,C'1'                                                   
         B     *+8                                                              
         MVI   RQCHARP+2,C'2'                                                   
*                                                                               
*                                                                               
RP30     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    RP35                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   RP30                                                             
         B     RP80                                                             
         EJECT                                                                  
*                                                                               
RP35     DS    0H                                                               
***      CLC   =C'10MV11',NBACTPRG                                              
***      BNE   RP30                                                             
         CLI   CLTSV,0             FIRST TIME                                   
         BNE   RP40                                                             
         BAS   RE,XCTBL                                                         
         MVC   ESTSV,NBACTEST                                                   
         MVC   CLTSV,NBACTCLI                                                   
**-->    BAS   RE,SETCTBL          FILL COMMISSION RATE TABLE                   
*                                                                               
RP40     DS    0H                  CHK CLT/EST BREAK                            
         CLC   CLTSV,NBACTCLI                                                   
         BNE   RP50                                                             
         CLC   ESTSV,NBACTEST                                                   
         BE    RP60                                                             
*                                                                               
RP50     BAS   RE,RDPGEST          READ PGEST REC/WRITE TO BUFF                 
         BAS   RE,XCTBL                                                         
         MVC   CLTSV,NBACTCLI                                                   
         MVC   ESTSV,NBACTEST                                                   
         BAS   RE,SETCTBL                                                       
*                                                                               
RP60     BAS   RE,DOUNIT                                                        
         B     RP30                                                             
*                                                                               
*                                  MODE=REQLAST                                 
RP80     CLI   CLTSV,0                                                          
         BE    *+12                                                             
         BAS   RE,RDPGEST                                                       
         BAS   RE,XCTBL                                                         
***-->   BAS   RE,SHORTEST                                                      
         BAS   RE,CLRBUFF          GET RECS FROM BUFF                           
*                                                                               
RP85     GOTO1 =V(COVAIL),DMCB,C'FREE',ABUFF,COVPARM                            
*                                                                               
         B     XIT                                                              
*                                                                               
INITRTN  NTR1                                                                   
         L     R6,=A(BUFFALOC)                                                  
         GOTO1 =V(COVAIL),DMCB,C'SETB',500000,100000,(R6)                       
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   COVPARM,DMCB+8                                                   
         MVC   ABUFF,DMCB+12                                                    
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
SETCTBL  NTR1     FILL COMMISSION RATE TABLE/ READ ESTIMATE HEADER              
         LA    R2,CTABLE                                                        
CT5      CLI   0(R2),0             CLEAR TABLE                                  
         BE    CT10                                                             
         XC    0(CTBLENE,R2),0(R2)                                              
         LA    R2,CTBLENE(R2)                                                   
         B     CT5                                                              
CT10     NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     CT20                                                             
*                                                                               
CT15     MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
CT20     CLC   KEY(4),KEYSAVE                                                   
         BNE   CTEND                                                            
         CLC   KEY+7(1),NBACTEST                                                
         BNE   CT15                                                             
         OC    KEY+8(5),KEY+8                                                   
         BNZ   CT15                                                             
*                                                                               
         MVC   AIO,ANETWS2                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         CLI   ERATE,C'8'          8=COMMISSION ONLY RATES                      
         BNE   CT15                                                             
         LA    R2,CTABLE                                                        
         USING CTABLED,R2                                                       
         CLI   0(R2),0                                                          
         BE    CT30                                                             
         LA    R2,CTBLENE(R2)                                                   
         B     *-12                                                             
CT30     MVC   CCLT,NBACTCLI                                                    
         MVC   CEST,NBACTEST                                                    
         MVC   CPRD,KEY+4          3 BYTE PROD FROM EST KEY                     
         MVC   CRATE,EBILLCOM      RATE                                         
         MVI   CRTYPE,C'G'                                                      
         TM    EBILLBAS,X'01'                                                   
         BNO   *+8                                                              
         MVI   CRTYPE,C'N'                                                      
         B     CT15                                                             
*                                                                               
CTEND    NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
         MVC   AIO,NBAIO                                                        
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*  SETS UP CLT/EST/PRD ACTUAL AND ASSIGNED DOLLAR BUCKETS                       
DOUNIT   NTR1                                                                   
DO01     LA    R4,DUB                                                           
         USING TABLED,R4                                                        
         MVC   TCLT,NBACTCLI                                                    
         MVC   TEST,NBACTEST                                                    
         MVC   TPRD,NBSPLPRN                                                    
         BAS   RE,GETPRD                                                        
         MVC   TPRDNM,WORK                                                      
         L     R4,=A(TABLE)                                                     
         LA    R3,255              MAX PRODS                                    
DO10     CLI   0(R4),0                                                          
         BNE   DO12                                                             
         MVI   DUB+6,0                 SET SUP FIRST                            
         MVC   DUB+3(3),=C'SUP'                                                 
         B     DO20                                                             
DO11     CLI   0(R4),0                                                          
         BE    DO20                                                             
DO12     CLC   DUB(7),0(R4)                                                     
         BE    DO25                                                             
         LA    R4,TLENE(R4)                                                     
         BCT   R3,DO11                                                          
         DC    H'0'                NO MATCH/NO ROOM                             
DO20     MVC   0(7,R4),DUB         FIRST TIME SET KEY                           
         LA    R3,TMON1                       PREPARE FIELDS                    
         LA    R0,48                                                            
DO22     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R0,DO22                                                          
         CLC   =C'SUP',DUB+3           WAS THIS INITIAL SUP SET?                
         BE     DO01                   YES-ADD FIRST PROD                       
*                                                                               
DO25     CLC   =C'UNL',DUB+3           IS THI UNALLOCATED?                      
         BNE   *+10                                                             
         MVC   WORK(3),=C'UNL'                                                  
         LA    R4,TMON1                                                         
         LA    R2,MONLIST                                                       
         L     R3,NUMMONS                                                       
DO30     CLC   NBACTDAT,2(R2)                                                   
         BNH   DO40                                                             
         LA    R2,4(R2)                                                         
         LA    R4,TMONLENE(R4)                                                  
         BCT   R3,DO30                                                          
         B     DOX                 EXIT IF DATES NOT WITHIN RANGE               
*                                                                               
DO40     LA    R5,CTABLE           IS IT COMMISSION-ONLY EST                    
         B     DO60        ****--> NO COMMISSION -ONLY EST                      
         USING CTABLED,R5                                                       
DO42     CLI   0(R5),0             TRY PROD SPECIFIC FIRST                      
         BE    DO45A                                                            
         CLC   CCLT(3),DUB         CLIENT/EST                                   
         BNE   DO45                                                             
         CLC   CPRD,DUB+3          3 BYTE PROD                                  
         BE    DO46                                                             
DO45     LA    R5,CTBLENE(R5)                                                   
         B     DO42                                                             
DO45A    LA    R5,CTABLE           THEN TRY POL ESTIMATE                        
         CLC   CCLT(3),DUB         CLIENT/EST                                   
         BNE   DO45B                                                            
         CLC   CPRD,=C'POL'                                                     
         BE    DO46                                                             
DO45B    LA    R5,CTBLENE(R5)                                                   
         CLI   0(R5),0                                                          
         BNE   DO45A+4                                                          
         B     DO60                NO MATCH                                     
         EJECT                                                                  
* - COMMISSION-ONLY ESTIMATE FOR F5 RECORD / F6 GETS NORMAL PROCESSING          
*   R5 POINTS TO CTABLE   R4 POINTS TO TABLED                                   
*                                                                               
DO46     CLI   CRTYPE,C'N'         IS IT NET RATE TYPE                          
         BE    DO47                                                             
***      GOTO1 =V(NETACC),DMCB,(28,NETACFLD),NETBLOCK      28=TIS               
         GOTO1 =V(NETACC),DMCB,(158,NETACFLD),NETBLOCK   ASSACTNET              
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
         LR    R2,R5               SAVE R5                                      
         LA    R5,DUB                                                           
         BAS   RE,DO99                                                          
         AP    8(8,R4),DUB                                                      
         MVC   DUB,NETACFLD+1                                                   
         LR    R5,R2               RESET R5                                     
         B     DO50                                                             
DO47     GOTO1 =V(NETACC),DMCB,(38,NETACFLD),NETBLOCK    38=TIS(NET)            
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
*                                                                               
DO50     CVB   R1,DUB       PUT  COMMISSION DOLLARS INTO ASSIGN SLOT            
         ICM   R0,15,CRATE                                                      
         LPR   R0,R0                                                            
         SLL   R0,1                                                             
         MR    R0,R0                                                            
         D     R0,=F'1000000'                                                   
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
*        CVB   R0,DUB                                                           
*        TM    CRATE,X'80'         ..IS RATE NEGATINE                           
*        BO    DO55                                                             
*        AR    R0,R1               ..NO ADD IT                                  
*        B     *+6                                                              
*DO55     SR    R0,R1               ..YES SUBTRACT                              
*        CVD   R0,DUB                                                           
         CVD   R1,DUB                                                           
         AP    16(8,R4),DUB                                                     
         B     DOX                                                              
         DROP  R5                                                               
*                                                                               
* - STANDARD ESTIMATE                                                           
DO60     GOTO1 =V(NETACC),DMCB,(158,NETACFLD),NETBLOCK  158 ASS/ACT             
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
***      GOTO1 =V(NETACC),DMCB,(19,NETACFLD),NETBLOCK   19 INT NET              
***      MVC   DUB,NETACFLD+1                                                   
***      AP    0(8,R4),DUB                                                      
         GOTO1 =V(NETACC),DMCB,(33,NETACFLD),NETBLOCK   33 SPECIAL NET          
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
**       LA    R5,DUB                                                           
**       BAS   RE,DO99                                                          
**       AP    8(8,R4),DUB                                                      
**       GOTO1 =V(NETACC),DMCB,(2,NETACFLD),NETBLOCK       2=ASS+I              
**       MVC   DUB,NETACFLD+1                                                   
**       GOTO1 =V(NETACC),DMCB,(23,NETACFLD),NETBLOCK      23=SPECIAL           
**       AP    DUB,NETACFLD+1(8)                                                
**       AP    16(8,R4),DUB                                                     
**       LA    R5,DUB                                                           
**       BAS   RE,DO99                                                          
**       AP    24(8,R4),DUB                                                     
*                              NOW GET $$$ FOR PROD=SUP                         
         CLC   =C'UNL',WORK            SKIP IF UNALLOCATED                      
         BNE   DO70                                                             
         XC    WORK,WORK                                                        
         B     DOX                                                              
DO70     MVC   DUB(2),NBACTCLI                                                  
         MVC   DUB+2(1),NBACTEST                                                
         MVC   DUB+3(3),=C'SUP'                                                 
         MVI   DUB+6,0                                                          
         L     R4,=A(TABLE)                                                     
         USING TABLED,R4                                                        
         LA    R3,255                  MAX PRODS                                
DO80     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                    SHOULD NEVER HAPPEN                      
         CLC   DUB(7),0(R4)                                                     
         BE    DO95                                                             
         LA    R4,TLENE(R4)                                                     
         BCT   R3,DO80                                                          
         DC    H'0'                    SHOULD NOT GET HERE                      
DO95     DS    0H                                                               
         LA    R4,TMON1                                                         
         LA    R2,MONLIST                                                       
         L     R3,NUMMONS                                                       
DO100    CLC   NBACTDAT,2(R2)                                                   
         BNH   DO110                                                            
         LA    R2,4(R2)                                                         
         LA    R4,TMONLENE(R4)                                                  
         BCT   R3,DO100                                                         
         B     DOX                 EXIT IF DATES NOT WITHIN RANGE               
         DROP  R4                                                               
*                                                                               
DO110    DS    0H                                                               
         GOTO1 =V(NETACC),DMCB,(31,NETACFLD),NETBLOCK  31 ORDRTN                
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
         GOTO1 =V(NETACC),DMCB,(19,NETACFLD),NETBLOCK   19 INT NET              
         MVC   DUB,NETACFLD+1                                                   
         AP    0(8,R4),DUB                                                      
         GOTO1 =V(NETACC),DMCB,(151,NETACFLD),NETBLOCK 151 NET ASSIGN           
         MVC   DUB,NETACFLD+1                                                   
         SP    0(8,R4),DUB                                                      
DOX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  READ PGEST RECORDS                           
RDPGEST  NTR1                                                                   
         NETGO NVSETSPT,DMCB       SET SPOT                                     
         LA    R2,KEY                                                           
         USING PGKEY,R2                                                         
         L     R4,=A(TABLE)                                                     
         USING TABLED,R4                                                        
PG10     CLI   TCLT,0                                                           
         BE    PG100                                                            
         XC    BRANDSV,BRANDSV                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D5D'     READ FOR OLD KEY                             
         MVC   PGKAM,NBACTAM                                                    
         MVC   PGKCLT,TCLT                                                      
         MVC   PGKNETE,TEST                                                     
         MVC   PGKNETP,TPRDNM                                                   
         CLC   =C'SUP',TPRDNM                                                   
         BNE   *+8                                                              
         MVI   PRDSUP,C'Y'                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PG30                                                             
*                                                                               
         XC    KEY,KEY             NO / TRY FOR NEW KEY                         
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   PGKAM,NBACTAM                                                    
         MVC   PGKCLT,TCLT                                                      
         MVI   PGKNETE,0           READ EST=000 FIRST FOR BRAND CODE            
         MVC   PGKNETP,TPRDNM                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PG90                                                             
         MVC   AIO,ANETWS2                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   PG90                                                             
         USING PGNETELM,R6                                                      
         MVC   BRANDSV(3),PGNETPB     SAVE BRAND                                
         MVC   BRANDSV+3(1),PGNETPB2     SAVE BRAND                             
         MVC   BRANDSFX,PGNETBFX         BRAND SUFFIXND                         
*                                                                               
         XC    KEY,KEY             NOW READ FOR PRD=POL RECORD                  
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   PGKAM,NBACTAM                                                    
         MVC   PGKCLT,TCLT                                                      
         MVC   PGKNETE,TEST                                                     
         MVC   PGKNETP,=C'POL'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PG90                                                             
*                                                                               
PG30     DS    0H                  NOW GET PGESTREC                             
         L     R6,ANETWS2                                                       
         A     R6,=F'500'                                                       
         ST    R6,AIO                                                           
         CLC   KEY(13),0(R6)       DO WE ALREADY HAVE REC                       
         BE    PG35                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PGKEY,R6                                                         
PG35     MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   PG90                                                             
         USING PGNETELM,R6                                                      
         CLC   PGNETCP,RQCHARP     CHK REQUEST DATES                            
         BNE   PG90                                                             
         XC    BFREC,BFREC                                                      
         MVC   B4ID,=C'F4'                F4 RECORD                             
         MVC   B4CODE,=C'A'                                                     
         MVC   B4CHPER,PGNETCP                                                  
         MVC   B4AGY,AGYCODE                                                    
         MVC   B4ACC(3),PGNETACC                                                
         MVC   B4ACC+3(3),PGNETAC2                                              
         MVC   B4PROD(3),PGNETPB                                                
         MVC   B4PROD+3(1),PGNETPB2                                             
         CLI   BRANDSV,0            IF NEW TYPE RECORD                          
         BE    *+10                                                             
         MVC   B4PROD,BRANDSV      USE BRAND FROM EST=000 REC                   
         CLI   PGNETNOB,C'Y'       IF NOBRAND=Y                                 
         BNE   *+10                                                             
         MVC   B4PROD,BRDCODE      SET 303/304 FOR BRAND                        
         MVC   B4EST,PGNETPE                                                    
         MVC   B4ESTDT,TODAT                                                    
         MVC   B4EVENT,PGNETEC                                                  
         MVC   B4MULTI,PGNETMB                                                  
*        GOTO1 =V(PRNTBL),DMCB,=C'F4IN',0(R6),C'DUMP',80,=C'1D'                 
*        GOTO1 =V(PRNTBL),DMCB,=C'MULT',B4MULTI,C'DUMP',1,=C'1D'                
         MVC   B4SPARE,SPACES                                                   
         LA    R1,B5MON1                                                        
         LA    R0,12                                                            
PG45     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PG45                                                          
         BAS   RE,PUTBUFF          PUT TO BUFFALLO                              
         BNO   *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*****    CLI   PRDSUP,C'Y'             IF SUP PROD                              
*****    BE    PG70                    NO F5 RECORD                             
         MVC   B5ID,=C'F5'                 MAKE IT F5                           
         MVC   B5SPR(B5SPRLE),SPACES      CLEAR NON-COMMON REC AREA             
PG55     LA    R3,TMON1                                                         
         CLI   PGNETCOM,C'Y'       COMMISSION-ONLY IN ASSSIGNED SLOT            
         BE    PG55A                                                            
         CLI   PGNETASS,C'Y'                                                    
         BNE   *+8                                                              
PG55A    LA    R3,16(R3)                                                        
         LA    RE,6                                                             
*                               PREPARE F5 FIELDS                               
         LA    R1,B5MON1                                                        
         LA    R0,12                                                            
PG57     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PG57                                                          
*                                                                               
         LA    R5,B5MON1                                                        
PG60     CLI   PGNETCOM,C'Y'       COMM-ONLY                                    
         BE    PG61                                                             
         CLI   PGNET993,C'Y'                                                    
         BE    *+14                                                             
PG61     AP    0(8,R5),0(8,R3)                                                  
         B     PG62                                                             
         AP    0(8,R5),8(8,R3)                                                  
PG62     LA    R3,32(R3)                                                        
         LA    R5,8(R5)                                                         
         BCT   RE,PG60                                                          
*                                                                               
         BAS   RE,PUTBUFF                                                       
         BNO   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PG70     CLI   PGNETNOB,C'N'       DO WE NEED F6                                
         BE    PG90                                                             
         MVI   PRDSUP,0                CLEAR SUP FLAG                           
         MVC   B6ID,=C'F6'              YES/                                    
         MVC   B6SPR(B6SPRLE),SPACES   CLEAR NO-COMMON AREAS                    
**       MVC   B6BSX,TPRDNM            UNIT DDS BRAND                           
         MVC   B6BSX,BRANDSFX          SUFFIX                                   
         MVC   B6DETBC(3),PGNETPB                                               
         MVC   B6DETBC+3(1),PGNETPB2                                            
         CLI   PGNETPB,0                                                        
         BNE   *+10                                                             
         MVC   B6DETBC,BRANDSV                                                  
         LA    R3,TMON1                                                         
         CLI   PGNETASS,C'Y'       ..ASSUME COMM-ONLY = NOT ASSIGN              
         BNE   *+8                 ..SO DOLLARS IN NORMAL SLOT                  
         LA    R3,16(R3)                                                        
         LA    R1,B6MON1                                                        
         LA    R0,12                                                            
PG77     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PG77                                                          
         LA    RE,6                                                             
         LA    R5,B6MON1                                                        
PG80     CLI   PGNET993,C'Y'                                                    
         BE    *+14                                                             
         AP    0(8,R5),0(8,R3)                                                  
         B     PG82                                                             
         AP    0(8,R5),8(8,R3)                                                  
PG82     LA    R3,32(R3)                                                        
         LA    R5,8(R5)                                                         
         BCT   RE,PG80                                                          
         BAS   RE,PUTBUFF                                                       
         BNO   *+6                                                              
         DC    H'0'                                                             
PG90     LA    R4,TLENE(R4)       BUMP TO NEXT TABLE ENTRY                      
         B     PG10                                                             
*                                                                               
PG100    NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
         MVC   AIO,NBAIO                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* READ ANY SHORT-RATE PGEST RECS AND ASSIGN APPROPRIATE DOLLARS                 
*                                                                               
SHORTEST NTR1                                                                   
         NETGO NVSETSPT,DMCB       READ PGEST POL RECS                          
         LA    R2,KEY                                                           
         USING PGKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   PGKAM,NBACTAM                                                    
         CLC   NBSELCLI,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   PGKCLT,NBACTCLI                                                  
         CLI   NBSELEST,0                                                       
         BE    *+10                                                             
         MVC   PGKNETE,NBSELEST                                                 
******** MVC   PGKNETP,=C'POL'                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     RT30                                                             
*                                                                               
RT5      NETGO NVSETSPT,DMCB         RESET SEQUENTIAL READ OF PGEST             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SHRTKEY                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RT10     MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
RT30     CLC   KEY(3),KEYSAVE      ID/AM                                        
         BNE   RTXX                                                             
         CLC   NBACTCLI,=C'ALL'                                                 
         BE    *+14                                                             
         CLC   PGKCLT,NBACTCLI     CLIENT                                       
         BNE   RTXX                                                             
         CLI   NBSELEST,0                                                       
         BE    *+14                                                             
         CLC   PGKNETE,NBSELEST    ESTIMATE                                     
         BNE   RT10                                                             
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    RT35                                                             
         CLC   NBSELPRD,=C'POL'                                                 
         BE    RT35                                                             
         CLI   NBSELPRD,X'40'                                                   
         BE    RT35                                                             
         CLC   PGKNETP,NBSELPRD                                                 
         BNE   RT10                                                             
*                                                                               
RT35     MVC   AIO,ANETWS2                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PGNETELM,R6                                                      
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   RT10                                                             
         OC    PGNETFYE,PGNETFYE   IS IT SHORT RATE ESTIMATE                    
         BZ    RT10                NO                                           
         CLI   PGNETXFR,0                                                       
         BE    RT10                                                             
         CLC   PGNETCP,RQCHARP     CHK REQUEST DATE VS CHARGE PERIOD            
         BNE   RT10                                                             
         MVC   EFCXFR,PGNETXFR     SAVE EFFECTIVE XFR ESTIMATE                  
         MVC   SHRTSTRT,PGNETFYE   SAVE SHORTRATE START                         
         MVC   SHRTKEY,PGKEY       SAVE SHORTRATE KEY                           
*                                                                               
         SPACE                                                                  
* - NOW READ BILLS FOR THE SHORT-RATE CLI/EST                                   
*                                                                               
         LA    R2,KEY                                                           
         USING BKEY,R2                                                          
         XC    KEY,KEY                                                          
         MVC   BKEYAM(3),SHRTKEY+2     AM AND CLT FROM SHORTRATE                
         MVC   BKEYEST,EFCXFR          EST FROM SHORTRATE                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     RT50                                                             
*                                                                               
RT40     LA    R2,KEY                                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
RT50     CLC   KEY(4),KEYSAVE      ID/AM/CLT                                    
         BNE   RT100                         DO F4,F5,F6                        
         OC    BKEYYSRV(5),BKEYYSRV  MAKE SURE ITS BILL REC                     
         BZ    RT40                                                             
         CLC   BKEYEST,EFCXFR      ESTIMATE                                     
         BNE   RT40                                                             
         CLC   NBSELPRD,=C'POL'                                                 
         BE    RT60                                                             
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    RT60                                                             
         CLC   NBSELPRD,BKEYPRD                                                 
         BNE   RT40                                                             
*                                                                               
RT60     MVC   AIO,ANETWS3                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         GOTO1 DATCON,DMCB,(0,BDATE),(2,HALF)                                   
         CLC   HALF,SHRTSTRT         BILLING RUN VS SHORT RATE START            
         BL    RT40                                                             
         L     R1,ANETWS1              CLIENT RECORD                            
         CLC   1(3,R1),SHRTKEY+2   DO WE HAVE CLIENT RECORD                     
         BE    *+8                                                              
         BAS   RE,CLTREC           READ CLIENT RECORD                           
*                                                                               
         EJECT                                                                  
* - PREPARE TABLE AND LOAD BILLING DATA                                         
         LA    R4,DUB                                                           
         USING TABLED,R4                                                        
         MVC   TCLT,SHRTKEY+3      CLIENT                                       
         MVC   TEST,EFCXFR         EFFECTIVE XFR ESTIMATE                       
         MVC   TPRDNM,BKEYPRD                                                   
         MVC   FULL,BKEYPRD                                                     
         BAS   RE,GETPRDN                                                       
         MVC   FULL,BACTUAL        SAVE ACTUAL BILLING                          
         MVC   TPRD,BYTE                                                        
         L     R4,=A(TABLE)                                                     
         LA    R3,255              MAX PRODS                                    
DD10     CLI   0(R4),0                                                          
         BE    DD20                                                             
         CLC   DUB(7),0(R4)                                                     
         BE    DD25                                                             
         LA    R4,TLENE(R4)                                                     
         BCT   R3,DD10                                                          
         DC    H'0'                NO MATCH/NO ROOM                             
DD20     DS    0H                                                               
         MVC   0(7,R4),DUB         FIRST TIME SET KEY                           
         LA    R3,TMON1                       PREPARE FIELDS                    
         LA    R0,48                                                            
DD22     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R0,DD22                                                          
*                                                                               
DD25     LA    R4,TMON1                       FIND PROPER BUCKET                
         LA    R2,MONLIST                                                       
         L     R3,NUMMONS                                                       
DD30     CLC   HALF,2(R2)          HALF CONTAINS BILLING RUN DATE               
         BNH   DD60                                                             
         LA    R2,4(R2)                                                         
         LA    R4,TMONLENE(R4)                                                  
         BCT   R3,DD30                                                          
         B     RT40                SKIP IF BILL DATE NOT WITHIN RANGE           
*                                                                               
DD60     ICM   RE,15,FULL          FULL=BACTUAL                                 
         CVD   RE,DUB                                                           
         AP    0(8,R4),DUB                                                      
         SPACE                                                                  
DDX      B     RT40                GET NEXT BILL RECORD                         
         EJECT                                                                  
* - NOW STEP THROUGH TABLE PUTTING F4,F5,F6 TO BUFFALO                          
RT100    DS    0H                                                               
         L     R4,=A(TABLE)                                                     
         USING TABLED,R4                                                        
PP10     CLI   0(R4),0                                                          
         BE    RT1000                                                           
         MVC   FULL,TPRDNM                                                      
         BAS   RE,PGBRAND          GET PG BRAND / SETS BRANDSV                  
         L     R6,ANETWS2                                                       
         USING PGKEY,R6                                                         
PP35     MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGNETELM,R6                                                      
         XC    BFREC,BFREC                                                      
         MVC   B4ID,=C'F4'                F4 RECORD                             
         MVC   B4CODE,=C'A'                                                     
         MVC   B4CHPER,PGNETCP                                                  
         MVC   B4AGY,AGYCODE                                                    
         MVC   B4ACC(3),PGNETACC                                                
         MVC   B4ACC+3(3),PGNETAC2                                              
         MVC   B4PROD(3),PGNETPB                                                
         MVC   B4PROD+3(1),PGNETPB2                                             
         CLI   BRANDSV,0            IF NEW TYPE RECORD                          
         BE    *+10                                                             
         MVC   B4PROD,BRANDSV      USE BRAND FROM EST=000 REC                   
         CLI   PGNETNOB,C'Y'       IF NOBRAND=Y                                 
         BNE   *+10                                                             
         MVC   B4PROD,BRDCODE      SET 303/304 FOR BRAND                        
         MVC   B4EST,PGNETPE                                                    
         MVC   B4ESTDT,TODAT                                                    
         MVC   B4EVENT,PGNETEC                                                  
         MVC   B4MULTI,PGNETMB                                                  
         MVC   B4SPARE,SPACES                                                   
         LA    R1,B5MON1                                                        
         LA    R0,12                                                            
PP45     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PP45                                                          
         BAS   RE,PUTBUFF          PUT TO BUFFALLO                              
         BNO   *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
         MVC   B5ID,=C'F5'                 MAKE IT F5                           
         MVC   B5SPR(B5SPRLE),SPACES      CLEAR NON-COMMON REC AREA             
         LA    R1,B5MON1           PREPARE FIELDS                               
         LA    R0,12                                                            
PP57     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PP57                                                          
*                                                                               
         LA    RE,6                                                             
         LA    R3,TMON1                                                         
         LA    R5,B5MON1                                                        
PP60     AP    0(8,R5),0(8,R3)     SET IN ACTUAL BUCKET                         
         LA    R3,32(R3)                                                        
         LA    R5,8(R5)                                                         
         BCT   RE,PP60                                                          
*                                                                               
         BAS   RE,PUTBUFF                                                       
         BNO   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PGNETNOB,C'N'       DO WE NEED F6                                
         BE    PP90                                                             
         MVC   B6ID,=C'F6'              YES/                                    
         MVC   B6SPR(B6SPRLE),SPACES   CLEAR NO-COMMON AREAS                    
***      MVC   B6BSX,TPRDNM                                                     
         MVC   B6BSX,BRANDSFX                                                   
         MVC   B6DETBC(3),PGNETPB                                               
         MVC   B6DETBC+3(1),PGNETPB2                                            
         CLI   PGNETPB,0                                                        
         BNE   *+10                                                             
         MVC   B6DETBC,BRANDSV                                                  
         LA    R1,B6MON1           PREPARE PACKED FIELDS                        
         LA    R0,12                                                            
PP77     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,PP77                                                          
*                                                                               
         LA    RE,6                                                             
         LA    R3,TMON1                                                         
         LA    R5,B6MON1                                                        
PP80     AP    0(8,R5),0(8,R3)                                                  
         LA    R3,32(R3)                                                        
         LA    R5,8(R5)                                                         
         BCT   RE,PP80                                                          
         BAS   RE,PUTBUFF                                                       
         BNO   *+6                                                              
         DC    H'0'                                                             
PP90     LA    R4,TLENE(R4)       BUMP TO NEXT TABLE ENTRY                      
         B     PP10                                                             
*                                                                               
RT1000   DS    0H                                                               
         BAS   RE,XCTBL                                                         
         B     RT5                 READ NEXT PGEST SHORRATE REC                 
*                                                                               
RTXX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  PRINT REPORT/DOWNLOAD                                                        
*                                                                               
CLRBUFF  NTR1                                                                   
         MVI   NODATA,0                                                         
         CLI   DOWNOPT,C'Y'    IS IT DOWNLOAD                                   
         BE    DL5                                                              
         BAS   RE,HIGHBUFF     *** NO/ PRINT REPORT ***                         
         BO    PLX                                                              
         MVI   NODATA,1                                                         
         B     PL10                                                             
PL5      BAS   RE,SEQBUFF                                                       
         BO    PLX                                                              
PL10     CLC   B4ID,=C'F4'                                                      
         BE    PL40                                                             
         CLC   B4ID,=C'F5'                                                      
         BE    PL50                                                             
         CLC   B4ID,=C'F6'                                                      
         BE    PL60                                                             
         DC    H'0'                                                             
PL40     DS    0H                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   BYTE,4                                                           
         LA    R2,P                                                             
         USING P4D,R2                                                           
         MVC   P4ID,B4ID                                                        
         MVC   P4CODE,B4CODE                                                    
         MVC   P4CHPER,B4CHPER                                                  
         MVC   P4AGY,B4AGY                                                      
         MVC   P4ACC,B4ACC                                                      
         MVC   P4PROD,B4PROD                                                    
         MVC   P4EST,B4EST                                                      
***      MVC   P4RUNDT,B4RUNDT                                                  
         MVC   P4ESTDT,B4ESTDT                                                  
         MVC   P4EVENT,B4EVENT                                                  
         MVC   P4MULTI,B4MULTI                                                  
*        GOTO1 =V(PRNTBL),DMCB,=C'MULT',B4MULTI,C'DUMP',1,=C'1D'                
*        GOTO1 =V(PRNTBL),DMCB,=C'B4ID',B4MULTI,C'DUMP',50,=C'1D'               
         B     PL80                                                             
*                                                                               
         EJECT                                                                  
PL50     DS    0H                    PRINT F5 RECORDS                           
**       BC    0,PL51                                                           
**       OI    PL50+1,X'F0'                                                     
         CLI   BYTE,5                                                           
         BE    PL51                                                             
         MVI   BYTE,5                                                           
         MVI   FORCEHED,C'Y'                                                    
PL51     LA    R2,P                                                             
         USING P5D,R2                                                           
         MVC   P5ID,B4ID                                                        
         MVC   P5CODE,B4CODE                                                    
         MVC   P5CHPER,B4CHPER                                                  
         MVC   P5AGY,B4AGY                                                      
         MVC   P5ACC,B4ACC                                                      
         MVC   P5PROD,B4PROD                                                    
         MVC   P5EST,B4EST                                                      
         LA    R4,B5MON1                                                        
         LA    R2,P5MON1                                                        
         LA    R3,6                                                             
PL55     EDIT  (P8,0(R4)),(11,0(R2)),2,MINUS=YES                                
         LA    R4,8(R4)                                                         
         LA    R2,12(R2)                                                        
         BCT   R3,PL55                                                          
         B     PL80                                                             
*                                                                               
*                                                                               
PL60     DS    0H                PRINT F6 RECORDS                               
**       BC    0,PL61                                                           
**       OI    PL60+1,X'F0'                                                     
         CLI   BYTE,6                                                           
         BE    PL61                                                             
         MVI   BYTE,6                                                           
         MVI   FORCEHED,C'Y'                                                    
PL61     LA    R2,P                                                             
         USING P6D,R2                                                           
         MVC   P6ID,B4ID                                                        
         MVC   P6CODE,B4CODE                                                    
         MVC   P6CHPER,B4CHPER                                                  
         MVC   P6AGY,B4AGY                                                      
         MVC   P6ACC,B4ACC                                                      
         MVC   P6PROD,B4PROD                                                    
         MVC   P6EST,B4EST                                                      
         MVC   P6BSX,B6BSX                                                      
         MVC   P6DETBC,B6DETBC                                                  
         LA    R4,B6MON1                                                        
         LA    R2,P6MON1                                                        
         LA    R3,6                                                             
PL65     EDIT  (P8,0(R4)),(10,0(R2)),2,MINUS=YES                                
         LA    R4,8(R4)                                                         
         LA    R2,11(R2)                                                        
         BCT   R3,PL65                                                          
         B     PL80                                                             
*                                                                               
*                                                                               
PL80     GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   NODATA,0                                                         
         BNE   PL5                                                              
PLX      B     XIT                                                              
         EJECT                                                                  
*                                 *** DOWNLOAD ***                              
DL5      BAS   RE,HIGHBUFF                                                      
         BO    *+8                                                              
         MVI   NODATA,1                                                         
         B     DL10                                                             
DL7      BAS   RE,SEQBUFF                                                       
         BO    DL100                                                            
DL10     LA    R4,DOWNWORK                                                      
         USING DLCBD,R4                                                         
*        LA    R1,PRNTLN                                                        
         LA    R1,P                                                             
         ST    R1,DLCBAPL                                                       
         LA    R1,DOWNHK                                                        
         ST    R1,DLCBAPR                                                       
*                                                                               
         MVI   DLCBACT,C'S'                                                     
         MVI   DLCBFLD,X'40'                                                    
         MVC   DLCBFLD+1(L'DLCBFLD-1),DLCBFLD                                   
******   GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,VDLFLD                                                        
*                                                                               
         CLI   NODATA,0            IS THERE ANY DATA                            
         BNE   DL20                                                             
         MVC   DLCBFLD(17),=C'NO DATA TO REPORT'                                
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,C'P'                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
         B     DL100                                                            
*                                                                               
DL20     DS    0H                                                               
         CLC   B4ID,=C'F4'                                                      
         BE    DL40                                                             
         CLC   B4ID,=C'F5'                                                      
         BE    DL50                                                             
         CLC   B4ID,=C'F6'                                                      
         BE    DL60                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
DL40     DS    0H                                F4 RECORD                      
         LA    R3,DLCBFLD          DATA FIELD                                   
*****    USING F4D,R3                                                           
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
***      MVC   0(2,R3),B4D                                                      
         MVC   0(2,R3),B4ID                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(1,R3),B4CODE                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(3,R3),B4CHPER                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(2,R3),B4AGY                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(6,R3),B4ACC                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B4PROD                                                   
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B4EST                                                    
**       BAS   RE,VDLFLD                                                        
**       LA    R1,2                FILLER                                       
DL42     MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVI   0(R3),X'40'                                                      
**       BAS   RE,VDLFLD                                                        
**       BCT   R1,DL42                                                          
         MVC   4(2,R3),=X'4040404040404040'                                     
**       BAS   RE,VDLFLD                                                        
**       MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   6(6,R3),B4ESTDT                                                  
**       BAS   RE,VDLFLD                                                        
**       MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   12(6,R3),B4EVENT                                                 
**       BAS   RE,VDLFLD                                                        
***      LA    R1,8                SEND 8 BLANKS                                
DL45     MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
***      MVI   0(R3),X'40'                                                      
         MVC   18(8,R3),=X'4040404040404040'                                    
**       BAS   RE,VDLFLD                                                        
***      BCT   R1,DL45                                                          
*8       MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   26(1,R3),B4MULTI                                                 
         CLI   26(R3),C'Y'          IF MULTI IS C'Y'                            
         BNE   *+8                                                              
         MVI   0(R3),X'40'         DOWNLOAD AS BLANK                            
         BAS   RE,VDLFLD                                                        
**       LA    R1,39                                                            
DL47     MVI   DLCBACT,C'P'        ACTION                                       
*8       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVI   0(R3),X'40'                                                      
*8       BAS   RE,VDLFLD                                                        
**       BCT   R1,DL47                                                          
**       MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVC   0(8,R3),B4ESTDT                                                  
**       BAS   RE,VDLFLD                                                        
**       MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVC   0(1,R3),B4STATUS                                                 
**       BAS   RE,VDLFLD                                                        
         B     DL70                                                             
         EJECT                                                                  
*                                                                               
DL50     DS    0H                                 F5 RECORD                     
         LA    R3,DLCBFLD          DATA FIELD                                   
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVC   0(2,R3),B5D                                                      
         MVC   0(2,R3),B5ID                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(1,R3),B5CODE                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(3,R3),B5CHPER                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(2,R3),B5AGY                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(6,R3),B5ACC                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B5PROD                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B5EST                                                    
         BAS   RE,VDLFLD                                                        
         LA    R1,2                                                             
DL52     MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVI   0(R3),X'40'                                                      
         BAS   RE,VDLFLD                                                        
         BCT   R1,DL52                                                          
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         LA    R1,B5MON1                                                        
         LA    R6,6                                                             
DL55     EDIT  (P8,0(R1)),(10,0(R3)),FILL=0                                     
         TM    7(R1),X'01'                                                      
         BNO   DL57                                                             
         NI    7(R3),X'DF'         OVERPUNCH                                    
         B     DL57                                                             
DL57     MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         BAS   RE,VDLFLD                                                        
         LA    R1,8(R1)                                                         
         BCT   R6,DL55                                                          
         B     DL70                                                             
         EJECT                                                                  
*                                                                               
DL60     DS    0H                                 F6 RECORD                     
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(2,R3),B6ID                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(1,R3),B6CODE                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(3,R3),B6CHPER                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(2,R3),B6AGY                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(6,R3),B6ACC                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B6PROD                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B6EST                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(2,R3),B6BSX                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         MVC   0(4,R3),B6DETBC                                                  
         BAS   RE,VDLFLD                                                        
         LA    R1,B6MON1                                                        
         LA    R6,6                                                             
DL65     EDIT  (P8,0(R1)),(9,0(R3)),FILL=0                                      
         TM    7(R1),X'01'                                                      
         BNO   DL66                                                             
         NI    6(R3),X'DF'         OVERPUNCH                                    
         B     DL66                                                             
DL66     MVI   DLCBACT,C'P'        ACTION                                       
         MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
         BAS   RE,VDLFLD                                                        
         LA    R1,8(R1)                                                         
         BCT   R6,DL65                                                          
**       LA    R1,3                                                             
DL67     MVI   DLCBACT,C'P'        ACTION                                       
**       MVI   DLCBTYP,C'T'        TYPE(TEXT)                                   
**       MVI   0(R3),X'40'                                                      
**       BAS   RE,VDLFLD                                                        
**       BCT   R1,DL67                                                          
         B     DL70                                                             
         EJECT                                                                  
*                                                                               
DL70     MVI   DLCBACT,C'L'        END OF PRINT LINE                            
         BAS   RE,VDLFLD                                                        
         B     DL7                                                              
*                                                                               
DL100    MVI   DLCBACT,C'R'        END OF REPORT                                
         BAS   RE,VDLFLD                                                        
         B     DLX                                                              
*                                                                               
DLX      XIT1                                                                   
*                                                                               
VDLFLD   NTR1                                                                   
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
***      MVI   LINE,2              PXZ TESTING                                  
         B     DLX                                                              
*                                                                               
DOWNHK   NTR1                                                                   
*        GOTO1 =V(PRINT),DMCB,PRNTLN,=C'BL01',RR=RELO                           
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     DLX                                                              
*                                                                               
DOWNWORK DS    CL100                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
XCTBL NTR1                                                                      
         L     RE,=A(TABLE)                                                     
         L     RF,=F'80000'                                                     
         XCEF                                                                   
         B     XIT                                                              
*                                                                               
GETPRD   NTR1                                                                   
         CLI   NBSPLPRN,X'FF'    NETVAUE SET UNALLOC TO FF                      
         BE    GP15                                                             
         L     R2,ANETWS1                                                       
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         CLI   NBSPLPRN,0                                                       
         BE    GP15                                                             
GP10     CLC   3(1,R2),NBSPLPRN                                                 
         BE    GP20                                                             
         LA    R2,4(R2)                                                         
         CLI   3(R2),0                                                          
         BNE   GP10                                                             
**GP15     XC    WORK(3),WORK      PRODUCT IS UNALLOCATED                       
GP15     MVC   WORK(3),=C'UNL'     PRODUCT IS UNALLOCATED                       
         B     GPX                                                              
GP20     MVC   WORK(3),0(R2)                                                    
*                                                                               
GPX      B     XIT                                                              
         DROP  R2                                                               
*                                                                               
GETPRDN  NTR1                                                                   
         L     R2,ANETWS1                                                       
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
GPN10    CLC   0(3,R2),FULL                                                     
         BE    GPN15                                                            
         LA    R2,4(R2)                                                         
         CLI   3(R2),0                                                          
         BNE   GPN10                                                            
         MVI   BYTE,0            PRODUCT IS UNALLOCATED                         
         B     GPNX                                                             
GPN15    MVC   BYTE,3(R2)                                                       
*                                                                               
GPNX     B     XIT                                                              
         DROP  R2                                                               
*                                                                               
DO99     CVB   R1,0(R5)                                                         
         L     R0,=F'9930'         GET 99.3 PERCENT INTO R0                     
         SPACE 1                                                                
DOALL    SLL   R0,1                (X2)                                         
         MR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,0(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
CLTREC   NTR1           READ CLIENT RECORD INTO ANETWS1                         
         XC    KEY,KEY                                                          
         LA    R1,SHRTKEY           SHORTRATE REC                               
         USING PGKEY,R1                                                         
         MVC   KEY+1(3),PGKAM                                                   
         MVC   AIO,ANETWS1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                   GET CLIENT RECORD                       
*                                                                               
         XIT1                                                                   
         SPACE                                                                  
*                                                                               
* - EXPECTS 3 CHAR PROD IN FULL                                                 
PGBRAND  NTR1                                                                   
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PGKEY,R2                                                         
         MVC   KEY(13),SHRTKEY                                                  
         CLC   PGKNETP,FULL        IS IT PRD WE WANT                            
         BNE   PGB20                                                            
         L     R1,ANETWS2                                                       
         A     R1,=F'500'                                                       
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   PGB20                                                            
         USING PGNETELM,R6                                                      
         MVC   BRANDSV(3),PGNETPB     SET PG BRAND                              
         MVC   BRANDSV+3(1),PGNETPB2  SET PG BRAND                              
         MVC   BRANDSFX,PGNETBFX      SET BRAND SUFFIX                          
         OC    BRANDSV,PGNETPB                                                  
         BNZ   PGBX                                                             
PGB20    XC    KEY,KEY             ELSE..READ FOR 0 EST                         
         MVC   KEY,SHRTKEY                                                      
         MVI   PGKNETE,0                                                        
         MVC   PGKNETP,FULL             ..AND PRODUCT                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGNETELM,R6                                                      
         MVC   BRANDSV(3),PGNETPB     SET PG BRAND                              
         MVC   BRANDSV+3(1),PGNETPB2  SET PG BRAND                              
         MVC   BRANDSFX,PGNETBFX      SET BRAND SUFFIX                          
PGBX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5(7),=C'PRODUCT'                                                
         MVC   H5+10(7),SPLPRO                                                  
         MVC   H6(8),=C'ESTIMATE'                                               
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         CLI   NBSELESE,0                                                       
         BE    HK10                                                             
         MVC   H6+10(7),SPLEST                                                  
         MVI   H6+17,C' '                                                       
         MVC   H6+18(24),SPLESTN                                                
         OC    H6+10(32),SPACES                                                 
         GOTO1 SQUASHER,DMCB,H6+10,32                                           
HK10     DS    0H                                                               
         CLC   B4ID,=C'F5'                                                      
         BE    HK50                                                             
         CLC   B4ID,=C'F6'                                                      
         BE    HK60                                                             
         LA    R2,H10                                                           
         USING P4D,R2                                                           
         MVC   P4ID,=C'ID'                                                      
         MVC   P4CODE-1(4),=C'CODE'                                             
         MVC   P4CHPER-1(6),=C'PERIOD'                                          
         MVC   P4AGY(3),=C'AGY'                                                 
         MVC   P4ACC(3),=C'ACC'                                                 
         MVC   P4PROD-1(4),=C'PROD'                                             
         MVC   P4EST+1(3),=C'EST'                                               
         MVC   P4ESTDT,=C'EST DATE'                                             
         MVC   P4EVENT+1(5),=C'EVENT'                                           
         MVC   P4MULTI-2(5),=C'MULTI'                                           
***      MVC   P4RUNDT(8),=C'RUN DATE'                                          
***      MVC   P4STATUS,=C'STATUS'                                              
         DROP  R5                                                               
         BC    0,*+12                                                           
         OI    *-3,X'F0'                                                        
         BAS   RE,INITBOX                                                       
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LA    R5,BOXCOLS                                                       
         USING P4D,R5                                                           
         MVI   P4L,C'L'           BOXES FOR F4                                  
         MVI   P4C1,C'C'                                                        
         MVI   P4C2,C'C'                                                        
         MVI   P4C3,C'C'                                                        
         MVI   P4C4,C'C'                                                        
         MVI   P4C5,C'C'                                                        
         MVI   P4C6,C'C'                                                        
         MVI   P4C7,C'C'                                                        
         MVI   P4C8,C'C'                                                        
         MVI   P4C9,C'C'                                                        
         MVI   P4CA,C'C'                                                        
***      MVI   P4CB,C'C'                                                        
         MVI   P4R,C'R'                                                         
         B     HK70                                                             
         EJECT                                                                  
*                                                                               
         DROP  R5                                                               
HK50     DS    0H                  F5 PRINT LINE/BOXES                          
         BC    0,HK51                                                           
         OI    HK50+1,X'F0'                                                     
         BAS   RE,INITBOX                                                       
HK51     LA    R2,H10                                                           
         USING P5D,R2                                                           
         MVC   P5ID(10),=C'CODE/DATES'                                          
**       MVC   P5ID,=C'ID'                                                      
**       MVC   P5CODE-1(4),=C'CODE'                                             
**       MVC   P5CHPER-1(6),=C'PERIOD'                                          
**       MVC   P5AGY(3),=C'AGY'                                                 
**       MVC   P5ACC(3),=C'ACC'                                                 
         MVC   P5PROD-1(4),=C'PROD'                                             
         MVC   P5EST+1(3),=C'EST'                                               
         LA    R3,MONLIST                                                       
         L     R4,NUMMONS                                                       
         LA    R2,P5MON1                                                        
HK53     GOTO1 DATCON,DMCB,(2,0(R3)),(4,2(R2))                                  
         LA    R2,12(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,HK53                                                          
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LA    R5,BOXCOLS                                                       
         USING P5D,R5                                                           
         MVI   P5L,C'L'                                                         
**       MVI   P5C1,C'C'                                                        
**       MVI   P5C2,C'C'                                                        
**       MVI   P5C3,C'C'                                                        
**       MVI   P5C4,C'C'                                                        
         MVI   P5C5,C'C'                                                        
         MVI   P5C6,C'C'                                                        
         MVI   P5C7,C'C'                                                        
         MVI   P5C8,C'C'                                                        
         MVI   P5C9,C'C'                                                        
         MVI   P5C10,C'C'                                                       
         MVI   P5C11,C'C'                                                       
         MVI   P5C12,C'C'                                                       
         MVI   P5R,C'R'                                                         
         B     HK70                                                             
         EJECT                                                                  
*                                                                               
         DROP  R2,R5                                                            
HK60     DS    0H                    F6 PRINT LINE/BOXES                        
         BC    0,HK61                                                           
         OI    HK60+1,X'F0'                                                     
         BAS   RE,INITBOX                                                       
HK61     LA    R2,H10                                                           
         USING P6D,R2                                                           
**       MVC   P6ID,=C'ID'                                                      
**       MVC   P6CODE-1(4),=C'CODE'                                             
**       MVC   P6CHPER-1(6),=C'PERIOD'                                          
**       MVC   P6AGY(3),=C'AGY'                                                 
         MVC   P6ID(10),=C'CODE/DATES'                                          
***      MVC   P6ACC(3),=C'ACC'                                                 
         MVC   P6PROD-1(4),=C'PROD'                                             
         MVC   P6EST+1(3),=C'EST'                                               
         MVC   P6BSX-2(5),=C' DDS '                                             
         MVC   P6DETBC-1(5),=C' PG  '                                           
         LA    R2,H11                                                           
         MVC   P6BSX-2(5),=C'BRAND'                                             
         MVC   P6DETBC-1(5),=C'BRAND'                                           
         LA    R3,MONLIST                                                       
         L     R4,NUMMONS                                                       
         LA    R2,H10                                                           
         LA    R2,P6MON1                                                        
HK63     GOTO1 DATCON,DMCB,(2,0(R3)),(4,2(R2))                                  
         LA    R2,11(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,HK63                                                          
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LA    R5,BOXCOLS                                                       
         USING P6D,R5                                                           
         MVI   P6L,C'L'           BOXES FOR F6                                  
**       MVI   P6C1,C'C'                                                        
**       MVI   P6C2,C'C'                                                        
**       MVI   P6C3,C'C'                                                        
**       MVI   P6C4,C'C'                                                        
         MVI   P6C5,C'C'                                                        
         MVI   P6C6,C'C'                                                        
         MVI   P6C7,C'C'                                                        
         MVI   P6C8,C'C'                                                        
         MVI   P6C9,C'C'                                                        
         MVI   P6C10,C'C'                                                       
         MVI   P6C11,C'C'                                                       
         MVI   P6C12,C'C'                                                       
         MVI   P6C13,C'C'                                                       
         MVI   P6C14,C'C'                                                       
         MVI   P6R,C'R'                                                         
         B     HK70                                                             
         EJECT                                                                  
HK70     L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         CLC   B4ID,=C'F6'                                                      
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     XIT                                                              
*                                                                               
*                                                                               
INITBOX  NTR1                                                                   
         L     R1,ABOX                    *** BOXES ***                         
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        BUFFALO ROUTINES                                                       
*                                                                               
HIGHBUFF XC    BFREC,BFREC                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
GETBUFF  LA    R1,=C'GET'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 BUFFALO,DMCB,,ABUFF,BFREC,0                                      
         TM    DMCB+8,X'80'                                                     
         B     XIT                                                              
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEADING  SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,56,C'PNG NETWORK REPORT'                                      
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,53,PERIOD                                                     
         SSPEC H3,99,NETREP                                                     
         SSPEC H4,125,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
CTABLE   DS    CL3000                                                           
*                                                                               
TABLE    DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=12,FLAVOR=PACKED,KEYLIST=(100,A)          
*                                                                               
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
DOWNOPT  DS    CL1          DOWNLOAD OPTION                                     
*                                                                               
DTITLE   DS    D                   ***MYD***                                    
RELO     DS    A                                                                
ABUFF    DS    A                                                                
NUMMONS  DS    F                                                                
COVPARM  DS    A                                                                
CLCREG   DS    F                                                                
PRDSUP   DS    CL1                                                              
PGEKEY   DS    CL6                                                              
AGYCODE  DS    CL2                                                              
BRDCODE  DS    CL4                                                              
MONLIST  DS    CL48                ROOM FOR 12 MONTHS                           
PERTYPE  DS    CL4                                                              
BRANDSV  DS    CL4                                                              
BRANDSFX DS    CL2                                                              
PRDSV    DS    CL3                                                              
PRDNSV   DS    CL1                                                              
ESTSV    DS    CL1                                                              
CLTSV    DS    CL2                                                              
OPTASS   DS    CL1                 ASSIGNED OPTION                              
OPT99    DS    CL1                 99.3 OPTION                                  
TODAT    DS    CL6                 RUN DATE                                     
NODATA   DS    CL1                                                              
RQCHARP  DS    CL6                                                              
NETACFLD DS    CL9                                                              
EFCXFR   DS    CL1                                                              
SHRTSTRT DS    CL2                                                              
SHRTKEY  DS    CL13                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
BFREC    DS    0CL196              BUFFALO RECORD LAYOUT                        
BFKEY    DS    CL100                                                            
BFDATA1  DS    CL8                                                              
BFDATA2  DS    CL8                                                              
BFDATA3  DS    CL8                                                              
BFDATA4  DS    CL8                                                              
BFDATA5  DS    CL8                                                              
BFDATA6  DS    CL8                                                              
BFDATA7  DS    CL8                                                              
BFDATA8  DS    CL8                                                              
BFDATA9  DS    CL8                                                              
BFDATAA  DS    CL8                                                              
BFDATAB  DS    CL8                                                              
BFDATAC  DS    CL8                                                              
*                                                                               
         ORG BFREC                                                              
B4D      DS    0CL100             F4 PNG RECORD                                 
B4CODE   DS    CL1                                                              
B4CHPER  DS    CL3                                                              
B4AGY    DS    CL2                                                              
B4ACC    DS    CL6                                                              
B4PROD   DS    CL4                                                              
B4EST    DS    CL4                                                              
B4ID     DS    CL2                                                              
         DS    CL2                                                              
B4ESTDT  DS    CL8                   RUN DATE                                   
B4EVENT  DS    CL6                                                              
         DS    CL8                                                              
B4MULTI  DS    CL1                                                              
B4SPARE  DS    CL39                                                             
         DS    CL14                    SPARE KEY                                
         DS    CL96                    SPARE DATA                               
*                                                                               
         ORG   BFREC                                                            
B5D      DS    0CL196             F5 PNG RECORD                                 
B5CODE   DS    CL1                                                              
B5CHPER  DS    CL3                                                              
B5AGY    DS    CL2                                                              
B5ACC    DS    CL6                                                              
B5PROD   DS    CL4                                                              
B5EST    DS    CL4                                                              
B5ID     DS    CL2                                                              
B5SPR    DS    CL2                 FILLER                                       
*                                                                               
         DS    CL76                KEY SPARE                                    
*                                                                               
B5MON1   DS    CL8                                                              
B5MON2   DS    CL8                                                              
B5MON3   DS    CL8                                                              
B5MON4   DS    CL8                                                              
B5MON5   DS    CL8                                                              
B5MON6   DS    CL8                                                              
         DS    CL48                                                             
B5SPRLE  EQU   *-B5SPR                                                          
B5DOLE   EQU   *-B5MON1                                                         
*                                                                               
         ORG   BFREC                                                            
B6D      DS    0CL196             F6 PNG RECORD                                 
B6CODE   DS    CL1                                                              
B6CHPER  DS    CL3                                                              
B6AGY    DS    CL2                                                              
B6ACC    DS    CL6                                                              
B6PROD   DS    CL4                                                              
B6EST    DS    CL4                                                              
B6ID     DS    CL2                                                              
B6SPR    DS    CL2                                                              
         ORG   B6SPR                                                            
B6BSX    DS    CL2           DDS BRAND SUFFIX                                   
B6DETBC  DS    CL4           PGBRAND                                            
*                                                                               
         DS    CL72                KEY SPARE                                    
B6MON1   DS    CL8                                                              
B6MON2   DS    CL8                                                              
B6MON3   DS    CL8                                                              
B6MON4   DS    CL8                                                              
B6MON5   DS    CL8                                                              
B6MON6   DS    CL8                                                              
         DS    CL48                                                             
B6DOLE   EQU   *-B6MON1                                                         
B6SPRLE  EQU   *-B6BSX                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         DSECT                                                                  
P4D      DS    0CL132            DSECT FOR PRINT LINE                           
         DS    CL20                                                             
P4L      DS    CL1                                                              
P4ID     DS    CL2                                                              
P4C1     DS    CL1                                                              
         DS    CL1                                                              
P4CODE   DS    CL1                                                              
         DS    CL2                                                              
P4C2     DS    CL1                                                              
         DS    CL1                                                              
P4CHPER  DS    CL3                                                              
         DS    CL2                                                              
P4C3     DS    CL1                                                              
P4AGY    DS    CL2                                                              
         DS    CL1                                                              
P4C4     DS    CL1                                                              
P4ACC    DS    CL6                                                              
P4C5     DS    CL1                                                              
         DS    CL1                                                              
P4PROD   DS    CL4   PXZ                                                        
P4C6     DS    CL1                                                              
P4EST    DS    CL4                                                              
P4C7     DS    CL1                                                              
         DS    CL1                                                              
P4ESTDT  DS    CL6                                                              
P4C8     DS    CL1                                                              
P4EVENT  DS    CL6                                                              
P4C9     DS    CL1                                                              
         DS    CL2                                                              
P4MULTI  DS    CL1                                                              
         DS    CL2                                                              
P4CA     DS    CL2                                                              
**P4RUNDT  DS    CL8                                                            
**P4CB     DS    CL1                                                            
**P4STATUS DS    CL6                                                            
P4R      DS    CL1                                                              
P4LENE   EQU   *-P4D                                                            
*                                                                               
         EJECT                                                                  
*                               F5 PRINT LINE DSECT                             
         ORG   P4D                                                              
P5D      DS    0CL132                                                           
         DS    CL1                                                              
P5L      DS    CL1                                                              
P5ID     DS    CL2      *CODE/PERIOD/AGY                                        
P5CODE   DS    CL1      *                                                       
P5CHPER  DS    CL3      *                                                       
P5AGY    DS    CL2      *                                                       
P5ACC    DS    CL6      *                                                       
P5C5     DS    CL1                                                              
P5PROD   DS    CL4    PXZ                                                       
P5C6     DS    CL1                                                              
P5EST    DS    CL4                                                              
P5C7     DS    CL1                                                              
P5MON1   DS    CL11                                                             
P5C8     DS    CL1                                                              
P5MON2   DS    CL11                                                             
P5C9     DS    CL1                                                              
P5MON3   DS    CL11                                                             
P5C10    DS    CL1                                                              
P5MON4   DS    CL11                                                             
P5C11    DS    CL1                                                              
P5MON5   DS    CL11                                                             
P5C12    DS    CL1                                                              
P5MON6   DS    CL11                                                             
**P5C13    DS    CL1                                                            
**P5XTRDAT DS    CL8                     XTRACT DATE                            
**P5C14    DS    CL1                                                            
**P5STATUS DS    CL4                     STATUS BYTE                            
**P5C15    DS    CL1                                                            
**P5CLIPRD DS    CL8                                                            
**P5C16    DS    CL1                                                            
**P5OFFICE DS    CL3                                                            
P5R     DS    CL1                                                               
P5LENE   EQU   *-P5D                                                            
*                                                                               
         EJECT                                                                  
*                              F6 PRINT LINE DSECT                              
         ORG   P4D                                                              
P6D      DS    0CL132                                                           
         DS    CL1                                                              
P6L      DS    CL1           *CODE/PERIOD/AGY                                   
P6ID     DS    CL2          *                                                   
P6CODE   DS    CL1          *                                                   
P6CHPER  DS    CL3          *                                                   
P6AGY    DS    CL2          *                                                   
P6ACC    DS    CL6          *       PXZ                                         
P6C5     DS    CL1                                                              
P6PROD   DS    CL4                 PXZ                                          
P6C6     DS    CL1                                                              
P6EST    DS    CL4                                                              
P6C7     DS    CL1                                                              
         DS    CL2                                                              
P6BSX    DS    CL2           DDS BRAND SUFFIX                                   
         DS    CL1                                                              
P6C8     DS    CL1                                                              
         DS    CL1                                                              
P6DETBC  DS    CL4           PGBRAND                                            
         DS    CL1                                                              
P6C9     DS    CL1                                                              
P6MON1   DS    CL10                                                             
P6C10    DS    CL1                                                              
P6MON2   DS    CL10                                                             
P6C11    DS    CL1                                                              
P6MON3   DS    CL10                                                             
P6C12    DS    CL1                                                              
P6MON4   DS    CL10                                                             
P6C13    DS    CL1                                                              
P6MON5   DS    CL10                                                             
P6C14    DS    CL1                                                              
P6MON6   DS    CL10                                                             
**P6C15    DS    CL1                                                            
**P6SYS    DS    CL3                     SYSTEM PREFIX                          
**P6C16    DS    CL1                                                            
**P6XTRDAT DS    CL8                     EXTRACT DATE                           
**P6C17    DS    CL1                                                            
**P6STATUS DS    CL1                                                            
**P6C18    DS    CL1                                                            
**P6CLIPRD DS    CL8                                                            
**P6C19    DS    CL1                                                            
**P6OFFICE DS    CL3                                                            
P6R      DS    CL1                                                              
P6LENE   EQU   *-P6D                                                            
         EJECT                                                                  
*                                                                               
TABLED   DSECT                                                                  
         DS    0CL1                                                             
TCLT     DS    CL2                                                              
TEST     DS    CL1                                                              
TPRDNM   DS    CL3                 CODE                                         
TPRD     DS    CL1                 BINARY                                       
         DS    CL7                 SPARE                                        
TKLNE    EQU   *-TCLT                                                           
TMON1    DS    PL8                 ACTUAL                                       
         DS    PL8                 ACTUAL   99.3                                
         DS    PL8                 ASSIGNED                                     
         DS    PL8                 ASSIGNED 99.3                                
TMONLENE EQU   *-TMON1                                                          
TMON2    DS    CL32                                                             
TMON3    DS    CL32                                                             
TMON4    DS    CL32                                                             
TMON5    DS    CL32                                                             
TMON6    DS    CL32                                                             
TMON7    DS    CL32                                                             
TMON8    DS    CL32                                                             
TMON9    DS    CL32                                                             
TMONA    DS    CL32                                                             
TMONB    DS    CL32                                                             
TMONC    DS    CL32                                                             
TLENE    EQU   *-TCLT                                                           
*                                                                               
         SPACE 2                                                                
CTABLED  DSECT                   COMMISSION TABLE DSECT                         
CCLT     DS    CL2                                                              
CEST     DS    CL1                                                              
CPRD     DS    CL3                                                              
CRATE    DS    CL4                                                              
CRTYPE   DS    CL1                 GROSS OR NET                                 
CTBLENE  EQU   *-CCLT                                                           
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID1D                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENBILL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEWRI46   06/28/06'                                      
         END                                                                    
