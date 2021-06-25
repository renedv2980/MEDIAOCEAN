*          DATA SET REREP1H10  AT LEVEL 046 AS OF 05/01/02                      
*PHASE RE1H02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1H02 - GENERAL CONTRACT FIXER   '                          
*********************************************************************           
*                                                                   *           
*        REREP1H02 --- GENERAL CONTRACT FIXER                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*  AUG14/96 (BU ) --- ACTUALIZATION CONTRACT/STATION CLOSEOUT       *           
*                     SWEEP:  FIND 'CLOSED BY RE16' STATIONS        *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1H02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1H02,R7,R8,R9,RR=RE                                        
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
*        DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
         DC    AL1(REQFRST),AL3(SWEEPSTA) SWEEP CONTRACTS FOR STAS              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
         XC    PROCCTR,PROCCTR     INITIALIZE CONTRACT COUNTER                  
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         B     MODEEXIT                                                         
MODEEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
*                                                                               
*   SWEEPSTA: SWEEP CONTRACT RECORDS LOOKING FOR CLOSEOUTS                      
*                                                                               
SWEEPSTA NTR1                                                                   
*                                                                               
         LA    R4,IO1                                                           
         USING RCONREC,R4                                                       
         ST    R4,AIOAREA                                                       
*                                                                               
         XC    HDRCOUNT(LCOUNTS),HDRCOUNT                                       
*                                  CLEAR COUNTERS                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'CC'           INSERT CON RECORD TYPE                       
         MVC   KEY+1(2),RCREPFL    INSERT REP CODE                              
         GOTO1 HIGH                START READ                                   
         B     SW020040                                                         
SW020020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT CC KEY                             
SW020040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'KEY FOUND:'                                           
         MVC   P+12(27),KEY                                                     
         MVC   P+45(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
         CLC   KEY(03),KEYSAVE     SAME TYPE/REP?                               
         BNE   SW020400            NO  - FINISHED                               
         GOTO1 GREC                YES - RETRIEVE RECORD                        
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'REC FOUND:'                                           
         MVC   P+12(60),RCONREC                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         ZICM  RF,RINVLEN,2        ACCUMULATE BYTES                             
         L     RE,TOTBYTES                                                      
         AR    RE,RF                                                            
         ST    RE,TOTBYTES         SAVE TOTAL BYTES                             
*                                                                               
         CLI   RINVKSRC,X'00'      HEADER?                                      
         BNE   SW020060            NO                                           
         L     RF,HDRCOUNT         YES - BUMP HEADER COUNT                      
         LA    RF,1(RF)                                                         
         ST    RF,HDRCOUNT                                                      
         B     SW020020            GO BACK FOR NEXT                             
SW020060 EQU   *                                                                
         CLI   RINVKSRC,X'FF'      RATIONALE?                                   
         BNE   SW020080            NO                                           
         L     RF,RATCOUNT         YES - BUMP RATION COUNT                      
         LA    RF,1(RF)                                                         
         ST    RF,RATCOUNT                                                      
         B     SW020020            GO BACK FOR NEXT                             
SW020080 EQU   *                                                                
         CLC   RINVKBK(2),=X'5F01' EARLIER THAN JAN95?                          
         BL    SW020020            YES - GO BACK FOR NEXT                       
         CLC   RINVKBK(2),=X'5F0C' LATER THAN JAN95?                            
         BH    SW020020            YES - GO BACK FOR NEXT                       
*                                                                               
         ZICM  RF,RINVLEN,2        ACCUMULATE BYTES                             
         L     RE,YRSBYTES                                                      
         AR    RE,RF                                                            
         ST    RE,YRSBYTES         SAVE TOTAL BYTES                             
*                                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'INV FOUND:'                                           
*        MVC   P+12(27),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
         LA    RE,MONDATE          NO  - ADD TO TABLE, COUNT IT                 
SW020090 EQU   *                                                                
         OC    0(4,RE),0(RE)       ANYTHING IN DATE FIELD?                      
         BZ    SW020100            NO  - ADD DATE TO TABLE                      
         CLC   0(2,RE),RINVKBK     THIS DATE IN TABLE?                          
         BE    SW020120            YES - ADD TO COUNTER                         
         LA    RE,8(RE)            BUMP TO NEXT BUCKET                          
         B     SW020090            GO BACK FOR NEXT DATE                        
SW020100 EQU   *                                                                
         MVC   0(2,RE),RINVKBK     PUT DATE IN TABLE                            
SW020120 EQU   *                                                                
         L     RF,4(RE)            LOAD COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,4(RE)            PUT IT BACK                                  
         B     SW020020            GO BACK FOR NEXT                             
SW020400 EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'STATION COUNT  :'                                     
         EDIT  STATCNT,(6,P+20)                                                 
         MVC   P+30(10),=C'TOTBYTE2: '                                          
         EDIT  TOTBYTE2,(12,P+42)                                               
         MVC   P+56(10),=C'YRSBYTE2: '                                          
         EDIT  YRSBYTE2,(12,P+68)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'TOTAL INVENTORY           '                           
         GOTO1 REPORT                                                           
         L     RF,TOTBYTE2                                                      
         SR    RE,RE                                                            
         L     R1,STATCNT                                                       
         DR    RE,R1                                                            
         MVC   P+1(16),=C'TOTAL BYTES    :'                                     
         EDIT  (RF),(12,P+20)                                                   
         GOTO1 REPORT                                                           
         L     RF,YRSBYTE2                                                      
         SR    RE,RE                                                            
         L     R1,STATCNT                                                       
         DR    RE,R1                                                            
         MVC   P+1(16),=C'YEARS BYTES    :'                                     
         EDIT  (RF),(12,P+20)                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'END OF JOB'                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TOTALS   NTR1                                                                   
         OC    LASTSTAT,LASTSTAT   FIRST TIME?                                  
         BZ    TOTS0200            YES                                          
         CLC   TOTBYTES,=F'100000' THRESHOLD CUTOFF FOR STATION?                
         BL    TOTS0200            NOT REACHED                                  
         L     RF,STATCNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STATCNT                                                       
*                                                                               
         L     RE,TOTBYTE2         LOAD GRAND TOTAL BYTES                       
         L     RF,TOTBYTES         LOAD THIS STATION BYTES                      
         AR    RE,RF                                                            
         ST    RE,TOTBYTE2         SAVE GRAND TOTAL BYTES                       
*                                                                               
         L     RE,YRSBYTE2         LOAD GRAND TOTAL YRS                         
         L     RF,YRSBYTES         LOAD THIS STATION YRS                        
         AR    RE,RF                                                            
         ST    RE,YRSBYTE2         SAVE GRAND TOTAL YEARS BYTES                 
*                                                                               
         MVC   P+1(26),=C'INVENTORY FOR STATION XXXX'                           
         MVC   P+23(4),LASTSTAT                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'TOTAL BYTES    :'                                     
         EDIT  TOTBYTES,(12,P+20)                                               
         EDIT  TOTBYTE2,(12,P+40)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'YEARS BYTES    :'                                     
         EDIT  YRSBYTES,(12,P+20)                                               
         EDIT  YRSBYTE2,(12,P+40)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'HEADERS        :'                                     
         EDIT  HDRCOUNT,(6,P+20)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'RATIONALE      :'                                     
         EDIT  RATCOUNT,(6,P+20)                                                
         GOTO1 REPORT                                                           
         LA    R6,MONDATE                                                       
TOTS0040 EQU   *                                                                
         OC    0(4,R6),0(R6)       ANY ENTRY?                                   
         BZ    TOTS0200            NO  -                                        
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,(R6)),(5,P+1)                                     
         MVC   FULL,4(R6)          MOVE BUCKET OUT                              
         EDIT  FULL,(6,P+12)                                                    
         PRINT NOGEN                                                            
         GOTO1 REPORT                                                           
         LA    R6,8(R6)            BUMP TO NEXT SET                             
         B     TOTS0040            GO BACK FOR NEXT                             
TOTS0200 EQU   *                                                                
         XC    TOTBYTES,TOTBYTES                                                
         XC    YRSBYTES,YRSBYTES                                                
         XC    HDRCOUNT(LCNTRS),HDRCOUNT                                        
         MVC   LASTSTAT,KEY+12                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
LASTSTAT DS    CL4                                                              
STATCNT  DS    F                                                                
         EJECT                                                                  
*                                                                               
*   COUNTER AREA                                                                
*                                                                               
HDRCOUNT DS    F                                                                
RATCOUNT DS    F                                                                
TOTBYTES DS    F                                                                
YRSBYTES DS    F                                                                
DATCOUNT DS    24F                                                              
LCNTRS   EQU   *-HDRCOUNT                                                       
         ORG   DATCOUNT                                                         
MONDATE  DS    F                                                                
MONCOUNT DS    F                                                                
         ORG                                                                    
LCOUNTS  EQU   *-HDRCOUNT                                                       
TOTBYTE2 DS    F                                                                
YRSBYTE2 DS    F                                                                
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         USING ALTHDR,RF           ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTCON   LA    RF,RCONREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    MODEEXIT            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
*                                                                               
PROCCTR  DS    F                   CONTRACTS READ      CTR                      
CHGDCTR  DS    F                   CONTRACTS PROCESSED CTR                      
BIGRCTR  DS    F                   CONTRACTS MADE LARGER                        
SMLRCTR  DS    F                   CONTRACTS MADE SMALLER                       
SAMESIZE DS    F                   CONTRACTS SAME SIZE                          
COUNT    DS    F                                                                
FIRSTCON DS    CL2                                                              
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
NEW23ELT DS    CL10                                                             
MYP      DS    CL132                                                            
TOTDAYS  DS    F                                                                
CYCLEDAT DS    CL6                                                              
DAYTABLE DS    14F                                                              
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
IO1      DS    2000C                                                            
IO2      DS    2000C                                                            
IO3      DS    2000C                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
*   COMMISSION RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,             
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENCOM                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 2                                                                
RINVRECD DSECT                                                                  
       ++INCLUDE REGENINV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046REREP1H10 05/01/02'                                      
         END                                                                    
