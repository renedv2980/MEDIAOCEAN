*          DATA SET REREP1H09A AT LEVEL 053 AS OF 05/01/02                      
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
*  FEB21/97 (BU ) --- AGENCY/ADVERTISER SPECIAL CHARACTER SWEEP     *           
*                                                                   *           
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
         DC    AL1(REQFRST),AL3(SPECCHAR) SWEEP ADV/AGY RECORDS                 
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
*   SWEEPINV: SWEEP INVENTORY RECORDS, DEVELOP COUNTS                           
*                                                                               
SPECCHAR NTR1                                                                   
*                                                                               
         LA    R4,IO1                                                           
         USING RADVREC,R4                                                       
         ST    R4,AIOAREA                                                       
*                                                                               
         XC    AGYCOUNT(16),AGYCOUNT                                            
*                                  CLEAR COUNTERS                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           INSERT ADV RECORD TYPE                       
         GOTO1 HIGH                START READ                                   
         B     SPCH0040                                                         
SPCH0020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT ADV KEY                            
SPCH0040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY FOUND:'                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+45(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
         CLI   KEY,X'08'           SAME TYPE/REP?                               
         BNE   SPCH0400            NO  - FINISHED WITH ADVERTISER               
         CLC   KEY+25(2),RCREPFL   SAME REP?                                    
         BNE   SPCH0020            NO  - ADVERT LOW IN KEY: GO BACK             
         GOTO1 SCANCODE,DMCB,KEY+21,0                                           
         B     SPCH0020            GO BACK FOR NEXT ADV KEY                     
SPCH0400 EQU   *                                                                
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           INSERT AGY RECORD TYPE                       
         GOTO1 HIGH                START READ                                   
         B     SPCH0440                                                         
SPCH0420 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT AGY KEY                            
SPCH0440 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY FOUND:'                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+45(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
         CLI   KEY,X'0A'           SAME TYPE?                                   
         BNE   SPCH0800            NO  - FINISHED WITH AGENCY                   
         CLC   KEY+25(2),RCREPFL   SAME REP?                                    
         BNE   SPCH0420            NO  - AGENCY LOW IN KEY: GO BACK             
         GOTO1 SCANCODE,DMCB,KEY+19,1                                           
         B     SPCH0420            GO BACK FOR NEXT ADV KEY                     
         EJECT                                                                  
*                                                                               
SCANCODE NTR1                                                                   
         L     R2,0(R1)            SAVE A(KEY FIELD)                            
         L     R3,4(R1)            SET FLAG FOR COUNTER                         
*                                  COUNT INPUT CODES                            
         LTR   R3,R3               ADVERTISER OR AGENCY?                        
         BNZ   SCOD0010            AGENCY                                       
         L     RF,ADVCOUNT         INCREMENT ADVERT COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,ADVCOUNT                                                      
         B     SCOD0015                                                         
SCOD0010 EQU   *                                                                
         L     RF,AGYCOUNT         INCREMENT AGENCY COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,AGYCOUNT                                                      
SCOD0015 EQU   *                                                                
         LA    RF,4                SET LOOP CONTROL                             
         LR    R4,R2               SET A(FIRST CHAR TO SCAN)                    
SCOD0020 EQU   *                                                                
         CLI   0(R4),C' '          CHARACTER = SPACE?                           
         BE    SCOD0120            YES - ACCEPTED                               
         CLI   0(R4),C'A'                                                       
         BL    SCOD0400            LESS THAN A:  ERROR                          
         CLI   0(R4),C'I'                                                       
         BNH   SCOD0120            BETWEEN A/I:  ACCEPTED                       
         CLI   0(R4),C'J'                                                       
         BL    SCOD0400            LESS THAN J:  ERROR                          
         CLI   0(R4),C'R'                                                       
         BNH   SCOD0120            BETWEEN J/R:  ACCEPTED                       
         CLI   0(R4),C'S'                                                       
         BL    SCOD0400            LESS THAN S:  ERROR                          
         CLI   0(R4),C'Z'                                                       
         BNH   SCOD0120            BETWEEN S/Z:  ACCEPTED                       
         CLI   0(R4),C'0'                                                       
         BL    SCOD0400            LESS THAN S:  ERROR                          
         CLI   0(R4),C'9'                                                       
         BNH   SCOD0120            BETWEEN S/Z:  ACCEPTED                       
         B     SCOD0400            GREATER THAN C'9': ERROR                     
SCOD0120 EQU   *                                                                
         LA    R4,1(R4)            BUMP TO NEXT CHARACTER                       
         BCT   RF,SCOD0020         GO BACK FOR NEXT                             
         B     SCOD0480            OKAY - FULLY ACCEPTED                        
SCOD0400 EQU   *                                                                
         LTR   R3,R3               ADVERTISER OR AGENCY?                        
         BNZ   SCOD0410            AGENCY                                       
         L     RF,ADVSPECS         INCREMENT ADVERT SPECS COUNT                 
         LA    RF,1(RF)                                                         
         ST    RF,ADVSPECS                                                      
         B     SCOD0415                                                         
SCOD0410 EQU   *                                                                
         L     RF,AGYSPECS         INCREMENT AGENCY SPECS COUNT                 
         LA    RF,1(RF)                                                         
         ST    RF,AGYSPECS                                                      
SCOD0415 EQU   *                                                                
         LTR   R3,R3               ADVERTISER OR AGENCY?                        
         BNZ   SCOD0440            AGENCY                                       
         MVC   P+1(12),=C'ADVERTISER: '                                         
         B     SCOD0460                                                         
SCOD0440 EQU   *                                                                
         MVC   P+1(12),=C'AGENCY    : '                                         
SCOD0460 EQU   *                                                                
         MVC   P+20(4),0(R2)       INSERT CODE FOUND                            
         GOTO1 REPORT                                                           
SCOD0480 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SPCH0800 EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'TOTAL ADVERTS  :'                                     
         EDIT  ADVCOUNT,(6,P+20)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'  W/SPEC CHARS :'                                     
         EDIT  ADVSPECS,(6,P+20)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'TOTAL AGENCYIES:'                                     
         EDIT  AGYCOUNT,(6,P+20)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'  W/SPEC CHARS :'                                     
         EDIT  AGYSPECS,(6,P+20)                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'END OF JOB'                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   COUNTER AREA                                                                
*                                                                               
AGYCOUNT DS    F                                                                
AGYSPECS DS    F                                                                
ADVCOUNT DS    F                                                                
ADVSPECS DS    F                                                                
LCNTRS   EQU   *-AGYCOUNT                                                       
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
**PAN#1  DC    CL21'053REREP1H09A05/01/02'                                      
         END                                                                    
