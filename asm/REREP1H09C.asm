*          DATA SET REREP1H09C AT LEVEL 063 AS OF 05/01/02                      
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
*        REREP1H02 --- CONTRACT CHECK:  X'8D' +  X'8E' KEYS         *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*  APR04/99 (BU ) --- CHECKER                                       *           
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
         DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
*        DC    AL1(REQFRST),AL3(SWEEPINV) SWEEP INVENTORY RECORDS               
*        DC    AL1(REQFRST),AL3(SWEEPIN2) SWEEP INVENTORY RECORDS               
*                                  FOR COUNTS                                   
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
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'8D'           SET KEY TYPE                                 
         MVC   KEY+1(2),RCREPFL    INSERT REP CODE                              
*                                                                               
*   TEST                                                                        
*        MVC   KEY+3(14),=X'0000000000C67DC6E40564908701'                       
*   TEST END                                                                    
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED ALSO                            
         GOTO1 HIGH                READ FIRST KEY                               
         XC    LASTKEY,LASTKEY     PRIME THE PUMP                               
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
         OI    DMINBTS,X'08'       READ DELETED ALSO                            
         GOTO1 SEQ                 READ NEXT                                    
INIT0040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   READCT,=F'256'                                                   
*        BH    INIT0900                                                         
*        MVC   P+1(12),=C'8D KEY READ:'                                         
*        MVC   P+20(27),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   KEY,X'8D'           SAME REC TYPE?                               
         BE    INIT0050            YES - PROCEED                                
         CLI   KEY,X'8E'           SAME REC TYPE?                               
         BNE   INIT0900            NO  - FINISHED                               
INIT0050 EQU   *                                                                
         CLC   KEY+1(2),RCREPFL    SAME REP?                                    
         BNE   INIT0900            NO  - FINISHED                               
         L     RF,READCT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,READCT                                                        
         L     RF,PULSECT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PULSECT                                                       
         CLC   PULSECT,=F'1000'    KEY COUNTER                                  
         BNE   INIT0060                                                         
         XC    PULSECT,PULSECT                                                  
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  READCT,(10,P+16)                                                 
         GOTO1 REPORT                                                           
INIT0060 EQU   *                                                                
         OC    LASTKEY,LASTKEY     FIRST TIME?                                  
         BZ    INIT0100                                                         
         CLC   KEY(16),LASTKEY     SAME CONTRACT?                               
         BNE   INIT0100            NO  - JUST RESET BASE                        
         ZIC   RF,LASTKEY+16       YES - LAST KEY TYPE                          
         LA    RF,1(RF)                                                         
         STC   RF,COMPKEY                                                       
         CLC   COMPKEY,KEY+16      NEW KEY IN SEQUENCE?                         
         BNE   INIT0080            NO  - ERROR MESSAGE                          
         MVC   LASTKEY,KEY         YES - SAVE KEY                               
         B     INIT0020            GO BACK FOR NEXT KEY                         
INIT0080 EQU   *                                                                
         MVC   P+1(16),=C'KEY ERROR: LAST:'                                     
         MVC   P+20(28),LASTKEY                                                 
         MVC   P+52(05),=C'THIS:'                                               
         MVC   P+60(28),KEY                                                     
         GOTO1 REPORT                                                           
         B     INIT0020            DON'T SAVE: GO BACK FOR NEXT                 
INIT0100 EQU   *                                                                
         MVC   LASTKEY,KEY         SAVE NEW KEY                                 
         CLI   KEY+16,1            NEW KEY:  TYPE 1?                            
         BE    INIT0020            YES - GO BACK FOR NEXT                       
         MVC   P+1(19),=C'1ST KEY NOT TYPE 1:'                                  
         MVC   P+24(28),KEY                                                     
         GOTO1 REPORT                                                           
         B     INIT0020            GO BACK FOR NEXT                             
**>>>>                                                                          
INIT0900 EQU   *                                                                
         MVC   P+1(16),=C'READ 8D/8E KEYS:'                                     
         EDIT  READCT,(10,P+20)                                                 
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
MODEEXIT EQU   *                                                                
         XIT1                                                                   
LASTKEY  DS    CL27                                                             
COMPKEY  DS    XL1                                                              
READCT   DS    F                                                                
MISSCT   DS    F                                                                
PULSECT  DS    F                                                                
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
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
**PAN#1  DC    CL21'063REREP1H09C05/01/02'                                      
         END                                                                    
