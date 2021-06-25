*          DATA SET REREP1H08  AT LEVEL 009 AS OF 05/01/02                      
*PHASE RE1H02A,*                                                                
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
*                                                                   *           
* JUN24/96 (BU ) --- INITIAL ENTRY:  ACTUALIZATION FIX              *           
*                    RESET PRIOR PERIOD EGAC ORDERS (ACTUALS)       *           
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
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
         DC    AL1(REQLAST),AL3(LAST)                                           
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
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
*                                                                               
*                                                                               
POST     NTR1                                                                   
*                                                                               
         LA    RF,CON#TABL                                                      
POST0001 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RCREPFL,0(RF)       REP IN TABLE?                                
         BE    POST0003            YES                                          
         LA    RF,12(RF)           NO  - BUMP TO NEXT ENTRY                     
         B     POST0001                                                         
POST0003 EQU   *                                                                
         CLC   RCONKCON,4(RF)      CONTRACT NUMBER START?                       
         BL    MODEEXIT            LOWER - DON'T PROCESS                        
*                                                                               
         CLC   RCONKCON,8(RF)      CONTRACT NUMBER END?                         
         BH    MODEEXIT            HIGHER - DON'T PROCESS                       
*                                                                               
                                                                                
         CLC   =C'EGAC',RCONKAGY   IS IT 'ACTUALS' ORDER?                       
         BNE   DUMPITOU            NO  - DON'T DO THIS ORDER                    
         CLC   =C'EGAC',RCONKADV   IS IT 'ACTUALS' ORDER?                       
         BNE   DUMPITOU            NO  - DON'T DO THIS ORDER                    
         CLC   CONCTR,=F'10'                                                    
         BH    POST0005                                                         
         MVC   P+1(10),=C'PRE RECORD'                                           
         GOTO1 REPORT                                                           
         ZICM  RF,RCONLEN,2        PRINT 'PRE   ' RECORD                        
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
POST0005 EQU   *                                                                
         MVI   HELOFLAG,C'N'       SET 'NO DELETES NEEDED'                      
         LA    R2,RCONELEM         FIND X'04' ELEMENTS                          
POST0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    POST0020            YES - NO X'04' ELEMENT                       
         CLI   0(R2),X'03'         EXTRA SAFEGUARD                              
         BNE   POST0015                                                         
         DC    H'0'                                                             
POST0015 EQU   *                                                                
         CLI   0(R2),X'04'                                                      
         BNE   POST0010            NOT X'06' ELEMENT                            
         MVI   0(R2),X'FF'         NO  - SET TO DELETE ELT                      
         MVI   HELOFLAG,C'Y'       SET TO REWRITE RECORD                        
         B     POST0010            GO BACK FOR NEXT                             
*                                                                               
POST0020 EQU   *                                                                
         CLI   HELOFLAG,C'Y'       ANYTHING TO DELETE?                          
         BNE   MODEEXIT                                                         
         CLC   CONCTR,=F'10'                                                    
         BH    POST0025                                                         
         MVC   P+1(13),=C'BEFORE RECORD'                                        
         GOTO1 REPORT                                                           
         ZICM  RF,RCONLEN,2        PRINT 'BEFORE' RECORD                        
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
POST0025 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
**                                 DELETE OLD X'04' ELEMENTS                    
         CLC   CONCTR,=F'10'                                                    
         BH    POST0045                                                         
         MVC   P+1(13),=C'AFTER  RECORD'                                        
         GOTO1 REPORT                                                           
         ZICM  RF,RCONLEN,2        PRINT 'AFTER ' RECORD                        
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
POST0045 EQU   *                                                                
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
*                                                                               
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   MODEEXIT            NO                                           
         BAS   RE,PUTCON           REWRITE RECORD                               
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
DUMPITOU EQU   *                                                                
         DC    H'0'                                                             
         DS    0F                                                               
CON#TABL DC    C'BF  ',X'03121943',X'03130428'                                  
         DC    C'CR  ',X'03126399',X'03135153'                                  
         DC    C'EA  ',X'03119748',X'03126843'                                  
         DC    C'KF  ',X'03102853',X'03103724'                                  
         DC    C'KU  ',X'03138598',X'03150312'                                  
         DC    X'0000'                                                          
*                                                                               
LAST     NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(16),=C'CONTRACTS FOUND:'                                     
         EDIT  CONCTR,(6,P+20)                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
HELOFLAG DS    CL1                                                              
CONCTR   DC    F'0'                                                             
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
FILLER   DS    6000C                                                            
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP1H08 05/01/02'                                      
         END                                                                    
