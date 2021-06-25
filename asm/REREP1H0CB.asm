*          DATA SET REREP1H0CB AT LEVEL 036 AS OF 05/01/02                      
*          DATA SET REREP1H0BA AT LEVEL 019 AS OF 08/09/99                      
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
* JUN24/96 (BU ) --- INITIAL ENTRY:  CONTRACT SWEEP FOR COVERSHEET  *           
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
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)                                          
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
         MVC   P+1(11),=C'SWEEP START'                                          
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0C'           INSERT KEY TYPE                              
         GOTO1 HIGH                                                             
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
INIT0040 EQU   *                                                                
**       MVC   P+1(11),=C'KEY RETURN:'                                          
**       MVC   P+15(27),KEY                                                     
**       GOTO1 REPORT                                                           
*                                                                               
         CLI   KEY,X'0C'           0C KEYS FINISHED?                            
         BNE   INIT0200            YES - FINISHED                               
*        L     RF,PULSECTR                                                      
*        LA    RF,1(RF)                                                         
*        ST    RF,PULSECTR         REPLACE COUNTER                              
*        L     RF,TOTALCTR                                                      
*        LA    RF,1(RF)                                                         
*        ST    RF,TOTALCTR         REPLACE COUNTER                              
*        CLC   PULSECTR,=F'5000'                                                
*        BNE   INIT0050                                                         
*        XC    PULSECTR,PULSECTR                                                
*        MVC   P+51(10),=C'PROCESSED:'                                          
*        EDIT  TOTALCTR,(10,P+62)                                               
*        MVC   P+74(09),=C'CONTRACTS'                                           
*        MVC   P+86(27),KEY                                                     
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
INIT0050 EQU   *                                                                
         BAS   RE,GETCON           RETRIEVE CONTRACT RECORD                     
         LA    R6,RCONELEM                                                      
INIT0060 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD                                
         BE    INIT0020            YES                                          
         CLI   0(R6),X'A6'         COVERSHEET ELEMENT?                          
         BE    INIT0080            YES - CHECK FOR COVERSHEET                   
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     INIT0060            GO BACK FOR NEXT ELEMENT                     
INIT0080 EQU   *                                                                
         MVC   SAVEKEY,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'49'                                                        
         MVC   KEY+16(2),SAVEKEY+2     INSERT REP CODE                          
         MVC   KEY+22(4),SAVEKEY+23    INSERT CONTRACT NUMBER                   
         MVI   KEY+18,X'FF'        INSERT IDENTIFIER                            
*                                                                               
*   TEST                                                                        
***      MVC   P+1(14),=C'LOOK FOR CVST:'                                       
***      MVC   P+16(27),KEY                                                     
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BE    INIT0100            YES - RESTART CONTRACTS                      
         MVC   P+1(21),=C'COVERSHEET NOT FOUND:'                                
         MVC   P+24(2),KEYSAVE+16                                               
         GOTO1 HEXOUT,DMCB,KEYSAVE+22,P+28,4,=C'TOG'                            
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
INIT0100 EQU   *                                                                
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                REREAD CONTRACT KEY TO RESTART               
         B     INIT0020            GO BACK FOR NEXT CONTRACT RECORD             
INIT0200 EQU   *                                                                
         B     MODEEXIT            YES - JOB FINISHED                           
SAVEKEY  DS    CL27                                                             
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
POST     NTR1                                                                   
*                                                                               
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
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
CTR1998  DS    F                   CONTRACTS READ      CTR                      
CTR1999  DS    F                   CONTRACTS READ      CTR                      
PULSECTR DS    F                   CONTRACTS PROCESSED CTR                      
TOTALCTR DS    F                                                                
SMLRCTR  DS    F                                                                
SAMESIZE DS    F                                                                
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
**PAN#1  DC    CL21'036REREP1H0CB05/01/02'                                      
         END                                                                    
