*          DATA SET REREP1H0Y  AT LEVEL 012 AS OF 05/01/02                      
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
* JUN17/98 (BU ) --- INITIAL ENTRY:  CONTRACT COUNTER               *           
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
         XC    CTR19TAB(LCTRS),CTR19TAB  INIT CONTRACT COUNTERS                 
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8D'           RETRIEVE KEYS                                
INIT0010 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
INIT0040 EQU   *                                                                
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
         BNE   INIT0050                                                         
         XC    PULSECTR,PULSECTR                                                
         MVC   P+40(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+51)                                               
         MVC   P+62(09),=C'CONTRACTS'                                           
         GOTO1 REPORT                                                           
INIT0050 EQU   *                                                                
         CLI   KEY,X'8D'           8D KEYS FINISHED?                            
         BNE   INIT0080            YES - FINISHED                               
         CLI   KEY+16,1            FIRST  SUBKEY TYPE?                          
         BNE   INIT0020            NO  - SKIP IT                                
*                                                                               
INIT0055 EQU   *                                                                
         CLC   KEY+1(2),=C'SZ'     ONLY PROCESS SELTEL                          
         BNE   INIT0020            GO BACK FOR NEXT                             
INIT0060 EQU   *                                                                
         CLC   KEY+8(2),=X'BE21'   FLT START DATE: 1994 AND EARLIER             
         BNL   INIT0062            NOT PRE 1/1/95                               
         L     RF,CTR1994          1994 AND EARLIER: COUNT IT                   
         LA    RF,1(RF)                                                         
         ST    RF,CTR1994                                                       
         B     INIT0072            GO BACK FOR NEXT                             
INIT0062 EQU   *                                                                
         CLC   KEY+8(2),=X'C021'   FLT START DATE: 1995                         
         BNL   INIT0064            NOT PRE 1/1/96                               
         L     RF,CTR1995          1995:  COUNT IT                              
         LA    RF,1(RF)                                                         
         ST    RF,CTR1995                                                       
         B     INIT0072            GO BACK FOR NEXT                             
INIT0064 EQU   *                                                                
         CLC   KEY+8(2),=X'C221'   FLT START DATE: 1996                         
         BNL   INIT0066            NOT PRE 1/1/97                               
         L     RF,CTR1996          1996:  COUNT IT                              
         LA    RF,1(RF)                                                         
         ST    RF,CTR1996                                                       
         B     INIT0072            GO BACK FOR NEXT                             
INIT0066 EQU   *                                                                
         CLC   KEY+8(2),=X'C421'   FLT START DATE: 1997                         
         BNL   INIT0068            NOT PRE 1/1/98                               
         L     RF,CTR1997          1997:  COUNT IT                              
         LA    RF,1(RF)                                                         
         ST    RF,CTR1997                                                       
         B     INIT0072            GO BACK FOR NEXT                             
INIT0068 EQU   *                                                                
         CLC   KEY+8(2),=X'C621'   FLT START DATE: 1998                         
         BNL   INIT0070            NOT PRE 1/1/99                               
         L     RF,CTR1998          1998:  COUNT IT                              
         LA    RF,1(RF)                                                         
         ST    RF,CTR1998                                                       
         B     INIT0072            GO BACK FOR NEXT                             
INIT0070 EQU   *                                                                
         L     RF,CTR1999          1999 AND AFTER: COUNT IT                     
         LA    RF,1(RF)                                                         
         ST    RF,CTR1999                                                       
INIT0072 EQU   *                                                                
         L     RF,CTR19TOT         TOTAL CONTRACTS:                             
         LA    RF,1(RF)                                                         
         ST    RF,CTR19TOT                                                      
         B     INIT0020            GO BACK FOR NEXT                             
INIT0080 EQU   *                                                                
         MVC   P+1(26),=C'COUNT: CONTRACTS PRE 1995:'                           
         EDIT  CTR1994,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT: CONTRACTS FOR 1995:'                           
         EDIT  CTR1995,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT: CONTRACTS FOR 1996:'                           
         EDIT  CTR1996,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT: CONTRACTS FOR 1997:'                           
         EDIT  CTR1997,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT: CONTRACTS FOR 1998:'                           
         EDIT  CTR1998,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT: CONTRACTS FOR 1999:'                           
         EDIT  CTR1999,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'COUNT:  TOTAL CONTRACTS  :'                           
         EDIT  CTR19TOT,(8,P+30)                                                
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
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
CTR19TAB EQU   *                                                                
CTR1994  DS    F                   CONTRACTS READ      CTR                      
CTR1995  DS    F                   CONTRACTS READ      CTR                      
CTR1996  DS    F                   CONTRACTS READ      CTR                      
CTR1997  DS    F                   CONTRACTS READ      CTR                      
CTR1998  DS    F                   CONTRACTS READ      CTR                      
CTR1999  DS    F                   CONTRACTS READ      CTR                      
CTR19TOT DS    F                   CONTRACTS READ      CTR                      
LCTRS    EQU   *-CTR19TAB                                                       
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
**PAN#1  DC    CL21'012REREP1H0Y 05/01/02'                                      
         END                                                                    
