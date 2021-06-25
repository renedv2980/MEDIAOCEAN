*          DATA SET REREP1H0BD AT LEVEL 011 AS OF 05/01/02                      
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
* SEP23/99 (BU ) --- INITIAL ENTRY:  CONTRACT COUNTER: ALL          *           
*                    CLEAR CHANNEL RADIO STATIONS, ACTIVE ACROSS    *           
*                    JAN1/00                                        *           
*                                                                   *           
* NOV30/99 (BU ) --- FILTER ON UNWIRED ORDERS ONLY                  *           
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
         XC    LASTSTAT,LASTSTAT                                                
         XC    CTR1998,CTR1998     INITIALIZE CONTRACT COUNTER                  
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8E'           RETRIEVE KEYS                                
*                                  BREAK ON STATION                             
         MVC   KEY+1(2),=C'CN'     SET TO CLEAR CHANNEL                         
INIT0010 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
INIT0040 EQU   *                                                                
         CLI   KEY,X'8E'           8E KEYS FINISHED?                            
         BNE   INIT0080            YES - FINISHED                               
         CLC   KEY+1(2),=C'CN'     SAME REP?                                    
         BNE   INIT0080            NO  - FINISHED                               
         CLI   KEY+16,2            FIRST  SUBKEY TYPE?                          
         BNE   INIT0020            NO  - SKIP ALL BUT TYPE 2'S                  
         CLI   KEY+20,C'N'         UNWIRED?                                     
         BE    INIT0042            YES - COUNT                                  
         CLI   KEY+20,C'X'         UNWIRED?                                     
         BNE   INIT0020            NO  - SKIP                                   
INIT0042 EQU   *                                                                
         OC    LASTSTAT,LASTSTAT   FIRST TIME?                                  
         BZ    INIT0045            YES                                          
         CLC   LASTSTAT,KEY+3      NO  - SAME STATION?                          
         BE    INIT0045            YES - CONTINUE                               
         BAS   RE,STATDISP         NO  - DISPLAY STATION                        
INIT0045 EQU   *                                                                
         MVC   LASTSTAT,KEY+3      SAVE LAST STATION                            
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
*                                                                               
         B     INIT0050            SKIP PULSECTR BREAK                          
*                                                                               
         BNE   INIT0050                                                         
         XC    PULSECTR,PULSECTR                                                
         MVC   P+01(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+12)                                               
         MVC   P+24(09),=C'CONTRACTS'                                           
         MVC   P+36(27),KEY                                                     
         GOTO1 REPORT                                                           
INIT0050 EQU   *                                                                
         CLC   KEY+10(2),=X'C821'  FLIGHT END   DATE                            
         BL    INIT0053            BEFORE 1/1/00 - SKIP IT                      
         CLC   =C'TEST',KEY+23     BYPASS ALL 'TEST' CONS                       
         BNE   INIT0055            EQU                                          
*                                                                               
INIT0053 EQU   *                                                                
         MVI   KEY+16,X'10'        SET RECORD ID TO SKIP THIS SET               
         B     INIT0010                                                         
INIT0055 EQU   *                                                                
*        CLC   KEY+8(2),=X'C621'   FLIGHT START DATE: 1999                      
*        BL    INIT0060            BEFORE 1/1/99 - ADD TO 1998 CTR              
*        L     RF,CTR1999          COUNT IT                                     
*        LA    RF,1(RF)                                                         
*        ST    RF,CTR1999                                                       
*        B     INIT0070            GO BACK FOR NEXT                             
INIT0060 EQU   *                                                                
         L     RF,CTR1998          COUNT IT                                     
         LA    RF,1(RF)                                                         
         ST    RF,CTR1998                                                       
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         B     INIT0070            GO BACK FOR NEXT                             
INIT0070 EQU   *                                                                
**       GOTO1 DATCON,DMCB,(2,KEY+8),(0,P+1)                                    
**       GOTO1 DATCON,DMCB,(2,KEY+10),(0,P+9)                                   
**       GOTO1 HEXOUT,DMCB,KEY+12,P+18,4,=C'TOG'                                
**       GOTO1 REPORT                                                           
         B     INIT0020                                                         
INIT0080 EQU   *                                                                
         BAS   RE,STATDISP                                                      
         GOTO1 REPORT                                                           
         MVC   P+1(05),=C'TOTAL'                                                
         EDIT  TOTALCTR,(8,P+10)                                                
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
*                                                                               
STATDISP NTR1                                                                   
         MVC   P+1(05),LASTSTAT                                                 
         EDIT  CTR1998,(8,P+10)                                                 
         XC    CTR1998,CTR1998                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
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
LASTSTAT DS    CL5                                                              
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
**PAN#1  DC    CL21'011REREP1H0BD05/01/02'                                      
         END                                                                    
