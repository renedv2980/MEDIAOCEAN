*          DATA SET REREP2402J AT LEVEL 073 AS OF 05/01/02                      
*PHASE RE2402J,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP2402 - GENERAL CONTRACT FIXER   '                          
*********************************************************************           
*                                                                   *           
*        REREP2402 --- GENERAL CONTRACT FIXER                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*  FEB25/99 (SKU) --- KEY VS RECORD SWEEP                           *           
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
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R7,R8,R9,RR=RE                                        
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
         DC    AL1(REQFRST),AL3(SWEEPSAL) SWEEP SALESPERSON RECORDS             
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
*   SWEEPSAL: TABLE UP S/P+TEAM FROM S/P RECORDS, THEN SCAN 'AC'                
*        CONTRACT KEYS, COMPARING THE DATA FOR EXISTENCE.                       
*                                                                               
SWEEPSAL NTR1                                                                   
*                                                                               
         LA    R4,IO1                                                           
         USING RDARREC,R4                                                       
         ST    R4,AIOAREA                                                       
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           INSERT 1ST RECORD TYPE                       
         MVC   KEY+2(2),=C'AQ'                                                  
         GOTO1 HIGH                START READ                                   
         B     SW020040                                                         
SW020020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT KEY                                
SW020040 EQU   *                                                                
*&&DO                                                                           
         CLC   KEY(2),LASTRTYP     SAME RECORD TYPE                             
         BE    SW020042            YES                                          
         MVC   LASTRTYP,KEY        NO                                           
         MVC   P+1(15),=C'NOW PROCESSING '                                      
         MVC   P+20(2),LASTRTYP                                                 
         GOTO1 REPORT                                                           
*&&                                                                             
SW020042 EQU   *                                                                
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
         BNE   SW020044                                                         
         XC    PULSECTR,PULSECTR                                                
         MVC   P+40(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+51)                                               
         MVC   P+61(09),=C'CONTRACTS'                                           
         GOTO1 REPORT                                                           
SW020044 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY FOUND:'                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+45(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
         CLC   KEY(4),=X'0C00C1D9' INTO PASSIVES?                               
         BNL   SW020400            NO  - FINISHED                               
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 GREC                YES - RETRIEVE RECORD                        
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'REC FOUND:'                                           
*        MVC   P+12(60),RDARREC                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         TM    RDARCNTL,X'80'      DELETED?                                     
         BZ    SW020020            OKAY - GO BACK FOR NEXT                      
         MVC   P(27),KEY                                                        
*&&DO                                                                           
         MVC   P+1(15),=C'K,X''41''6X''00''C'''                                 
         MVC   P+16(13),KEY+7                                                   
         MVC   P+29(3),=C'''X'''                                                
         GOTO1 HEXOUT,DMCB,KEY+20,P+32,4,=C'N'                                  
         MVC   P+40(1),=C''''                                                   
         GOTO1 HEXOUT,DMCB,KEY+28,P+46,4,=C'N'                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,RDARREP#,P+56,4,=C'N'                                
         MVC   P+70(30),RDARREC                                                 
*&&                                                                             
         GOTO1 REPORT                                                           
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         B     SW020020            GO BACK FOR NEXT                             
SW020400 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'**********'                                           
         MVC   P+11(10),=C'END OF JOB'                                          
         MVC   P+21(10),=C'**********'                                          
         GOTO1 REPORT                                                           
         MVC   P+40(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+51)                                               
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
PULSECTR DS    F                                                                
TOTALCTR DS    F                                                                
LASTRTYP DS    CL2                                                              
         DS    0F                                                               
*                                                                               
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
LASTTEAM DS    CL2                                                              
LASTSALP DS    CL3                                                              
SPCOUNT  DS    F                                                                
         SPACE 1                                                                
*                                                                               
PROCCTR  DS    F                   CONTRACTS READ      CTR                      
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
MYP      DS    CL132                                                            
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
SALSAREA DS    1500CL5                                                          
IO1      DS    2000C                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REGENDAR                                                       
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
**PAN#1  DC    CL21'073REREP2402J05/01/02'                                      
         END                                                                    
