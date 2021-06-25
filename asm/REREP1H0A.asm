*          DATA SET REREP1H0A  AT LEVEL 057 AS OF 05/01/02                      
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
*  SEP04/96 (BU ) --- S/P SWEEP                                     *           
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
         LA    R5,SALSAREA         SET A(SALESPERSON AREA)                      
         LA    R4,IO1                                                           
         USING RSALREC,R4                                                       
         ST    R4,AIOAREA                                                       
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           INSERT S/P RECORD TYPE                       
         MVC   KEY+22(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                START READ                                   
         B     SW020040                                                         
SW020020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT 06 KEY                             
SW020040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY FOUND:'                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+45(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
         CLC   KEY(24),KEYSAVE     SAME TYPE/REP?                               
         BNE   SW020050            NO  - FINISHED                               
         GOTO1 GREC                YES - RETRIEVE RECORD                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'REC FOUND:'                                           
*        MVC   P+12(60),RSALREC                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   0(2,R5),RSALTEAM    INSERT TEAM INTO TABLE                       
         MVC   2(1,R5),RSALKSAL+2  INSERT LAST CHAR S/P INTO TABLE              
         MVC   3(2,R5),RSALKSAL    INSERT 1ST 2 CHARS S/P INTO TABLE            
         XC    5(5,R5),5(R5)       CLEAR NEXT ENTRY                             
         LA    R5,5(R5)            BUMP TO NEXT ENTRY                           
         B     SW020020            GO BACK FOR NEXT                             
SW020050 EQU   *                                                                
         CLC   =C'PRINTABL',QUESTOR REQUEST TO PRINT TABLE?                     
         BNE   SW020080            NO  - DON'T DISPLAY IT                       
         LA    R5,SALSAREA         REDISPLAY TABLE AREA                         
SW020060 EQU   *                                                                
         OC    0(5,R5),0(R5)       ANY ENTRY?                                   
         BZ    SW020080            NO  - FINISHED REDISPLAY                     
         MVC   P+10(5),0(R5)       YES - DISPLAY IT                             
         L     RF,SPCOUNT          SET COUNT FOR DISPLAY                        
         LA    RF,1(RF)                                                         
         ST    RF,SPCOUNT                                                       
         EDIT  SPCOUNT,(6,P+1)                                                  
         GOTO1 REPORT                                                           
         LA    R5,5(R5)            BUMP TO NEXT ENTRY                           
         B     SW020060            GO BACK FOR NEXT                             
SW020080 EQU   *                                                                
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'AC'           INSERT PP3 RECORD TYPE                       
         MVC   KEY+01(2),RCREPFL   INSERT REP CODE                              
SW020100 EQU   *                                                                
         GOTO1 HIGH                START READ                                   
         B     SW020140                                                         
SW020120 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT AC KEY                             
SW020140 EQU   *                                                                
         CLC   KEY(03),KEYSAVE     SAME TYPE/REP?                               
         BNE   SW020400            NO  - FINISHED                               
         CLC   LASTTEAM(5),KEY+5   TEAM/S-P ALREADY SEEN?                       
         BNE   SW020150            NO  - PROCESS IT                             
         ZICM  RF,KEY+7,3          USE S/P CODE AS 'NUMBER'                     
         LA    RF,1(RF)            BUMP UP SALESPERSON CODE                     
         STCM  RF,7,KEY+7          REINSERT KEY                                 
         B     SW020100            READ HIGH ON NEXT KEY                        
SW020150 EQU   *                                                                
         MVC   LASTTEAM(5),KEY+5   NO  - SAVE LAST ONE                          
         LA    R5,SALSAREA         TEAM/S-P IN TABLE?                           
SW020160 EQU   *                                                                
         OC    0(5,R5),0(R5)       END OF TABLE?                                
         BZ    SW020200            YES                                          
         CLC   2(3,R5),KEY+7       S/P CODE IN TABLE?                           
         BNE   SW020180            NO  - CHECK NEXT ENTRY                       
         CLC   0(2,R5),KEY+5       YES - S/P IN SAME TEAM?                      
         BE    SW020120            YES - GO BACK FOR NEXT 'AC' KEY              
         MVC   P+1(16),=C'S/P NOT IN TEAM:'                                     
         B     SW020240                                                         
SW020180 EQU   *                                                                
         LA    R5,5(R5)            NO  - BUMP TO NEXT ENTRY                     
         B     SW020160            CHECK NEXT TABLE ENTRY                       
SW020200 EQU   *                                                                
         MVC   P+1(16),=C'S/P NOT ON FILE:'                                     
SW020240 EQU   *                                                                
         MVC   P+22(5),KEY+5                                                    
         MVC   P+30(07),=C'OFFICE='                                             
         MVC   P+38(2),KEY+3                                                    
         GOTO1 HEXOUT,DMCB,KEY+23,P+45,04,=C'TOG'                               
         MVC   P+55(2),KEY+1                                                    
         GOTO1 REPORT                                                           
         B     SW020120            GO BACK FOR NEXT 'AC' KEY                    
SW020400 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'**********'                                           
         MVC   P+11(10),=C'END OF JOB'                                          
         MVC   P+21(10),=C'**********'                                          
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
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
**PAN#1  DC    CL21'057REREP1H0A 05/01/02'                                      
         END                                                                    
