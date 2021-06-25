*          DATA SET REREP1X02  AT LEVEL 113 AS OF 05/01/02                      
*PHASE RE1X02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1X02 - OFFICE COMMENT ANALYZER  '                          
*********************************************************************           
*                                                                   *           
*        REREP1X02 --- OFFICE COMMENT ANALYZER                      *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* DEC14/95 (BG ) 112 CHANGE REGENALL TO REGENALL1 2K CON            *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1X02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1X02,R7,R8,R9,RR=RE                                        
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
         DC    AL1(REQFRST),AL3(INITIAL)  ENTIRE JOB RUN FROM                   
*                                     REQFRST                                   
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
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
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK AREA)                   
         USING TSARD,R1                                                         
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,12               SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,16               SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         BAS   RE,POSB             PROCESS ALL CONTRACTS                        
         B     MODEEXIT                                                         
*                                                                               
       ++INCLUDE REREPTSAR                                                      
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS LOOKING FOR OFFICE COMMENT INDICATOR.                        
*                                                                               
POSB     NTR1                                                                   
         MVC   COMPANY,=C'DI'      SET TO DO DI                                 
POSB0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           SET UP KEY                                   
         MVC   KEY+2(2),COMPANY                                                 
         GOTO1 HIGH                                                             
         B     POSB0120                                                         
POSB0080 EQU   *                                                                
         GOTO1 SEQ                                                              
POSB0120 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME REC TYPE/USER?                          
         BE    POSB0160            YES                                          
         CLC   COMPANY,=C'HN'      HN DONE?                                     
         BE    POSB0240            YES - PRODUCE REPORT                         
         MVC   COMPANY,=C'HN'      NO  - DO IT                                  
         B     POSB0040            GO BACK AND DO IT                            
*                                                                               
POSB0160 EQU   *                                                                
         GOTO1 GETCON                                                           
         L     RF,PROCCTR          INCREMENT CONTRACT COUNT                     
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
         LA    R2,RCONELEM         FIND X'02' ELEMENT                           
POSB0200 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    POSB0080            YES - NO X'02' ELEMENT                       
*                                     FOR REQUESTED PERIOD                      
         CLI   0(R2),X'02'                                                      
         BNE   POSB0200            NOT X'02' ELEMENT                            
         CLC   2(3,R2),=C'SC='     OFFICE COMMENT USED?                         
         BNE   POSB0200            NO  - LOOK FOR NEXT ELEMENT                  
         MVC   BUFFREC(2),RCONKOFF INSERT OFFICE INTO KEY                       
         MVC   BUFFREC+2(2),5(R2)  INSERT COMMENT NUMBER INTO KEY               
         MVC   BUFFREC+4(2),RCONKREP                                            
*                                  INSERT REP CODE INTO KEY                     
         MVC   BUFFREC+6(6),RCONDATE                                            
*                                  INSERT DATES INTO KEY                        
         MVC   BUFFREC+12(4),RCONKCON INSERT CON # INTO RECORD                  
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         LA    RF,BUFFREC                                                       
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSAADD     SET ACTION TO 'ADD'                          
         GOTO1 ATSAROFF            ADD RECORD TO BUFFER                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         L     RF,TSARCTR          INCREMENT TABLED   COUNT                     
         LA    RF,1(RF)                                                         
         ST    RF,TSARCTR                                                       
         B     POSB0080            GO BACK FOR NEXT CONTRACT                    
POSB0240 EQU   *                                                                
         MVC   P+1(17),=C'CONTRACTS READ:  '                                    
         EDIT  PROCCTR,(10,P+22)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(19),=C'CONTRACTS TABLED:  '                                  
         EDIT  TSARCTR,(10,P+22)                                                
         GOTO1 REPORT                                                           
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         LA    RF,BUFFREC                                                       
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         XC    BUFFREC,BUFFREC                                                  
         XC    LASTKEY,LASTKEY                                                  
POSB0280 EQU   *                                                                
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    POSB0320            YES - FINISHED                               
POSB0300 EQU   *                                                                
         BAS   RE,SETPRINT         GET UP PRINT OUTPUT                          
         MVI   TSOFFACT,TSANXT     SET TO READ NEXT                             
         B     POSB0280            GO BACK FOR NEXT LINE                        
POSB0320 EQU   *                                                                
*                                                                               
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
         DS    0F                                                               
BUFFREC  DS    CL16                RECORD BUFFER                                
         EJECT                                                                  
SETPRINT NTR1                                                                   
         OC    LASTKEY,LASTKEY     FIRST PASS?                                  
         BNZ   SETP0020            NO                                           
         MVI   FORCEHED,C'Y'       YES - FORCE PAGE BREAK                       
SETP0020 EQU   *                                                                
         CLC   LASTKEY(2),BUFFREC  SAME OFFICE IN PROGRESS?                     
         BE    SETP0040            YES                                          
         MVI   FORCEHED,C'Y'       NO  - FORCE PAGE BREAK                       
SETP0040 EQU   *                                                                
         CLC   LASTKEY,BUFFREC     SAME OFFICE/CODE/COMPANY?                    
         BE    SETP0120            YES                                          
         BAS   RE,GETOCOM          NO  - RETRIEVE OFFICE COMMENT                
         CLC   LASTKEY(4),BUFFREC  SAME OFFICE/CODE?                            
         BNE   SETP0120            NO                                           
         MVC   P+9(2),=C'**'       YES - SET OVERLAP INDICATOR                  
SETP0120 EQU   *                                                                
         CLC   LASTKEY(2),BUFFREC  SAME OFFICE?                                 
         BE    SETP0160            YES - DON'T SHOW OFFICE                      
         MVC   P(2),BUFFREC        MOVE OFFICE TO PRINT                         
SETP0160 EQU   *                                                                
         CLC   LASTKEY(4),BUFFREC  SAME OFFICE/CODE?                            
         BE    SETP0200            YES - DON'T SHOW CODE                        
         MVC   P+3(2),BUFFREC+2    MOVE CODE   TO PRINT                         
SETP0200 EQU   *                                                                
         CLC   LASTKEY(6),BUFFREC  SAME OFFICE/CODE/COMPANY?                    
         BE    SETP0240            YES - DON'T SHOW COMPANY                     
         MVC   P+6(2),BUFFREC+4    MOVE COMPANY TO PRINT                        
SETP0240 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BUFFREC+6),(5,P+12)                               
         GOTO1 DATCON,DMCB,(3,BUFFREC+9),(5,P+22)                               
         GOTO1 HEXOUT,DMCB,BUFFREC+12,P+32,4,=C'TOG'                            
         GOTO1 REPORT                                                           
         MVC   LASTKEY,BUFFREC     SET LASTKEY                                  
         XIT1                                                                   
         EJECT                                                                  
GETOCOM  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'34'                                                        
         MVC   KEY+20(2),BUFFREC+4 INSERT COMPANY CODE                          
         MVC   KEY+22(2),BUFFREC   INSERT OFFICE CODE                           
         MVC   KEY+24(2),BUFFREC+2 INSERT COMMENT CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     SAME KEY?                                    
         BE    GETO0040            YES                                          
         MVC   P+45(28),=C'***COMMENT NOT FOUND ON FILE***'                     
         B     GETO0080                                                         
GETO0040 EQU   *                                                                
         GOTO1 GETCON              RETRIEVE RECORD                              
         LA    R1,ROCMDSEL                                                      
         ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         ZIC   RF,1(R1)            GET ELEMENT LENGTH                           
         LA    RE,3                SUB 3: CODE, LEN, FOR EX                     
         SR    RF,RE                                                            
         EX    RF,GETO0100         MOVE STATEMENT BY LENGTH                     
GETO0080 EQU   *                                                                
         XIT1                                                                   
GETO0100 MVC   P+45(0),2(R1)                                                    
         EJECT                                                                  
*                                                                               
         DROP  R1                                                               
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
         SPACE 1                                                                
*                                                                               
PROCCTR  DS    F                   CONTRACTS READ      CTR                      
TSARCTR  DS    F                   CONTRACTS PROCESSED CTR                      
COMPANY  DS    CL2                                                              
LASTKEY  DS    CL6                 LAST KEY USED                                
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
         ORG RCONREC                                                            
       ++INCLUDE REGENOCM                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE DDTSARD                                                        
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113REREP1X02 05/01/02'                                      
         END                                                                    
