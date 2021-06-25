*          DATA SET REREPCM02  AT LEVEL 052 AS OF 05/01/02                      
*PHASE RECM02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPCM02 - SACP SWEEP'                                         
*********************************************************************           
*                                                                   *           
*        REREPCM02 --- SACP SWEEP                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R9  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RECM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RECM02,R7,R9,RR=RE                                           
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
         B     MAINEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MAINEXIT                                                         
MAINEXIT EQU   *                                                                
         XIT1                                                                   
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
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
*                                                                               
*                                  EVERYTHING IS DONE IN INITIAL                
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
*        DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   P+1(16),=C'ENTERING INITIAL'                                     
         GOTO1 REPORT                                                           
         XC    SORTREC(LSORTREC),SORTREC                                        
         XC    SORTREC2(LSORTREC),SORTREC2                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   UNDRSCOR,X'6D'      SET FIELD TO UNDERSCORE                      
         MVC   UNDRSCOR+1(L'UNDRSCOR),UNDRSCOR                                  
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVC   P+1(16),=C'OPENING SORTER  '                                     
         GOTO1 REPORT                                                           
         LA    R3,REPTABLE         A(IDS WANTED)                                
         XC    SORTREC,SORTREC                                                  
         XC    SORTREC2,SORTREC2                                                
INIT0010 EQU   *                                                                
         MVC   P+1(16),=C'NEW COMPANY:    '                                     
         MVC   P+20(2),0(R3)                                                    
         GOTO1 REPORT                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           INSERT RECORD TYPE                           
         MVC   KEY+2(2),0(R3)      INSERT REP CODE                              
         GOTO1 HIGH                READ FOR KEY: 1ST CONTRACT                   
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
INIT0040 EQU   *                                                                
         CLC   KEY(04),KEYSAVE     KEY FOUND?  THROUGH REP?                     
         BE    INIT0060            YES                                          
         LA    R3,2(R3)            NO  - BUMP TO NEXT COMPANY                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    INIT0200            YES - NOW PROCESS THE INFORMATION            
         B     INIT0010            NO  - GO BACK AND PULL INFO                  
INIT0060 EQU   *                                                                
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         GOTO1 GREC2                                                            
         MVI   SORTREC,X'08'       SET ADVERTISER                               
         MVC   SORTREC+1(4),KEY+19 INSERT ADVERTISER                            
         BAS   RE,SORTGEN          RELEASE TO SORT                              
         XC    SORTREC,SORTREC                                                  
         CLC   RCONPRD,SPACES      ANY PRODUCT?                                 
         BE    INIT0080            NO                                           
         MVI   SORTREC,X'09'       SET ADVERTISER PRODUCT                       
         MVC   SORTREC+1(4),KEY+19 INSERT ADVERTISER                            
         MVC   SORTREC+5(3),RCONPRD                                             
*                                  INSERT PRODUCT                               
         BAS   RE,SORTGEN          RELEASE TO SORT                              
INIT0080 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         MVI   SORTREC,X'0A'       SET AGENCY                                   
         MVC   SORTREC+1(6),KEY+13 INSERT AGENCY, AGY OFF                       
         GOTO1 SORTGEN             RELEASE TO SORT                              
         XC    SORTREC,SORTREC                                                  
         B     INIT0020            GO BACK FOR NEXT                             
REPTABLE DC    C'RMS1IFCM'                                                      
         DC    H'00'               DELIMITER                                    
         EJECT                                                                  
INIT0200 EQU   *                                                                
         MVC   P+1(11),=C'CONTRACTS ='                                          
         EDIT  CONCTR,(7,P+20)                                                  
         GOTO1 REPORT                                                           
INIT0240 EQU   *                                                                
         GOTO1 GETSORT                                                          
         CLI   SORTREC,X'FF'       END OF FILE?                                 
         BE    INIT0800            YES - JOB FINISHED                           
         CLC   SORTREC,SORTREC2    ALREADY SEEN?                                
         BE    INIT0240            YES -                                        
         MVC   SORTREC2,SORTREC    LAST SAVED                                   
         CLI   SORTREC,X'08'       ADVERTISER?                                  
         BE    INIT0280            YES                                          
         CLI   SORTREC,X'09'       PRODUCT?                                     
         BE    INIT0320            YES                                          
         CLI   SORTREC,X'0A'       AGENCY?                                      
         BE    INIT0360            YES                                          
         B     INIT0240            GO BACK FOR NEXT                             
INIT0280 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           INSERT KEY TYPE                              
         MVC   KEY+21(4),SORTREC+1 INSERT ADVERTISER CODE                       
         MVC   KEY+25(2),=C'IR'    INSERT MASTER CODE                           
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    INIT0240            YES -                                        
         MVC   P+1(10),=C'ADVERTISER'                                           
         MVC   P+16(4),SORTREC+1                                                
         GOTO1 REPORT                                                           
         B    INIT0240             GO BACK FOR NEXT                             
INIT0320 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'           INSERT KEY TYPE                              
         MVC   KEY+18(7),SORTREC+1 INSERT PRODUCT    CODE                       
         MVC   KEY+25(2),=C'IR'    INSERT MASTER CODE                           
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    INIT0240            YES -                                        
         MVC   P+1(10),=C'PRODUCT   '                                           
         MVC   P+16(7),SORTREC+1                                                
         GOTO1 REPORT                                                           
         B    INIT0240             GO BACK FOR NEXT                             
INIT0360 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           INSERT KEY TYPE                              
         MVC   KEY+19(6),SORTREC+1 INSERT AGENCY     CODE                       
         MVC   KEY+25(2),=C'IR'    INSERT MASTER CODE                           
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    INIT0240            YES -                                        
         MVC   P+1(10),=C'AGENCY    '                                           
         MVC   P+16(6),SORTREC+1                                                
         GOTO1 REPORT                                                           
         B    INIT0240             GO BACK FOR NEXT                             
INIT0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GREC2    MVC   COMMAND,GETREC                                                   
         LA    RF,RCONREC                                                       
         ST    RF,AIOAREA                                                       
         NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 ,DMCB,((R4),COMMAND),=C'REPFILE',(R2),(R5),DMWORK                
         GOTO1 DATAMGR                                                          
         B     RGENIODM                                                         
*                                                                               
*  GENERATE SORT RECORDS:  INSERT RATE INTO EACH UNIQUE STATION FOR             
*     THESE RECORDS, WHICH CONTAIN THE CONTRACT DOLLARS                         
*                                                                               
SORTGEN  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XIT1                                                                   
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1                                                                   
         CLI   SORTREC,X'FF'       EOF REACHED?                                 
         BE    GSOR0900            YES                                          
         MVI   SORTREC,X'FF'       SET 'EOF REACHED'                            
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0900            TEST RETURN ZERO=EOF                         
         MVC   SORTREC(LSORTREC),0(R6)                                          
*                                  LOAD RECORD TO SORTREC                       
GSOR0900 EQU   *                      OVERLAYS 'EOF REACHED' FLAG               
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
         XIT1                                                                   
         EJECT                                                                  
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
COMMAND  DS    CL8                                                              
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
CONCTR   DS    F                                                                
         SPACE 3                                                                
         DC    CL12'**SORTREC **'                                               
SORTREC  DS    CL10                                                             
LSORTREC EQU   *-SORTREC                                                        
*                                                                               
         DC    CL12'**SORTREC2**'                                               
SORTREC2 DS    0CL(LSORTREC)       SAVED SORTKEY                                
*                                                                               
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
*                                                                               
AIOAREA  DS    F                                                                
*                                                                               
PROCCTR  DC    PL4'0'              CONTRACTS PROCESSED CTR                      
PAGENUM  DS    X                   PAGE NUMBER COUNTER                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=10'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
         ORG   RCONREC                                                          
       ++INCLUDE REGENSTR                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
         CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052REREPCM02 05/01/02'                                      
         END                                                                    
