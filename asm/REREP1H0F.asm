*          DATA SET REREP1H0F  AT LEVEL 028 AS OF 05/01/02                      
*PHASE RE1H02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1H02 - GENERAL SWEEP'                                      
*********************************************************************           
*                                                                   *           
*        REREP1H02 --- GENERAL SWEEP                                *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* NOV6/00  (BU ) --- INITIAL ENTRY:  STATION DISPLAY                *           
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
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET TO STATION                               
INIT0020 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     INIT0060                                                         
INIT0040 EQU   *                                                                
         GOTO1 SEQ                                                              
INIT0060 EQU   *                                                                
         CLI   KEY,2               SAME KEY ?                                   
         BNE   INIT0600            NO  - FINISHED                               
*                                                                               
         GOTO1 GETSTA                                                           
         LA    R2,RSTAELEM                                                      
INIT0080 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    INIT0040            YES - SKIP RECORD                            
         CLI   0(R2),X'F1'         ACTIVITY ELEMENT?                            
         BE    INIT0100                                                         
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     INIT0080            GO BACK FOR NEXT                             
INIT0100 EQU   *                                                                
         CLC   =X'640A10',RSTACTCD-RSTACTEL(R2)                                 
*                                  LAST ACTIVITY DATE PRE OCT16/00?             
         BH    INIT0040            YES - SKIP THIS RECORD                       
         OC    RSTACLDT,RSTACLDT   ANY CLOSE DATE?                              
         BNZ   INIT0040            YES - SKIP THIS RECORD                       
         MVC   P+1(08),=C'STATION:'                                             
         MVC   P+10(2),RSTAKREP                                                 
         MVC   P+13(5),RSTAKSTA                                                 
         MVC   P+22(09),=C'LAST ACT:'                                           
         GOTO1 DATCON,DMCB,(3,RSTACTCD-RSTACTEL(R2)),(5,P+34)                   
         MVC   P+44(06),=C'CLOSE:'                                              
         MVC   P+52(02),RSTACLDT                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     INIT0040                                                         
INIT0300 EQU   *                                                                
         DC    H'0'                                                             
INIT0600 EQU   *                                                                
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
GETSTA   LA    RF,RSTAREC                                                       
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
SRATYP1  DS    F                   TYPE 1 RECORDS READ                          
SRATYP2  DS    F                   TYPE 2 RECORDS READ                          
SRATYP3  DS    F                   TYPE 3 RECORDS READ                          
SRATYP4  DS    F                   TYPE 4 RECORDS READ                          
TOTALCTR DS    F                                                                
PULSECTR DS    F                                                                
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
MYP      DS    CL132                                                            
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
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REREP1H0F 05/01/02'                                      
         END                                                                    
