*          DATA SET REREP1H0G  AT LEVEL 032 AS OF 05/01/02                      
*PHASE RE1H02B,*                                                                
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
* JAN07/98 (BU ) --- SWEEP KUSI ORDERS OUT OF KNBC FOR NBC          *           
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
*        DC    AL1(REQFRST),AL3(????)                                           
         DC    AL1(PROCCONT),AL3(KNBC)    SWEEP KNBC ORDERS                     
*                                                                               
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(BUCK)    SWEEP/FIX NEG BUCKETS                 
         DC    AL1(REQLAST),AL3(TOTS)                                           
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
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
*                                                                               
KNBC     NTR1                                                                   
*                                                                               
*        BAS   RE,INITIAL                                                       
*        XC    KEY,KEY                                                          
*        MVI   KEY,X'0C'           SET UP KEY FOR BLAIR                         
*        MVC   KEY+2(2),=C'BF'     INSERT BANNER USER ID                        
*        GOTO1 HIGH                GET FIRST RECORD                             
*        B     KNBC0080                                                         
KNBC0040 EQU   *                                                                
*        GOTO1 SEQ                 GET NEXT RECORD                              
KNBC0080 EQU   *                                                                
*        CLC   KEY(4),KEYSAVE      SAME REC TYPE/USER?                          
*        BE    KNBC0100            YES                                          
*        B     MODEEXIT                                                         
KNBC0100 EQU   *                                                                
*        GOTO1 GETCON              YES - READ THE CONTRACT RECORD               
         L     RF,PROCCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
         LA    R4,RCONELEM         FIND X'04' ELEMENT (INV$)                    
KNBC0120 EQU   *                                                                
         ZIC   R1,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R4),0             END OF RECORD?                               
         BE    MODEEXIT            YES - FINISHED WITH CONTRACT                 
         CLI   0(R4),X'06'         X'04' ELEMENT?                               
         BNE   KNBC0120            NO  - BUMP TO NEXT ELEMENT                   
*                                  YES -                                        
         USING RCONSPEL,R4                                                      
         ZIC   RF,RCONSPNU                                                      
         LA    RE,RCONSPST                                                      
KNBC0140 EQU   *                                                                
         CLC   =C'KUSI',0(RE)      STATION = KUSI?                              
         BE    KNBC0160            YES - CHECK %                                
         LA    RE,9(RE)            NO  - BUMP TO NEXT ELEMENT                   
         BCT   RF,KNBC0140                                                      
         B     MODEEXIT            STATION NOT FOUND                            
KNBC0160 EQU   *                                                                
         L     RF,KUSICTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,KUSICTR                                                       
         OC    5(4,RE),5(RE)       ANY PERCENT?                                 
         BZ    KNBC0180            NO                                           
         MVC   P+26(12),=C'HAS PERCENT:'                                        
         EDIT  (4,5(RE)),(6,P+39),2                                             
         L     RF,PCTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PCTCTR                                                        
*                                                                               
KNBC0180 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+1(09),=C'CONTRACT:'                                            
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,P+50)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,P+60)                              
         GOTO1 REPORT                                                           
         B     MODEEXIT            GO GET NEXT CONTRACT                         
TOTS     NTR1                                                                   
         MVC   P+1(20),=C'CONTRACTS PROCESSED:'                                 
         EDIT  PROCCTR,(6,P+40)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'CONTRACTS W/KUSI   :'                                 
         EDIT  KUSICTR,(6,P+40)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'CONTRACTS W/KSUI % :'                                 
         EDIT  PCTCTR,(6,P+40)                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
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
KUSICTR  DS    F                   CONTRACTS WITH KUSI                          
PCTCTR   DS    F                   CONTRACTS WITH KUSI PERCENT                  
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
       ++INCLUDE REGENINVA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032REREP1H0G 05/01/02'                                      
         END                                                                    
