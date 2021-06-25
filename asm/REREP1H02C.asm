*          DATA SET REREP1H02C AT LEVEL 007 AS OF 05/01/02                      
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
*                                  SPACE FOR READING RECORD                     
         TITLE 'REREP1H02 - GENERAL CONTRACT FIXER   '                          
*********************************************************************           
*                                                                   *           
*        REREP1H02 --- GENERAL CONTRACT FIXER                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
*                                                                   *           
* JAN22/96 (BU ) --- CORRECT TTV1/TTV2 STATION RECORDS:             *           
*                    TRAFFIC = 'G' - RANK X'F0'                     *           
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
         DC    AL1(REQFRST),AL3(KZSTATS)  RESET KATZ STATION RECORDS            
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POSB)    CLOSED-OUT MONTHS: OPEN UP            
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
*                                                                               
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
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
*   READ KATZ 'TAPE' FILE.  ADD INFORMATION TO EACH OF THE  TRAFFIC             
*      SYSTEM CODE OF 'A', AND ADD AN X'05' ELEMENT FOR GRAPH.                  
*                                                                               
*                                                                               
KZSTATS  NTR1                                                                   
         LA    R5,RSTAREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         LA    R2,KATZREPS                                                      
*                                                                               
KZST0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    KZST0900            YES - FINISHED                               
         XC    KEY,KEY             CLEAR KEY FOR STATION READ                   
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),0(R2)     INSERT REP CODE                              
         GOTO1 HIGH                                                             
         B     KZST0060                                                         
KZST0040 EQU   *                                                                
         GOTO1 SEQ                                                              
KZST0060 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME RECORD/REP?                             
         BE    KZST0080            YES                                          
         LA    R2,2(R2)            NO  - BUMP TO NEXT COMPANY                   
         B     KZST0020            GO BACK FOR NEXT COMPANY                     
KZST0080 EQU   *                                                                
         GOTO1 GREC                YES - READ THE RECORD                        
         MVI   RSTATRAF,C'G'       SET TRAFFIC CODE                             
         OI    RSTARANK,X'F0'      SET ZONE BITS FOR RANK                       
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   KZST0480            NO                                           
         BAS   RE,PUTSTA           REWRITE RECORD                               
KZST0480 EQU   *                                                                
         L     RF,STAUPDTD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAUPDTD                                                      
         MVC   P+1(23),=C'STATION RECORD: UPDATED'                              
         GOTO1 REPORT                                                           
         ZICM  RF,RSTALEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RSTAREC),RSTAREC,C'DUMP',(RF),=C'2D'          
         B     KZST0040            GO BACK FOR NEXT                             
KZST0900 EQU   *                                                                
         MVC   P+1(17),=C'***JOB TOTALS*** '                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'FILL-IN RECORDS READ:'                                
         EDIT  KATZCTR,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'STATIONS NOT FOUND  :'                                
         EDIT  STAMISSD,(8,P+30)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'INCOMPLETE FILL-INS :'                                
         EDIT  BADKATZ,(8,P+30)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'STATIONS UPDATED    :'                                
         EDIT  STAUPDTD,(8,P+30)                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         DS    0F                                                               
         DC    C'*COMPS**'                                                      
KATZREPS DC    C'V1'                                                            
         DC    C'V2'                                                            
         DC    X'0000'                                                          
         DS    0F                                                               
         DC    C'*RECORD*'                                                      
RECAREA  DS    0CL80                                                            
PLSTAT   DS    CL4    +0           STATION CALL LETTERS                         
PLAMFM   DS    CL2    +4           AM/FM                                        
PLDIVISN DS    CL1    +6           DIVISION=COMPANY CODE                        
PLOWNERD DS    CL3    +7           DDS OWNER                                    
PLOWNERK DS    CL3    +10          KATZ OWNER                                   
PLMKTABR DS    CL4    +13          MARKET ABBREVIATION                          
PLMKTNAM DS    CL18   +17          MARKET NAME                                  
PLREGION DS    CL2    +35          REGION                                       
PLTIMZON DS    CL1    +37          TIME ZONE                                    
PLFREQ   DS    CL4    +38          FREQUENCY                                    
PLTRAFFC DS    CL1    +42          TRAFFIC                                      
PLRECV   DS    CL5    +43          RECEIVING ID                                 
PLFAX    DS    CL10   +48          FAX: AREA/EXCH/EXTENSION                     
         DS    CL22                SPARE                                        
PLLENGTH EQU   *-PLSTAT            RECORD LENGTH                                
*                                                                               
KATZCTR  DS    F                                                                
STAMISSD DS    F                                                                
BADKATZ  DS    F                                                                
STAUPDTD DS    F                                                                
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
PUTBUY   LA    RF,RBUYREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
PUTSTA   LA    RF,RSTAREC                                                       
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
LASTSTAT DS    CL5                                                              
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007REREP1H02C05/01/02'                                      
         END                                                                    
