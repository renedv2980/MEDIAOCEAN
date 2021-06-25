*          DATA SET REREP1H0BE AT LEVEL 044 AS OF 05/01/02                      
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
* JUN24/96 (BU ) --- INITIAL ENTRY:  CONTRACT ANALYZER:             *           
*                    AVERAGE SIZE, BUCKET COUNT                     *           
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
         MVI   RUNCOUNT,C'Y'                                                    
         XC    CTRCONTS,CTRCONTS   INITIALIZE CONTRACT COUNTER                  
         XC    BUCK03CT,BUCK03CT   INITIALIZE BUCKET   COUNTER                  
         XC    BUCK53CT,BUCK53CT   INITIALIZE BUCKET   COUNTER                  
         XC    CONTOTAL,CONTOTAL   INITIALIZE TOTAL BYTES CTR                   
         XC    MAXSIZE,MAXSIZE     INITIALIZE TOTAL BYTES CTR                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8D'           RETRIEVE KEYS                                
         MVC   KEY+1(2),RCREPFL    START AT DESIRED REP                         
         MVC   KEY+8(2),=X'C621'   START AT JAN1/99                             
INIT0010 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     INIT0040                                                         
INIT0020 EQU   *                                                                
**       CLC   TOTALCTR,=F'100000' TEST STOP                                    
**       BH    INIT0300                                                         
         GOTO1 SEQ                                                              
INIT0040 EQU   *                                                                
         CLI   KEY+16,1            FIRST  SUBKEY TYPE?                          
         BNE   INIT0020            NO  - SKIP THIS KEY                          
*                                                                               
         CLI   KEY,X'8D'           8D KEYS FINISHED?                            
         BNE   INIT0300            YES - FINISHED                               
         CLC   KEY+1(2),RCREPFL    SAME REP?                                    
         BNE   INIT0300            NO                                           
         L     RF,PULSECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PULSECTR         REPLACE COUNTER                              
         L     RF,TOTALCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTALCTR         REPLACE COUNTER                              
         CLC   PULSECTR,=F'5000'                                                
         BNE   INIT0050                                                         
         XC    PULSECTR,PULSECTR                                                
         MVC   P+01(10),=C'PROCESSED:'                                          
         EDIT  TOTALCTR,(10,P+12)                                               
         MVC   P+24(14),=C'CONS ACCEPTED:'                                      
         EDIT  CTRCONTS,(10,P+40)                                               
         MVC   P+52(14),=C'BYTE COUNTER :'                                      
         EDIT  CONTOTAL,(14,P+68)                                               
         GOTO1 REPORT                                                           
         BAS   RE,DISPTOTS                                                      
INIT0050 EQU   *                                                                
***      CLC   KEY+10(2),=X'C39D'  FLIGHT END   DATE                            
***      BL    INIT0053            BEFORE 12/29/97 - SKIP IT                    
         L     RF,CTRCONTS         ADD TO CONTRACTS READ                        
         LA    RF,1(RF)                                                         
         ST    RF,CTRCONTS                                                      
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         MVI   BUCKET03,C'N'       SET NO BUCKETS TO 'NO'                       
         MVI   BUCKET53,C'N'       SET NO BUCKETS TO 'NO'                       
         XC    CT03BUCK,CT03BUCK                                                
         XC    CT53BUCK,CT53BUCK                                                
         GOTO1 GETCON              RETRIEVE CONTRACT RECORD                     
         TM    RCONCNTL,X'80'      RECORD DELETED?                              
         BO    INIT0020            YES - SKIP THIS RECORD                       
         ZICM  RF,RCONLEN,2        GET RECORD LENG                              
         L     RE,CONTOTAL         ACCUMULATE TOTAL BYTES                       
         AR    RE,RF                                                            
         ST    RE,CONTOTAL                                                      
         CLC   RCONLEN,MAXSIZE                                                  
         BL    INIT0055                                                         
         MVC   MAXSIZE,RCONLEN                                                  
         MVC   MAXCON#,RCONKCON                                                 
INIT0055 EQU   *                                                                
         LA    R4,RCONELEM         SET A(01 ELEMENT)                            
INIT0060 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    INIT0070            YES - CHECK FOR BUCKETS                      
         CLI   0(R4),3             ESTIMATE BUCKET?                             
         BE    INIT0080            YES                                          
         CLI   0(R4),X'53'         ALTERNATE ESTIMATE BUCKET?                   
         BE    INIT0100            YES                                          
         B     INIT0200            NO                                           
INIT0070 EQU   *                                                                
         CLI   BUCKET03,C'N'       ANY 03 BUCKETS IN ORDER?                     
         BE    INIT0072            NO                                           
         L     RF,CONWBK03         INCREMENT BUCKET CON CTR                     
         LA    RF,1(RF)                                                         
         ST    RF,CONWBK03                                                      
         CLC   CT03BUCK,BUCK03MX                                                
         BNH   INIT0071                                                         
         MVC   BUCK03MX,CT03BUCK                                                
         MVC   BUCK03C#,RCONKCON                                                
INIT0071 EQU   *                                                                
         CLI   RUNCOUNT,C'Y'                                                    
*                                                                               
         B     INIT0072            DON'T PRINT INDIVIDUAL DISPLAY               
*                                                                               
         BNE   INIT0072                                                         
         MVC   P+1(10),=C'# BUCKETS:'                                           
         EDIT  CT03BUCK,(3,P+12),ZERO=NOBLANK                                   
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
         EDIT  BUCK03CT,(7,P+32),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
INIT0072 EQU   *                                                                
         CLI   BUCKET53,C'N'       ANY 53 BUCKETS IN ORDER?                     
         BE    INIT0020            NO  - GO BACK FOR NEXT ORDER                 
         L     RF,CONWBK53         INCREMENT BUCKET CON CTR                     
         LA    RF,1(RF)                                                         
         ST    RF,CONWBK53                                                      
         CLC   CT53BUCK,BUCK53MX                                                
         BNH   INIT0020                                                         
         MVC   BUCK53MX,CT53BUCK                                                
         MVC   BUCK53C#,RCONKCON                                                
         B     INIT0020                                                         
INIT0080 EQU   *                                                                
         L     RF,BUCK03CT         COUNT X'03' BUCKET                           
         LA    RF,1(RF)                                                         
         ST    RF,BUCK03CT                                                      
         L     RF,CT03BUCK         INCREMENT BUCKETS THIS ORDER                 
         LA    RF,1(RF)                                                         
         ST    RF,CT03BUCK                                                      
         MVI   BUCKET03,C'Y'                                                    
         B     INIT0200                                                         
INIT0100 EQU   *                                                                
         L     RF,BUCK53CT         COUNT X'53' BUCKET                           
         LA    RF,1(RF)                                                         
         ST    RF,BUCK53CT                                                      
         L     RF,CT53BUCK         INCREMENT BUCKETS THIS ORDER                 
         LA    RF,1(RF)                                                         
         ST    RF,CT53BUCK                                                      
         MVI   BUCKET53,C'Y'                                                    
         B     INIT0200                                                         
INIT0200 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     INIT0060            GO BACK FOR NEXT BUCKET                      
INIT0300 EQU   *                                                                
         BAS   RE,DISPTOTS                                                      
         B     MODEEXIT                                                         
         EJECT                                                                  
DISPTOTS NTR1                                                                   
         MVI   RUNCOUNT,C'N'                                                    
         MVC   P+1(26),=C'TOTAL CONTRACTS          :'                           
         EDIT  CTRCONTS,(10,P+30)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'CONTRACTS W/03 BUCKETS   :'                           
         EDIT  CONWBK03,(10,P+30)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'CONTRACTS W/53 BUCKETS   :'                           
         EDIT  CONWBK53,(10,P+30)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'TOTAL 03 BUCKETS         :'                           
         EDIT  BUCK03CT,(10,P+30)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'TOTAL 53 BUCKETS         :'                           
         EDIT  BUCK53CT,(10,P+30)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'MAX   03 BUCKETS         :'                           
         EDIT  BUCK03MX,(10,P+30)                                               
         GOTO1 HEXOUT,DMCB,BUCK03C#,P+45,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'MAX   53 BUCKETS         :'                           
         EDIT  BUCK53MX,(10,P+30)                                               
         GOTO1 HEXOUT,DMCB,BUCK53C#,P+45,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'LARGEST CONTRACT         :'                           
         EDIT  MAXSIZE,(10,P+30)                                                
         GOTO1 HEXOUT,DMCB,MAXCON#,P+45,4,=C'TOG'                               
         GOTO1 REPORT                                                           
         SR    R0,R0                                                            
         L     R1,CONTOTAL                                                      
         L     R2,CTRCONTS                                                      
         DR    R0,R2               CALCULATE AVERAGE SIZE                       
         MVC   P+1(26),=C'AVERAGE CONTRACT         :'                           
         LR    R6,R1                                                            
         EDIT  (R6),(10,P+30)                                                   
         GOTO1 REPORT                                                           
         SR    R0,R0                                                            
         L     R1,BUCK03CT                                                      
         L     R2,CONWBK03                                                      
         M     R0,=F'100'          DECIMAL ALIGN FOR DIVISION                   
         SR    R6,R6                                                            
         LTR   R2,R2                                                            
         BZ    DTOT0020                                                         
         DR    R0,R2               CALCULATE # BUCKETS/ORDER                    
         LR    R6,R1                                                            
DTOT0020 EQU   *                                                                
         MVC   P+1(26),=C'AVERAGE 03 ELTS/CONTRACT :'                           
         EDIT  (R6),(10,P+30),2,ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         SR    R0,R0                                                            
         L     R1,BUCK53CT                                                      
         L     R2,CONWBK53                                                      
         M     R0,=F'100'          DECIMAL ALIGN FOR DIVISION                   
         SR    R6,R6                                                            
         LTR   R2,R2                                                            
         BZ    DTOT0040                                                         
         DR    R0,R2               CALCULATE # BUCKETS/ORDER                    
         LR    R6,R1                                                            
DTOT0040 EQU   *                                                                
         MVC   P+1(26),=C'AVERAGE 53 ELTS/CONTRACT :'                           
         EDIT  (R6),(10,P+30),2,ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         MVC   P+20(18),=C'<<<<<<******>>>>>>'                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
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
*M010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
DM010    TM    DMCB+8,X'ED'        TEST FOR OTHER ERRORS: NOT DELETE            
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
CTRCONTS DS    F                   CONTRACTS READ      CTR                      
BUCK03CT DS    F                   X'03' BUCKETS       CTR                      
BUCK53CT DS    F                   X'53' BUCKETS       CTR                      
BUCK03MX DS    F                   X'03' BUCKETS MAX   CTR                      
BUCK53MX DS    F                   X'53' BUCKETS MAX   CTR                      
BUCK03C# DS    F                   X'03' BUCKETS MAX   CON #                    
BUCK53C# DS    F                   X'53' BUCKETS MAX   CON #                    
CT03BUCK DS    F                   X'03' BUCKETS THIS  ORDER                    
CT53BUCK DS    F                   X'53' BUCKETS THIS  ORDER                    
CONTOTAL DS    F                   TOTAL BYTES FOR CONTRACTS                    
PULSECTR DS    F                   CONTRACTS PROCESSED CTR                      
TOTALCTR DS    F                                                                
SMLRCTR  DS    F                                                                
SAMESIZE DS    F                                                                
CONWBK03 DS    F                                                                
CONWBK53 DS    F                                                                
FIRSTCON DS    CL2                                                              
MAXSIZE  DS    XL2                 LARGEST CONTRACT                             
MAXCON#  DS    XL4                                                              
BUCKET03 DS    CL1                                                              
BUCKET53 DS    CL1                                                              
RUNCOUNT DS    CL1                                                              
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
LASTSERV DS    CL2                                                              
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
**PAN#1  DC    CL21'044REREP1H0BE05/01/02'                                      
         END                                                                    
