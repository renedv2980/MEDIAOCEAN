*          DATA SET REREP1H02G AT LEVEL 026 AS OF 05/01/02                      
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
         TITLE 'REREP1H02F - CONTRACT EXPANSION COUNTER'                        
*********************************************************************           
*                                                                   *           
*        REREP1H02F  --- EXPANSION COUNTER                          *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* MAY28/97 (BU ) --- INITIAL ENTRY:                                 *           
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
         NMOD1 0,**RE1H02,R7,R9,RR=RE                                           
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
***      DC    AL1(REQFRST),AL3(CONX)    BUYLINE SCAN                           
***      DC    AL1(REQFRST),AL3(CONX2)   ADVERT DISPLAY                         
         DC    AL1(REQFRST),AL3(CONY3)   ADVERT DISPLAY                         
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
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR TYPE N/X ORDERS.  WHEN FOUND, SCAN BUYS FOR              
*      X'08' ELEMENTS WITHOUT A TRANSFER DATE.  IF FOUND, DISPLAY               
*      CONTRACT NUMBER                                                          
*                                                                               
*                                                                               
CONX     NTR1                                                                   
*                                                                               
         LA    R5,RCONREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTART         SAVE START TIME                              
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0C'           REQUEST CONTRACT RECORD                      
         MVC   KEY+2(2),RCREPFL    INSERT REP INTO KEY                          
         GOTO1 HIGH                READ FIRST RECORD                            
         B     CONX0020                                                         
CONX0010 EQU   *                                                                
         GOTO1 SEQ                                                              
CONX0020 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE/REP?                        
         BNE   CONX0800            NO  - FINISHED                               
         L     RF,CONCOUNT         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
         CLC   =C'WIDEOPEN',QUESTOR                                             
         BE    CONX0025                                                         
         C     RF,=F'50'           PROCESS N CONTRACTS                          
         BH    CONX0800            FINISHED                                     
CONX0025 EQU   *                                                                
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
         C     RF,=F'250'          DISPLAY EVERY N RECORDS                      
         BNE   CONX0030                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  CONCOUNT,(10,P+15)   DISPLAY TOTAL  COUNTER                      
         EDIT  TOOBIG,(10,P+41)     DISPLAY TOOBIG COUNTER                      
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+55,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
CONX0030 EQU   *                                                                
         GOTO1 GREC                                                             
         ZICM  R6,RCONLEN,2        EXTRACT LENGTH OF RECORD                     
*        LA    R5,RCONREC                                                       
*        GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
         LA    R1,RCONELEM         SET TO FIRST ELEMENT                         
         SR    R3,R3               RESET COUNTER                                
CONX0040 EQU   *                                                                
         ZIC   RE,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RE                                                            
         CLI   0(R1),0             END OF RECORD REACHED?                       
         BE    CONX0060            YES - CALCULATE NEW SIZE                     
         CLI   0(R1),3             ESTIMATE BUCKET?                             
         BNE   CONX0040            NO  - GO BACK FOR NEXT ELEMENT               
         LA    R3,1(R3)                                                         
         B     CONX0040            GO BACK FOR NEXT ELEMENT                     
CONX0060 EQU   *                                                                
         SR    R2,R2                                                            
         M     R2,=F'10'           MULTIPLY COUNT BY 10 CHARS                   
         AR    R6,R3               ADD NEW ELTS LENGTH                          
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'CONFIGS:'                                             
*        EDIT  (R3),(10,P+12)                                                   
*        EDIT  (R6),(10,P+24)                                                   
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         C     R6,=F'1976'                                                      
         BNH   CONX0010            DOESN'T EXCEED: GO BACK                      
         L     RF,TOOBIG                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOOBIG                                                        
         B     CONX0010            GO BACK FOR NEXT CONTRACT                    
CONX0800 EQU   *                                                                
         MVC   P+1(37),=C'CONTRACT RECORDS PROCESSED:          '                
         EDIT  CONCOUNT,(12,P+28),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(27),=C'NUMBER IN XS OF 1976 CHARS:'                          
         EDIT  TOOBIG,(12,P+28),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTART,P+20,4,=C'TOG'                              
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SEQUENTIALLY PROCESS ADVERTISER RECORDS FOR MG.                             
*                                                                               
*                                                                               
CONX2    NTR1                                                                   
*                                                                               
         LA    R5,RADVREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTART         SAVE START TIME                              
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'08'           REQUEST ADVERTISER RECORD                    
         MVC   KEY+21(4),=C'ANE '  INSERT ADVERTISER CODE                       
         MVC   KEY+25(2),=C'MG'    SET REP CODE                                 
         GOTO1 HIGH                READ FIRST RECORD                            
         B     CON20020                                                         
CON20010 EQU   *                                                                
         GOTO1 SEQ                                                              
CON20020 EQU   *                                                                
         CLI   KEY,X'08'           SAME RECORD TYPE?                            
         BNE   CON20800            NO  - FINISHED                               
         L     RF,CONCOUNT         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
         CLC   =C'WIDEOPEN',QUESTOR                                             
         BE    CON20025                                                         
         C     RF,=F'100'          PROCESS N ADVERTISERS                        
         BH    CON20800            FINISHED                                     
CON20025 EQU   *                                                                
         MVC   P+1(27),KEY         DISPLAY KEY FOUND                            
         GOTO1 REPORT                                                           
*                                                                               
         B     CON20010            GO BACK FOR NEXT ADVERTISER                  
CON20800 EQU   *                                                                
         MVC   P+1(37),=C'ADVERT   RECORDS PROCESSED:          '                
         EDIT  CONCOUNT,(12,P+28),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTART,P+20,4,=C'TOG'                              
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR RCONTEM ERRORS.                                          
*                                                                               
*                                                                               
CONY3    NTR1                                                                   
*                                                                               
         LA    R5,RCONREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTART         SAVE START TIME                              
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0C'           REQUEST CONTRACT RECORD                      
         MVC   KEY+2(2),RCREPFL    INSERT REP INTO KEY                          
         GOTO1 HIGH                READ FIRST RECORD                            
         B     CONY0020                                                         
CONY0010 EQU   *                                                                
         GOTO1 SEQ                                                              
CONY0020 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE/REP?                        
         BNE   CONY0800            NO  - FINISHED                               
         L     RF,CONCOUNT         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
         CLC   =C'WIDEOPEN',QUESTOR                                             
**       BE    CONY0025                                                         
         B     CONY0030                                                         
** FORCE FULL PRINT                                                             
         C     RF,=F'50'           PROCESS N CONTRACTS                          
         BH    CONY0800            FINISHED                                     
CONY0025 EQU   *                                                                
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
         C     RF,=F'250'          DISPLAY EVERY N RECORDS                      
         BNE   CONY0030                                                         
         BNE   CONY0030                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  CONCOUNT,(10,P+15)   DISPLAY TOTAL  COUNTER                      
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+55,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
CONY0030 EQU   *                                                                
         GOTO1 GREC                                                             
         CLI   RCONTEM,C'R'        NOT RADIO?                                   
         BE    CONY0010            YES - SKIP IT                                
**       CLC   RCONSAL,=C'633'      S/P TEST                                    
**       BNE   CONY0010            FAILS -  SKIP IT                             
*                                  NO  - PRINT IT                               
         MVC   P+1(09),=C'    TEAM:'                                            
         GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4,=C'TOG'                              
         MVC   P+24(2),RCONTEM                                                  
**       CLC   RCONTEM,=C'R '      NOT RADIO+SPACE?                             
**       BE    CONY0040            YES - SKIP IT                                
         MVC   P+30(04),=C'<---'                                                
CONY0040 EQU   *                                                                
         GOTO1 REPORT                                                           
         B     CONY0010            GO BACK FOR NEXT CONTRACT                    
CONY0800 EQU   *                                                                
         MVC   P+1(21),=C'**   END OF JOB   ***'                                
         GOTO1 REPORT                                                           
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTART,P+20,4,=C'TOG'                              
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
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
PROCCTR  DS    F                   CONTRACTS READ      CTR                      
CHGDCTR  DS    F                   CONTRACTS PROCESSED CTR                      
BIGRCTR  DS    F                   CONTRACTS MADE LARGER                        
SMLRCTR  DS    F                   CONTRACTS MADE SMALLER                       
SAMESIZE DS    F                   CONTRACTS SAME SIZE                          
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
CONCOUNT DS    F                                                                
TOOBIG   DS    F                                                                
DISPCTR  DS    F                                                                
RUNSTART DS    F                                                                
RUNEND   DS    F                                                                
SAVEREGS DS    11F                                                              
SAVCON#  DS    CL4                                                              
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
**PAN#1  DC    CL21'026REREP1H02G05/01/02'                                      
         END                                                                    
