*          DATA SET REREP1Y02  AT LEVEL 067 AS OF 05/01/02                      
*PHASE RE1Y02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1Y02  - MAKEGOOD S/P-TEAM INSERTION'                       
*********************************************************************           
*                                                                   *           
*        REREP1Y0E   --- MAKEGOOD S/P-TEAM INSERTION                *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* NOV18/98 (BU ) --- INITIAL ENTRY:                                 *           
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
RE1Y02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1Y02,R7,R9,RR=RE                                           
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
         DC    AL1(REQFRST),AL3(MKGUP)   MAKEGOOD UPDATE                        
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
*                                                                               
*   SCAN MAKEGOODS.  RETRIEVE CONTRACT RECORD, INSERT X'0A' ELTS.               
*                                                                               
*                                                                               
MKGUP    NTR1                                                                   
*                                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTART         SAVE START TIME                              
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'11'           SET TO PICK UP FIRST MKG REC                 
         GOTO1 HIGH                                                             
         B     MKUP0040                                                         
MKUP0020 EQU   *                                                                
         GOTO1 SEQ                                                              
MKUP0040 EQU   *                                                                
         CLI   KEY,X'11'           STILL MAKEGOOD RECORD?                       
         BNE   MKUP0800            NO  - FINISHED                               
         OC    KEY+21(6),KEY+21    GROUP COMMENT RECORD?                        
         BNZ   MKUP0020            NO  - SKIP IT                                
*                                                                               
*   TEST CUTOFF                                                                 
*        CLC   MKGCTR,=F'25'       CUTOFF AFTER N RECORDS                       
*        BE    MKUP0800            FINISHED                                     
*   TEST CUTOFF END                                                             
*                                                                               
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         GOTO1 GETMKG              RETRIEVE MAKEGOOD REC                        
         L     RF,MKGCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,MKGCTR                                                        
MKUP0060 EQU   *                                                                
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
         C     RF,=F'200'          DISPLAY EVERY N RECORDS                      
         BNE   MKUP0080                                                         
         BNE   MKUP0080                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  MKGCTR,(10,P+15)     DISPLAY TOTAL  COUNTER                      
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+28,4,=C'TOG'                                  
         MVC   P+37(27),KEY                                                     
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
MKUP0080 EQU   *                                                                
         MVI   READHIGH,C'N'       SET 'RESTART: RD HI' TO NO                   
         CLC   KEY+6(2),MKGREP     REP + CON# ALREADY SEEN?                     
         BNE   MKUP0100            NO  -                                        
         CLC   KEY+15(4),MKGCON#                                                
         BE    MKUP0160            YES - INSERT PRE-BUILT ELT                   
MKUP0100 EQU   *                                                                
         MVI   READHIGH,C'Y'       SET 'RESTART: RD HI' TO YES                  
         MVC   MKGREP,KEY+6        SAVE REP OF NEW CONTRACT                     
         MVC   MKGCON#,KEY+15      SAVE CONTRACT #                              
         XC    WORK,WORK           CLEAR WORK AREA                              
         PACK  WORK+0(1),KEY+18(1) REVERSE THE NUMBER                           
         PACK  WORK+1(1),KEY+17(1)                                              
         PACK  WORK+2(1),KEY+16(1)                                              
         PACK  WORK+3(1),KEY+15(1)                                              
*        MVC   P+1(08),=C'CON REV:'                                             
*        MVC   P+10(2),MKGREP                                                   
*        GOTO1 HEXOUT,DMCB,KEY+15,P+15,4,=C'TOG'                                
*        GOTO1 HEXOUT,DMCB,WORK+0,P+35,4,=C'TOG'                                
*        GOTO1 REPORT                                                           
***>>>   DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             SET UP CONTRACT PASSIVE                      
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),MKGREP    INSERT REP CODE                              
         MVC   KEY+23(4),WORK      INSERT CONTRACT NUMBER                       
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    MKUP0110            YES                                          
         MVC   P+1(24),=C'NO CONTRACT FOR MAKEGOOD'                             
         MVC   P+27(27),KEYSAV2                                                 
         GOTO1 REPORT              PRINT ERROR MESSAGE                          
         XC    MKGREP,MKGREP       CLEAR SAVED VALUE                            
         L     RF,NOCONCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NOCONCTR                                                      
         B     MKUP0180            SKIP MAKEGOOD RECORD                         
MKUP0110 EQU   *                                                                
         GOTO1 GETCON              RETRIEVE CONTRACT RECORD                     
*                                  BUILD NEW 0A ELT                             
         XC    NEW0ADSP,NEW0ADSP   CLEAR                                        
         XC    NEW0ADCT,NEW0ADCT                                                
         MVC   NEW0ASAL,RCONSAL                                                 
         MVC   NEW0AOFF,RCONKOFF                                                
         MVC   NEW0ATEM,RCONTEM                                                 
         MVC   NEW0ASTA,RCONKSTA                                                
         MVC   NEW0AADV,RCONKADV                                                
         MVC   NEW0AAGY,RCONKAGY                                                
         MVC   NEW0AAOF,RCONKAOF                                                
         MVC   NEW0AGRP,RCONKGRP                                                
         MVC   NEW0AFLT,RCONDATE                                                
         LA    R2,RCONELEM         FIND DEVELOPMENT ELT                         
MKUP0120 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    MKUP0160            YES - NO DEV ELT PRESENT                     
         CLI   0(R2),X'18'         DEVELOPMENT ELT?                             
         BE    MKUP0140            YES -                                        
         ZIC   RE,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RE                                                            
         B     MKUP0120            GO BACK FOR NEXT                             
MKUP0140 EQU   *                                                                
         MVC   NEW0ADSP,RCONDVSP-RCONDVEL(R2)                                   
*                                  INSERT DEV S/P                               
         MVC   NEW0ADCT,RCONDVCT-RCONDVEL(R2)                                   
*                                  INSERT DEV CON TYPE                          
MKUP0160 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'0A',RMKGREC),0,0            
*                                  DELETE ANY PREEXISTING ELTS                  
                                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RMKGREC,NEW0AELT               
*                                  ADD NEW ELT                                  
MKUP0180 EQU   *                                                                
         CLI   READHIGH,C'Y'       RESTART: RD HI' SET TO YES?                  
         BNE   MKUP0200            NO  - RESTART NOT NEEDED                     
MKUP0190 EQU   *                                                                
         PRINT GEN                                                              
         MOVE  (RSTAREC,800),RMKGREC     SAVE THIS MAKEGOOD                     
         MVC   KEY,KEYSAV2         RESET MAKEGOOD KEY                           
         GOTO1 HIGH                RESTART THE SEQUENCE                         
         GOTO1 GETMKG              RETRIEVE THE MAKEGOOD RECORD                 
         MOVE  (RMKGREC,800),RSTAREC                                            
         PRINT NOGEN                                                            
*                                  RESET THE NEW MAKEGOOD RECORD                
MKUP0200 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   MKUP0220            NO                                           
         GOTO1 PUTMKG                                                           
MKUP0220 EQU   *                                                                
*                                                                               
*   TEST CUTOFF OF PRNTBL                                                       
         CLC   MKGCTR,=F'25'       CUTOFF AFTER N RECORDS                       
         BH    MKUP0240            DON'T PRINT                                  
*   TEST CUTOFF OF PRNTBL END                                                   
*                                                                               
         ZICM  RF,RMKGLEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RMKGREC),RMKGREC,C'DUMP',(RF),=C'2D'          
MKUP0240 EQU   *                                                                
         B     MKUP0020            GO BACK FOR NEXT MAKEGOOD                    
MKUP0800 EQU   *                                                                
         MVC   P+1(21),=C'**   END OF JOB   ***'                                
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'MAKEGOODS UPDATED:'                                   
         EDIT MKGCTR,(10,P+25)                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'MAKEGOODS/NO CON :'                                   
         EDIT NOCONCTR,(10,P+25)                                                
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
GETMKG   LA    RF,RMKGREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTMKG   LA    RF,RMKGREC                                                       
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
NOCONCTR DS    F                   MGS/NO CONTRACT     CTR                      
MKGCTR   DS    F                   MAKEGOODS READ      CTR                      
CONCOUNT DS    F                   CONTRACT COUNTER                             
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
NEW23ELT DS    CL10                                                             
RELO     DS    F                   RELOCATION ADDRESS                           
DISPCTR  DS    F                                                                
RUNSTART DS    F                                                                
RUNEND   DS    F                                                                
SAVEREGS DS    11F                                                              
*                                                                               
MKGREP   DS    CL2                 LAST REP PROCESSED                           
MKGCON#  DS    CL4                 LAST CONTRACT NUMBER RETRIEVED               
*                                                                               
NEW0AELT EQU   *                                                                
NEW0ACOD DC    X'0A25'             ELEMENT CODE/LENGTH                          
*                                                                               
NEW0ASAL DS    CL3   RCONSAL       SALESPERSON CODE - ALL FOR THIS              
NEW0AOFF DS    CL2   RCONKOFF      OFFICE CODE                                  
NEW0ATEM DS    CL2   RCONTEM       SALES TEAM                                   
NEW0ASTA DS    CL5   RCONKSTA      PRIMARY STATION CALL LETTERS                 
NEW0AADV DS    CL4   RCONKADV      ADVERTISER CODE                              
NEW0AAGY DS    CL4   RCONKAGY      AGENCY CODE                                  
NEW0AAOF DS    CL2   RCONKAOF      AGENCY OFFICE CODE                           
NEW0AGRP DS    CL2   RCONKGRP      STATION GROUP/SUBGROUP                       
NEW0ADSP DS    CL3   RCONDVSP      DEVELOPEMENTAL SALESPERON CODE               
NEW0ADCT DS    CL2   RCONDVCT      DEVELOPEMENTAL CONTRACT TYPE                 
NEW0AFLT DS    CL6   RCONDATE      FLIGHT START & END DATES                     
NEW0ALNQ EQU   *-NEW0AELT          L'ELEMENT                                    
*                                                                               
READHIGH DS    CL1                                                              
FILLER   DS    6000C                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
*   MAKEGOOD RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,               
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENMKG                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067REREP1Y02 05/01/02'                                      
         END                                                                    
