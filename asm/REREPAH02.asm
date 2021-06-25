*          DATA SET REREPAH02  AT LEVEL 089 AS OF 05/01/02                      
*PHASE REAH02A,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE REGENBUC                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPAH02 - ALLHIST  REPORT'                                    
*********************************************************************           
*                                                                   *           
*        REREPAH02 --- REPPAK ALLHIST  REPORT                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JAN18/01 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*   SPECIAL NOTE:                                                   *           
*      QSTATION  =  'NOSTA'  - IGNORE STATION FILTER REQUIREMENT    *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
REAH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REAH02,R7,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
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
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
*                                  ALL DATA COLLECTION WILL BE                  
*                                     OUT OF THE 'INITIAL' ROUTINE              
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
***      DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
*                                                                               
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         LA    R0,TKWORK           CLEAR WORKING STORAGE                        
         LHI   R1,TKWORKLQ         IN PROGRAMS AREA                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,RCONREC                                                       
         ST    RF,AIO              SET UP A(IO AREA)                            
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    RF,MCIO-MASTD(,RE)                                               
*   TEST SRCE                                                                   
*        MVC   P+1(08),=C'SRCENAME'                                             
*        MVC   P+10(6),SRCESAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST SRCE END                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BEFFDATE)                              
         GOTO1 DATCON,DMCB,(0,QSTART),(2,CEFFDATE)                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         BAS   RE,SRCESIDE         ACCESS SOURCE INFORMATION                    
         OC    RECCOUNT,RECCOUNT   ANY RECORDS FOUND?                           
         BNZ   INIT0060            YES - PROCESS                                
         GOTO1 REPORT                                                           
         MVC   P+20(38),=C'**  NO CONTRACTS FOUND FOR REPORT    **'             
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
INIT0060 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+20(23),=C'NUMBER OF ORDERS FOUND:'                             
         EDIT  RECCOUNT,(4,P+50)                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+20(38),=C'**  ALLHIST/BACKBILL DISPLAY ENDED   **'             
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
MODEEXIT LTR   R0,R0                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SRCESIDE:  RETRIEVE ALL INFORMATION.  SCAN CONS LOOKING                     
*        FOR 2C ELEMENTS, WHICH INDICATE ALLHIST/BACKBILL                       
*        ORDERS.                                                                
*                                                                               
SRCESIDE NTR1                                                                   
*                                                                               
*                                                                               
*   CYCLE THROUGH REP'S CONTRACTS                                               
*                                                                               
***>>>                                                                          
SRCE0220 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING RCON8TYP,R2                                                      
                                                                                
         MVI   RCON8ETP,X'8E'      STA/FLIGHT/CON#                              
*                                                                               
         MVC   RCON8ERP,RCREPFL    INSERT SIGNON REP CODE                       
         CLC   =C'NOSTA',QSTATION  IGNORE STATIONS?                             
         BE    SRCE0240            YES                                          
         MVC   RCON8EST,QSTATION   INSERT TAKEOVER STATION                      
SRCE0240 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     SRCE0280                                                         
SRCE0260 EQU   *                                                                
         GOTO1 SEQ                                                              
SRCE0280 EQU   *                                                                
*                                                                               
*   TEST KEYS                                                                   
*        MVC   P+1(09),=C'KEY FOUND'                                            
*        MVC   P+12(27),KEY                                                     
*        MVC   P+42(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST KEYS EXIT                                                              
*                                                                               
         CLC   QSTATION,=C'NOSTA'  ANY STATION ENTERED?                         
         BNE   SRCE0300            YES                                          
         CLC   KEY(3),KEYSAVE      NO  - SAME KEY TYPE/REP?                     
         BNE   SRCE0600            NO  - FINISHED SWEEP                         
         B     SRCE0320                                                         
SRCE0300 EQU   *                                                                
         CLC   KEY(8),KEYSAVE      SAME KEY TYPE/REP/STATION?                   
         BNE   SRCE0600            NO  - FINISHED SWEEP                         
SRCE0320 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         CLC   RCON8ECN,=X'05497200'                                            
         BNE   TEST0020                                                         
         MVC   RCON8ECN,RCON8ECN                                                
TEST0020 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         CLC   QAGENCY,SPACES      AGENCY FILTER?                               
         BE    SRCE0380            NO  -                                        
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   SRCE0380            NO  - FILTER TEST ALREADY DONE -             
*                                     NEXT FILTER(S) TO BE DONE                 
         CLC   QAGYOFF,SPACES      AGENCY-OFFICE IN FILTER?                     
         BE    SRCE0340            NO                                           
         CLC   RCON8AGY,QAGENCY    YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    SRCE0380            YES - CHECK NEXT FILTER                      
         B     SRCE0360            NO  - SET TO SKIP                            
SRCE0340 EQU   *                                                                
         CLC   RCON8AGY(4),QAGENCY YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BE    SRCE0380            YES - CHECK NEXT FILTER                      
SRCE0360 EQU   *                                                                
*                                                                               
*   TEST KEYS                                                                   
*        MVC   P+1(10),=C'SKIP: AGY '                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+42(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST KEYS EXIT                                                              
*                                                                               
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     SRCE0240            RESTART ON NEXT KEY                          
*                                                                               
SRCE0380 EQU   *                                                                
         CLC   QOFFICE,SPACES      OFFICE FILTER?                               
         BE    SRCE0420            NO  -                                        
         CLI   RCON8EID,3          YES - TYPE 3 KEY (OFF/DEMO/CREAT)?           
         BNE   SRCE0260            NO  - GO BACK FOR NEXT KEY                   
         CLC   RCON8EOF,QOFFICE    ORDER FOR FILTER OFFICE?                     
         BE    SRCE0420            YES - PROCESS ORDER                          
*                                                                               
*   TEST KEYS                                                                   
*        MVC   P+1(10),=C'SKIP: OFF '                                           
*        MVC   P+12(27),KEY                                                     
*        MVI   P+41,C'>'                                                        
*        MVC   P+42(2),QOFFICE                                                  
*        MVI   P+44,C'<'                                                        
*        GOTO1 REPORT                                                           
*   TEST KEYS EXIT                                                              
*                                                                               
SRCE0400 EQU   *                                                                
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     SRCE0240            RESTART ON NEXT KEY                          
*                                                                               
         DROP  R2                                                               
*                                                                               
SRCE0420 DS    0H                                                               
*                                                                               
*   TEST KEYS                                                                   
*        MVC   P+1(10),=C'GET CON   '                                           
*        MVC   P+12(27),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST KEYS EXIT                                                              
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETCON                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    SRCE0400               FOR DELETION                              
*                                                                               
*                                                                               
*                                                                               
         LA    R6,RCONELEM         FIND X'2C' ELEMENT                           
SRCE0450 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    SRCE0400            YES - SKIP THIS ORDER                        
*                                     SHOULD HAVE BEEN THERE                    
         CLI   0(R6),X'2A'         TAKEOVER/TAKENET?                            
         BE    SRCE0460            YES                                          
         CLI   0(R6),X'2C'         ALLHIST/BACKBILL?                            
         BE    SRCE0460            YES                                          
         ZIC   RE,1(R6)            NO                                           
         AR    R6,RE               BUMP TO NEXT ELEMENT                         
         B     SRCE0450            GO BACK FOR NEXT                             
SRCE0460 EQU   *                                                                
         CLI   0(R6),X'2A'         TAKEOVER/TAKENET?                            
         BNE   SRCE0470            NO                                           
         CLI   RCONTYPE,C'N'       NETWORK ORDER?                               
         BE    SRCE0465            YES - PROCESS IT                             
         CLI   RCONTYPE,C'X'       NETWORK ORDER?                               
         BNE   SRCE0400            NO  - SKIP TAKEOVER/NOT TAKENET              
SRCE0465 EQU   *                                                                
         MVC   P+65(13),=C'NETWORK ORDER'                                       
         B     SRCE0475                                                         
SRCE0470 EQU   *                                                                
         MVC   P+65(13),=C'ALLHIST ORDER'                                       
SRCE0475 EQU   *                                                                
         MVC   P+80(4),RCONKSTA    INSERT STATION CALL LETTERS                  
         MVI   P+84,C'-'           INSERT SEPARATOR                             
         MVC   P+85(1),RCONKSTA+4  INSERT MEDIA                                 
         USING RCON2CEL,R6                                                      
         LA    R5,COMPTABL                                                      
SRCE0480 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE?                                
         BE    SRCE0500            YES                                          
         CLC   RCON2COR,0(R5)      ORIG REP IN TABLE?                           
         BE    SRCE0520            YES                                          
         LA    R5,LCOMPTAB(R5)                                                  
         B     SRCE0480            GO BACK FOR NEXT                             
SRCE0500 EQU   *                                                                
         MVC   P+2(02),RCON2COR                                                 
         MVC   P+5(12),=C'UNIDENTIFIED'                                         
         B     SRCE0540                                                         
SRCE0520 EQU   *                                                                
         MVC   P+2(16),2(R5)       INSERT COMPANY NAME                          
SRCE0540 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RCON2COC,P+29,4,=C'TOG'                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,P+49,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         L     RF,RECCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,RECCOUNT                                                      
         B     SRCE0400            GO BACK FOR NEXT                             
SRCE0600 EQU   *                                                                
*                                                                               
*   TEST COUNT DISPLAY                                                          
*        EDIT  CONCOUNT,(4,P+13)                                                
*        MVC   P+1(11),=C'# CONTRACTS'                                          
*        GOTO1 REPORT                                                           
*   TEST COUNT DISPLAY END                                                      
*                                                                               
*                                                                               
*   TEST DUMP                                                                   
*        DC    H'0'                                                             
*   TEST DUMP END                                                               
*                                                                               
*                                  FINISHED WITH EXTRACT                        
         B     EXIT                                                             
         EJECT                                                                  
*                0 0         1                                                  
*                0.0.2.4.6.8.0.2.4.6.8.0                                        
COMPTABL DC    C'BLBLAIR           '                                            
LCOMPTAB EQU   *-COMPTABL                                                       
         DC    C'PVPETRY           '                                            
         DC    C'FNFOX TV          '                                            
         DC    C'AMEAGLE TV        '                                            
         DC    C'CQCONTINENTAL     '                                            
         DC    C'CRCHRISTAL RADIO  '                                            
         DC    C'EAEASTMAN RADIO   '                                            
         DC    C'GSKATZ INTERACTIVE'                                            
         DC    C'KFKATZ HISPANIC   '                                            
         DC    C'KUKATZ RADIO      '                                            
         DC    C'K3KRG             '                                            
         DC    C'K4KATZ SYNDICATION'                                            
         DC    C'K6DIMENSIONS      '                                            
         DC    C'MRKATZ TV         '                                            
         DC    C'PVPETRY           '                                            
         DC    C'NUCLEAR CHANNEL/KZ'                                            
         DC    C'QDSPORTS SPECTRUM '                                            
         DC    C'RSABC RADIO/KZ    '                                            
         DC    C'S3SENTRY RADIO    '                                            
         DC    C'AQALLIED RADIO    '                                            
         DC    C'B1TELEMUNDO       '                                            
         DC    C'CNCLEAR CHANNEL/IR'                                            
         DC    C'D4D+R RADIO       '                                            
         DC    C'IBABC RADIO/IR    '                                            
         DC    C'IFINFINITY RADIO  '                                            
         DC    C'I2NONREP          '                                            
         DC    C'I8CABALLERO       '                                            
         DC    C'I9SCHUBERT RADIO  '                                            
         DC    C'MGMCGAVERN GUILD  '                                            
         DC    C'NXPUBLIC NET RADIO'                                            
         DC    C'UNUNIVISION       '                                            
         DC    C'UOCUMULUS         '                                            
         DC    C'UVUNIVISION       '                                            
         DC    X'0000'                                                          
         DS    0H                                                               
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETBUY   LA    RF,RBUYREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
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
DM010    EQU   *                                                                
         TM    DMINBTS,X'08'       PASS BACK DELETED RECORDS?                   
         BO    MODEEXIT            YES                                          
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
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
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
TKWORK   EQU   *                                                                
SORTREC  DS    0CL65                                                            
STYP     DS    CL1   +0                                                         
*                                  TYPE 0  =  SOURCE CONTRACT                   
*                                  TYPE 1  =  TARGET CONTRACT                   
SOFFICE  DS    CL2   +1            OFFICE CODE                                  
SAGENCY  DS    CL4   +3            AGENCY CODE                                  
SAGYOFF  DS    CL2   +7            AGENCY OFFICE CODE                           
SOFFIC2  DS    CL2   +9            2ND OFFICE CODE:  IF AGY/OFF SEQ             
SADVERT  DS    CL4   +11           ADVERTISER CODE                              
SPRODUCT DS    CL20  +15           PRODUCT                                      
SCONTYPE DS    CL1   +35           CONTRACT TYPE                                
SSALPER  DS    CL3   +36           SALESPERSON                                  
SFLITEST DS    CL3   +39           FLIGHT START DATE                            
SFLITEND DS    CL3   +42           FLIGHT END   DATE                            
SDOLLARO DS    CL4   +45           CONTRACT VALUE:  OLD                         
SOLDCON# DS    CL4   +49           OLD CONTRACT NUMBER                          
SNEWCON# DS    CL4   +53           NEW CONTRACT NUMBER                          
SCMBOFL1 DS    CL1   +57           Y = COMBO ORDER: SOURCE                      
SCMBOFL2 DS    CL1   +58           Y = COMBO ORDER: TARGET                      
SDOLLARN DS    CL4   +59           CONTRACT VALUE:  NEW                         
         DS    CL2   +63           SPARE                                        
SRTRECLN EQU   *-SORTREC                                                        
*                                                                               
SORTREC2 DS    0CL65               SAVED SORTKEY                                
*                                                                               
TTYP     DS    CL1   +0                                                         
*                                  TYPE 0  =  SOURCE CONTRACT                   
*                                  TYPE 1  =  TARGET CONTRACT                   
TOFFICE  DS    CL2   +1            OFFICE CODE                                  
TAGENCY  DS    CL4   +3            AGENCY CODE                                  
TAGYOFF  DS    CL2   +7            AGENCY OFFICE CODE                           
TOFFIC2  DS    CL2   +9            2ND OFFICE CODE:  IF AGY/OFF SEQ             
TADVERT  DS    CL4   +11           ADVERTISER CODE                              
TPRODUCT DS    CL20  +15           PRODUCT                                      
TCONTYPE DS    CL1   +35           CONTRACT TYPE                                
TSALPER  DS    CL3   +36           SALESPERSON                                  
TFLITEST DS    CL3   +39           FLIGHT START DATE                            
TFLITEND DS    CL3   +42           FLIGHT END   DATE                            
TDOLLARO DS    CL4   +45           CONTRACT VALUE:  OLD                         
TOLDCON# DS    CL4   +49           OLD CONTRACT NUMBER                          
TNEWCON# DS    CL4   +53           NEW CONTRACT NUMBER                          
TCMBOFL1 DS    CL1   +57           Y = COMBO ORDER:  SOURCE                     
TCMBOFL2 DS    CL1   +58           Y = COMBO ORDER:  TARGET                     
TDOLLARN DS    CL4   +59           CONTRACT VALUE:  NEW                         
         DS    CL2   +63           SPARE                                        
*                                                                               
*                                                                               
COUNTUPS EQU   *                                                                
CONCOUNT DS    F                                                                
TKOCOUNT DS    F                                                                
AOFDOLLN DS    F                   AGENCY/OFF $$ (NEW)                          
AGYDOLLN DS    F                   AGENCY     $$ (NEW)                          
OFFDOLLN DS    F                   OFFICE     $$ (NEW)                          
FNLDOLLN DS    F                   FINAL      $$ (NEW)                          
AOFDOLLO DS    F                   AGENCY/OFF $$ (OLD)                          
AGYDOLLO DS    F                   AGENCY     $$ (OLD)                          
OFFDOLLO DS    F                   OFFICE     $$ (OLD)                          
FNLDOLLO DS    F                   FINAL      $$ (OLD)                          
LCOUNTUP EQU   *-COUNTUPS                                                       
*                                                                               
RECCOUNT DS    F                                                                
LASTAGY  DS    CL6                                                              
LASTOFF  DS    CL2                                                              
LASTADV  DS    CL4                                                              
*                                                                               
AGYNAME  DS    CL20                                                             
OFFNAME  DS    CL20                                                             
ADVNAME  DS    CL20                                                             
*                                                                               
SRCESAVE DS    CL6                 SOURCE REP NAME FROM REQUEST CARD            
SAVEVUTL DS    CL1                 ORIGINAL UTL NUMBER                          
NEWUTL   DS    CL1                 NEW UTL NUMBER                               
FNLTOTAL DS    CL1                                                              
*                                                                               
BEFFDATE DS    CL3                 BINARY EFFECTIVE DATE                        
CEFFDATE DS    CL2                 COMPRESSED EFFECTIVE DATE                    
SRCEREP  DS    CL2                 SOURCE REP                                   
CNFORVER DS    CL1                 CONFIRM OR VERSION FLAG                      
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
ACCBUY$$ DS    F                                                                
SVTOTSPT DS    F                                                                
SVTOTWKS DS    F                                                                
COMMAND  DS    CL8                                                              
AIO      DS    A                   USE CONTRACT RECORD AREA AS IO               
RELO     DS    F                   RELOCATION ADDRESS                           
*                                                                               
TKWORKLQ EQU   *-TKWORK                                                         
*                                                                               
* END OF REINITIALIZED WORKING STORAGE                                          
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*TL      DC    F'0',X'0A'          FOR CONTROL SYSTEM                           
***<<<                                                                          
*                                                                               
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE DDMASTD                                                        
PRINLIN  DSECT                                                                  
*                                                                               
         DS    CL1                                                              
DOFFICE  DS    CL2                 OFFICE CODE                                  
         DS    CL3                                                              
DAGENCY  DS    CL4                 AGENCY CODE                                  
         DS    CL1                                                              
DAGYOFF  DS    CL2                 AGENCY OFFICE CODE                           
         DS    CL2                                                              
DADVERT  DS    CL4                 ADVERTISER CODE                              
         DS    CL5                                                              
DPRODUCT DS    CL12                PRODUCT                                      
         DS    CL1                                                              
DCONTYPE DS    CL1                 CONTRACT TYPE                                
         DS    CL2                                                              
DSALPER  DS    CL3                 SALESPERSON                                  
         DS    CL3                                                              
DFLITEST DS    CL8                 FLIGHT START DATE                            
         DS    CL1                                                              
DFLITEND DS    CL8                 FLIGHT END   DATE                            
         DS    CL2                                                              
DOLDCON# DS    CL8                 OLD CONTRACT NUMBER                          
DOLDCMBO DS    CL1                 COMBO ORDER FLAG                             
         DS    CL2                                                              
DNEWCON# DS    CL8                 NEW CONTRACT NUMBER                          
DNEWCMBO DS    CL1                 COMBO ORDER FLAG                             
         DS    CL8                                                              
DDOLLARO DS    CL11                CONTRACT VALUE:  OLD                         
         DS    CL1                                                              
DDOLLARN DS    CL11                CONTRACT VALUE:  NEW                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089REREPAH02 05/01/02'                                      
         END                                                                    
