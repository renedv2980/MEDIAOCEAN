*          DATA SET REREP1H02B AT LEVEL 167 AS OF 05/01/02                      
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
* MAY30/96 (BU ) --- KATZ MISSING STATION SWEEP                     *           
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
*        DC    AL1(REQFRST),AL3(KZPURGE)  PURGE RECORDS FROM KZ                 
*        DC    AL1(REQFRST),AL3(B4PURGE)  PURGE RECORDS FROM B4                 
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(DATEDUMP) DATE ROUTINE(S) EXERCIZER             
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(DATEVAL)  DATVAL TEST                           
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(POSD)     UPDATE FORECAST BUCKETS               
*                                  ALL IO DONE FROM REQFRST                     
*        DC    AL1(REQFRST),AL3(KATZ)     KATZ STATION RECORD FILE              
*                                             MARKER                            
*        DC    AL1(REQFRST),AL3(KATZTAPE) KATZ STATION RECORD FILE              
*                                             MARKER                            
         DC    AL1(REQFRST),AL3(STASWEEP) KATZ MISSING STATION                  
*                                             SWEEP                             
*        DC    AL1(REQFRST),AL3(OFFSWEEP) KATZ MISSING OFFICE                   
*                                             SWEEP                             
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
*        DC    AL1(PROCCONT),AL3(POSA)    17 ELT SCAN                           
*        DC    AL1(PROCCONT),AL3(POSB)    CLOSED-OUT MONTHS: OPEN UP            
*        DC    AL1(PROCCONT),AL3(POSC)    DISPLAY COMBO CONTRACT #S             
*        DC    AL1(PROCCONT),AL3(POSE)    SPL SWEEP                             
*        DC    AL1(PROCCONT),AL3(BUYL)    REP TO SPOT TRANSFER SCAN             
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
*                                                                               
*   STASWEEP:  DETERMINE WHICH STATION RECORDS ARE MISSING.                     
*                                                                               
STASWEEP NTR1                                                                   
         LA    R1,REPLIST          SET A(LIST OF REPS)                          
STAS0000 EQU   *                                                                
         XC    LASTSTAT,LASTSTAT   CLEAR LAST STATION SEEN                      
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'CC'           INSERT RECORD TYPE                           
         MVC   KEY+1(2),0(R1)      INSERT REP CODE FROM LIST                    
STAS0010 EQU   *                                                                
         GOTO1 HIGH                READ FIRST RECORD                            
         B     STAS0040                                                         
STAS0020 EQU   *                                                                
         GOTO1 SEQ                                                              
STAS0040 EQU   *                                                                
         CLC   KEY(3),KEYSAVE      SAME RECORD TYPE/REP?                        
         BNE   STAS1000            NO  - FINISHED                               
         CLC   KEY+3(5),LASTSTAT   STATION ALREADY SEEN?                        
         BNE   STAS0050            YES - GO BACK FOR NEXT CONTRACT              
         MVI   KEY+8,X'FF'         BUMP TO NEXT STATION                         
         B     STAS0010            START NEXT STATION                           
STAS0050 EQU   *                                                                
         MVC   P+1(12),=C'NEW STATION:'                                         
         MVC   P+15(2),0(R1)       INSERT REP CODE                              
         MVI   P+17,C'/'                                                        
         MVC   P+18(05),KEY+3      INSERT STATION CALL LETTERS                  
         MVC   LASTSTAT,KEY+3      SAVE LAST STATION SEEN                       
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,2                                                            
         MVC   KEY+20(2),0(R1)     INSERT REP CODE                              
         MVC   KEY+22(5),LASTSTAT  INSERT STATION CALL LETTERS                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     STATION FOUND?                               
         BNE   STAS0055            YES                                          
         MVC   P+30(05),=C'FOUND'                                               
         B     STAS0060                                                         
STAS0055 EQU   *                                                                
         MVC   P+30(08),=C'MISSING '                                            
STAS0060 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   KEY,KEYSAV2                                                      
         B     STAS0010            GO BACK AND RESTART AT THIS KEY              
STAS1000 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'REP XX: STATIONS DONE'                                
         MVC   P+05(2),0(R1)                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         LA    R1,2(R1)            BUMP TO NEXT REP                             
         CLI   0(R1),X'FF'         END OF LIST?                                 
         BNE   STAS0000            NO  - GO BACK AND PROCESS NEXT REP           
*                                                                               
         XIT1                                                                   
*                                                                               
REPLIST  DC    C'BF'                                                            
         DC    C'KU'                                                            
         DC    C'KF'                                                            
         DC    C'EA'                                                            
         DC    C'CR'                                                            
         DC    C'K4'                                                            
         DC    C'S3'                                                            
         DC    C'RS'                                                            
         DC    C'K6'                                                            
         DC    X'FFFF'                                                          
         EJECT                                                                  
                                                                                
*                                                                               
*   OFFSWEEP:  DETERMINE WHICH OFFICE RECORDS ARE MISSING.                      
*                                                                               
OFFSWEEP NTR1                                                                   
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'9C'           INSERT RECORD TYPE                           
         MVC   KEY+2(2),RCREPFL    INSERT REP CODE                              
OFFC0010 EQU   *                                                                
         GOTO1 HIGH                READ FIRST RECORD                            
         B     OFFC0040                                                         
OFFC0020 EQU   *                                                                
         GOTO1 SEQ                                                              
OFFC0040 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE/REP?                        
         BNE   OFFC1000            NO  - FINISHED                               
         CLC   KEY+4(2),LASTOFF    OFFICE  ALREADY SEEN?                        
         BNE   OFFC0050            YES - GO BACK FOR NEXT CONTRACT              
         MVI   KEY+6,X'FF'         BUMP TO NEXT OFFICE                          
         B     OFFC0010            START NEXT OFFICE                            
OFFC0050 EQU   *                                                                
         MVC   P+1(12),=C'NEW OFFICE :'                                         
         MVC   P+15(02),KEY+4                                                   
         GOTO1 REPORT                                                           
         MVC   LASTOFF,KEY+4       SAVE LAST OFFICE  SEEN                       
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,4                                                            
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+25(2),LASTOFF   INSERT OFFICE                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     OFFICE  FOUND?                               
         BNE   OFFC0055            YES                                          
         MVC   P+1(16),=C'OFFICE  FOUND  :'                                     
         MVC   P+20(5),LASTOFF                                                  
         GOTO1 REPORT                                                           
         B     OFFC0060                                                         
OFFC0055 EQU   *                                                                
         MVC   P+1(16),=C'OFFICE  MISSING:'                                     
         MVC   P+20(5),LASTOFF                                                  
         GOTO1 REPORT                                                           
OFFC0060 EQU   *                                                                
         MVC   KEY,KEYSAV2                                                      
         B     OFFC0010            GO BACK AND RESTART AT THIS KEY              
OFFC1000 EQU   *                                                                
         MVC   P+1(17),=C'ALL OFFICES  DONE'                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
                                                                                
*                                                                               
*   POST:  INSPECT X'06' ELEMENTS FOR STATION=WOOD (RUN FILTERED                
*     ON 'WOTV'.  IF FOUND, LIST CONTRACT NUMBER, CHANGE 'WOOD' TO              
*     'WOTV'.                                                                   
*                                                                               
POST     NTR1                                                                   
*                                                                               
         LA    R2,RCONELEM         FIND X'06' ELEMENT                           
POST0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    MODEEXIT            YES - NO X'06' ELEMENT                       
         CLI   0(R2),X'06'                                                      
         BNE   POST0010            NOT X'06' ELEMENT                            
         CLC   =C'WOOD',9(R2)      X'06' FOUND - STATION = 'WOOD'?              
         BNE   MODEEXIT            NO  - EXIT                                   
         CLI   8(R2),1             HOW MANY MINI-ELEMENTS?                      
         BH    MODEEXIT            MORE THAN 1                                  
         MVC   9(4,R2),=C'WOTV'    YES - INSERT 'WOTV'                          
         MVC   P+5(10),=C'CONTRACT= '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   MODEEXIT            NO                                           
         BAS   RE,PUTCON           REWRITE RECORD                               
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   B4PURGE:  QUESTOR BYTES INDICATE WHICH RECORDS TO PURGE.                    
*        QUESTOR+0  =  PURGE STATIONS                                           
*        QUESTOR+1  =  PURGE SALESPERSONS                                       
*        QUESTOR+2  =  PURGE AGENCY RECORDS                                     
*        QUESTOR+3  =  PURGE ADVERTISER RECORDS                                 
*        QUESTOR+4  =  PURGE CONTRACTS + BUYLINES                               
*      KEYS ARE RETRIEVED FOR REP, AND CONTROL BYTE IS SET TO                   
*        DELETE, AND KEY REWRITTEN.  DUMP WILL THEREAFTER BYPASS                
*        RECORD.                                                                
*                                                                               
B4PURGE  NTR1                                                                   
*                                                                               
         CLI   QUESTOR+0,C'Y'      DELETE STATIONS?                             
         BNE   B4PU0100            NO                                           
         MVC   P+1(25),=C'STARTING STATION DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,2               SET TYPE FOR STATION                         
         MVC   KEY+20(2),=C'B4'    INSERT CODE B4                               
         GOTO1 HIGH                START READ                                   
         B     B4PU0020                                                         
B4PU0010 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0020 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   B4PU0100            NO  - FINISHED STATIONS                      
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0010            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0010            GO BACK FOR NEXT RECORD                      
B4PU0100 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DELETE SALESPERSONS?                         
         BNE   B4PU0200            NO                                           
         MVC   P+1(25),=C'STARTING S/P     DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,6               SET TYPE FOR SALESPERSON                     
         MVC   KEY+22(2),=C'B4'    INSERT CODE B4                               
         GOTO1 HIGH                START READ                                   
         B     B4PU0120                                                         
B4PU0110 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0120 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   B4PU0200            NO  - FINISHED SALESPERSONS                  
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0110            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0110            GO BACK FOR NEXT RECORD                      
B4PU0200 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DELETE AGENCY RECORDS?                       
         BNE   B4PU0300            NO                                           
         MVC   P+1(25),=C'STARTING AGENCY  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0A'           SET TYPE FOR AGENCY RECORDS                  
         GOTO1 HIGH                START READ                                   
         B     B4PU0220                                                         
B4PU0210 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0220 EQU   *                                                                
         CLI   KEY,X'0A'           AGENCY RECORD?                               
         BNE   B4PU0250            NO  - FINISHED AGENCY 0A RECORDS             
         CLC   KEY+25(2),=C'B4'    B4 REP?                                      
         BNE   B4PU0210            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0210            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0210            GO BACK FOR NEXT RECORD                      
B4PU0250 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'1A'           SET TYPE FOR AGENCY 2NDARY RECORDS           
         GOTO1 HIGH                START READ                                   
         B     B4PU0270                                                         
B4PU0260 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0270 EQU   *                                                                
         CLI   KEY,X'1A'           AGENCY RECORD?                               
         BNE   B4PU0300            NO  - FINISHED AGENCY 1A RECORDS             
         CLC   KEY+25(2),=C'B4'    B4 REP?                                      
         BNE   B4PU0260            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0260            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0260            GO BACK FOR NEXT RECORD                      
B4PU0300 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DELETE ADVERTISER RECORDS?                   
         BNE   B4PU0400            NO                                           
         MVC   P+1(25),=C'STARTING ADVERT  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,8               SET TYPE FOR ADVERTISER                      
         GOTO1 HIGH                START READ                                   
         B     B4PU0320                                                         
B4PU0310 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0320 EQU   *                                                                
         CLI   KEY,8               ADVERTISER RECORD?                           
         BNE   B4PU0400            NO  - FINISHED ADVERTS                       
         CLC   KEY+25(2),=C'B4'    SAME REP?                                    
         BNE   B4PU0310            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0310            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0310            GO BACK FOR NEXT RECORD                      
B4PU0400 LTR   R0,R0                                                            
         CLI   QUESTOR+4,C'Y'      DELETE CON/BUY RECORDS?                      
         BNE   B4PU0500            NO                                           
         MVC   P+1(25),=C'STARTING CON/BUY DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0C'           SET TYPE FOR CONTRACT                        
         MVC   KEY+2(2),=C'B4'     INSERT  REP CODE                             
         GOTO1 HIGH                START READ                                   
         B     B4PU0420                                                         
B4PU0410 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0420 EQU   *                                                                
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   B4PU0450            NO  - FINISHED CONTRACTS                     
         CLC   KEY+2(2),=C'B4'     SAME REP?                                    
         BNE   B4PU0450            NO  - FINISHED CONTRACTS                     
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0410            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0410            GO BACK FOR NEXT RECORD                      
B4PU0450 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0B'           SET TYPE FOR BUYLINES                        
         MVC   KEY+16(2),=C'B4'    INSERT  REP CODE                             
         GOTO1 HIGH                START READ                                   
         B     B4PU0470                                                         
B4PU0460 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0470 EQU   *                                                                
         CLI   KEY,X'0B'           CONTRACT RECORD?                             
         BNE   B4PU0500            NO  - FINISHED BUYLINES                      
         CLC   KEY+16(2),=C'B4'    NO  - FINISHED                               
         BNE   B4PU0500            NO  - NOT RIGHT REP - FINISHED               
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0460            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0460            GO BACK FOR NEXT RECORD                      
B4PU0500 LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 5                                                                
B4PRINT  NTR1                                                                   
         CLI   QOPTION2,C'P'       PRINT DETAILS?                               
         BNE   B4PR0020            NO                                           
         MVC   P+1(34),KEY         SHOW PRE-KEY                                 
         GOTO1 REPORT                                                           
B4PR0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   KZPURGE:  QUESTOR BYTES INDICATE WHICH RECORDS TO PURGE.                    
*        QUESTOR+0  =  PURGE STATIONS                                           
*        QUESTOR+1  =  PURGE SALESPERSONS                                       
*        QUESTOR+2  =  PURGE AGENCY RECORDS                                     
*        QUESTOR+3  =  PURGE ADVERTISER RECORDS                                 
*        QUESTOR+4  =  PURGE CONTRACTS + BUYLINES                               
*      KEYS ARE RETRIEVED FOR REP, AND CONTROL BYTE IS SET TO                   
*        DELETE, AND KEY REWRITTEN.  DUMP WILL THEREAFTER BYPASS                
*        RECORD.                                                                
*      THIS RUN WILL PURGE ALL CONTRACTS AND BUYS FOR ALL KATZ COS.             
*                                                                               
KZPURGE  NTR1                                                                   
*                                                                               
         CLI   QUESTOR+4,C'Y'      DELETE CON/BUY RECORDS?                      
         BNE   KZPU0600            NO                                           
         LA    R3,KATZCMPS         A(KATZ COMPANY TABLE)                        
KZPU0400 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    KZPU0600            YES - FINISHED                               
         MVC   P+1(37),=C'STARTING CON/BUY DELETION FOR COMPANY'                
         MVC   P+40(2),0(R3)                                                    
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0C'           SET TYPE FOR CONTRACT                        
         MVC   KEY+2(2),0(R3)      INSERT  REP CODE                             
         GOTO1 HIGH                START READ                                   
         B     KZPU0420                                                         
KZPU0410 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
KZPU0420 EQU   *                                                                
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   KZPU0450            NO  - FINISHED CONTRACTS                     
         CLC   KEY+2(2),0(R3)      SAME REP?                                    
         BNE   KZPU0450            NO  - FINISHED CONTRACTS                     
         BAS   RE,KZPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,KZPRINT                                                       
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,CONCTR2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR2                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    KZPU0410            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     KZPU0410            GO BACK FOR NEXT RECORD                      
KZPU0450 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0B'           SET TYPE FOR BUYLINES                        
         MVC   KEY+16(2),0(R3)     INSERT  REP CODE                             
         GOTO1 HIGH                START READ                                   
         B     KZPU0470                                                         
KZPU0460 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
KZPU0470 EQU   *                                                                
         CLI   KEY,X'0B'           CONTRACT RECORD?                             
         BNE   KZPU0500            NO  - FINISHED BUYLINES                      
         CLC   KEY+16(2),0(R3)     NO  - FINISHED                               
         BNE   KZPU0500            NO  - NOT RIGHT REP - FINISHED               
         BAS   RE,KZPRINTB                                                      
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,KZPRINTB                                                      
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         L     RF,BUYCTR2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR2                                                       
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    KZPU0460            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     KZPU0460            GO BACK FOR NEXT RECORD                      
KZPU0500 EQU   *                                                                
         MVC   P+1(08),=C'COMPANY:'                                             
         MVC   P+12(2),0(R3)       INSERT COMPANY                               
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'CONTRACTS DELETED:'                                   
         EDIT  CONCTR,(12,P+24)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'BUYLINES  DELETED:'                                   
         EDIT  BUYCTR,(12,P+24)                                                 
         GOTO1 REPORT                                                           
         XC    CONCTR(8),CONCTR    CLEAR COUNTERS                               
         LA    R3,2(R3)            BUMP TO NEXT COMPANY                         
         B     KZPU0400            GO BACK FOR NEXT COMPANY                     
KZPU0600 EQU   *                                                                
         MVC   P+1(18),=C'CONTRACT  TOTAL  :'                                   
         EDIT  CONCTR2,(12,P+24)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'BUYLINE   TOTAL  :'                                   
         EDIT  BUYCTR2,(12,P+24)                                                
         GOTO1 REPORT                                                           
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 5                                                                
KATZCMPS DC    C'BF'               BANNER                                       
         DC    C'CR'               CHRISTAL                                     
         DC    C'EA'               EASTMAN                                      
         DC    C'KF'               KATZ RADIO                                   
         DC    C'KU'               KATZ HISPANIC                                
         DC    C'K4'               SYNDICATED                                   
         DC    H'0000'             DELIMITER                                    
         SPACE 5                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
CONCTR2  DS    F                                                                
BUYCTR2  DS    F                                                                
KZPRINT  NTR1                                                                   
         CLI   QOPTION2,C'P'       PRINT DETAILS?                               
         BNE   KZPR0020            NO                                           
         CLC   CONCTR,=F'50'       PRINT FIRST 50 KEYS                          
         BH    KZPR0020            HIGH - SKIP IT                               
         MVC   P+1(34),KEY         SHOW PRE-KEY                                 
         GOTO1 REPORT                                                           
KZPR0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
KZPRINTB NTR1                                                                   
         CLI   QOPTION2,C'P'       PRINT DETAILS?                               
         BNE   KZPB0020            NO                                           
         CLC   BUYCTR,=F'50'       PRINT FIRST 50 KEYS                          
         BH    KZPB0020            HIGH - SKIP IT                               
         MVC   P+1(34),KEY         SHOW PRE-KEY                                 
         GOTO1 REPORT                                                           
KZPB0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR TYPE N/X ORDERS.  WHEN FOUND, SCAN BUYS FOR              
*      X'08' ELEMENTS WITHOUT A TRANSFER DATE.  IF FOUND, DISPLAY               
*      CONTRACT NUMBER                                                          
*                                                                               
*                                                                               
BUYL     NTR1                                                                   
*                                                                               
         CLI   RCONTYPE,C'N'       REP TO SPOT ORDER?                           
         BE    BUYL0020            YES                                          
         CLI   RCONTYPE,C'X'       REP TO SPOT ORDER?                           
         BNE   BUYL0800            NO  - SKIP THIS ORDER                        
BUYL0020 EQU   *                                                                
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,11              INSERT RECORD TYPE                           
         MVC   KEY+16,RCREPFL      INSERT REP CODE                              
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+18(4),WORK+15                                                
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
*        MVC   P+1(10),=C'BUYLINE = '                                           
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
***      GOTO1 HEXOUT,DMCB,KEY+18,P+25,4,=C'TOG'                                
**       GOTO1 REPORT                                                           
*                                                                               
         LA    R5,RBUYREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         GOTO1 HIGH                                                             
         B     BUYL0060                                                         
BUYL0040 EQU   *                                                                
         GOTO1 SEQ                                                              
BUYL0060 EQU   *                                                                
         CLC   KEY(22),KEYSAVE                                                  
         BNE   BUYL0700            FINISHED - RELOAD CONTRACT                   
         GOTO1 GREC                                                             
         TM    RBUYCOMB,X'80'      COMBO PLACE HOLDER?                          
         BO    BUYL0040            YES - SKIP THE RECORD                        
         LA    R6,RBUYELEM                                                      
BUYL0070 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BUYL0040            YES - GO GET NEXT RECORD                     
         CLI   0(R6),8             SPOT INTERFACE ELEMENT?                      
         BE    BUYL0080            YES - CHECK DATE                             
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               NO  - BUMP TO NEXT ELEMENT                   
         B     BUYL0070            GO BACK FOR NEXT                             
BUYL0080 EQU   *                                                                
         USING RBUYSPEL,R6                                                      
         OC    RBUYSPDT,RBUYSPDT   ANY DATE TRANSFERRED?                        
         BNZ   BUYL0040            YES - GO BACK FOR NEXT RECORD                
         DROP  R6                                                               
         PRINT GEN                                                              
         MVC   P+1(12),=C'NO XFER DATE'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
***>>>   MVC   P+30(40),0(R6)                                                   
         GOTO1 REPORT                                                           
         B     BUYL0750                                                         
BUYL0700 EQU   *                                                                
***      MVC   P+1(16),=C'CONTRACT CORRECT'                                     
***      GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
***      GOTO1 REPORT                                                           
         PRINT NOGEN                                                            
BUYL0750 EQU   *                                                                
         MVC   KEY,KEYSAV2         RESTORE CONTRACT KEY                         
         GOTO1 READ                READ THE CONTRACT RECORD                     
BUYL0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   READ KATZ 'TAPE' FILE.  ADD INFORMATION TO EACH OF THE  TRAFFIC             
*      SYSTEM CODE OF 'A', AND ADD AN X'05' ELEMENT FOR GRAPH.                  
*                                                                               
*                                                                               
KATZTAPE NTR1                                                                   
         OPEN  (KZFILE,(INPUT))                                                 
         LA    R5,RSTAREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
*                                                                               
KATP0020 EQU   *                                                                
         GET   KZFILE              READ RECORD                                  
         L     RF,KATZCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,KATZCTR                                                       
         MVC   RECAREA(80),0(R1)   MOVE RECORD TO AREA                          
         GOTO1 REPORT                                                           
         MVC   P+1(80),RECAREA                                                  
         GOTO1 REPORT                                                           
         LA    R2,KATPCOS                                                       
KATP0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BNE   KATP0050            YES - NO COMPANY: SKIP IT                    
         MVC   P+1(17),=C'COMPANY NOT FOUND'                                    
         GOTO1 REPORT                                                           
         L     RF,BADKATZ                                                       
         LA    RF,1(RF)                                                         
         ST    RF,BADKATZ                                                       
         B     KATP0020            GO BACK FOR NEXT                             
KATP0050 EQU   *                                                                
         CLC   0(1,R2),PLDIVISN    NO  - FIND COMPANY IN TABLE                  
         BE    KATP0060            FOUND                                        
         LA    R2,3(R2)            BUMP TO NEXT ENTRY                           
         B     KATP0040            GO BACK FOR NEXT                             
KATP0060 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY FOR STATION READ                   
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),1(R2)     INSERT REP CODE                              
         MVC   KEY+22(5),PLSTAT    INSERT STATION + MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     STATION RECORD FOUND?                        
         BE    KATP0080            YES                                          
         MVC   P+1(18),=C'STATION NOT FOUND'                                    
         MVC   P+20(4),PLSTAT                                                   
         MVC   P+25(2),PLAMFM                                                   
         MVC   P+28(1),PLDIVISN                                                 
         GOTO1 REPORT                                                           
         L     RF,STAMISSD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAMISSD                                                      
         B     KATP0020            NO  - SKIP THIS RECORD                       
KATP0080 EQU   *                                                                
         GOTO1 GREC                YES - READ THE RECORD                        
         CLC   KATZCTR,=F'200'                                                  
         BH    KATP0085                                                         
         MVC   P+1(26),=C'STATION RECORD: PRE-UPDATE'                           
         EDIT  KATZCTR,(8,P+32)                                                 
         GOTO1 REPORT                                                           
         ZICM  RF,RSTALEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RSTAREC),RSTAREC,C'DUMP',(RF),=C'2D'          
KATP0085 EQU   *                                                                
         CLC   PLOWNERD,SPACES     ANY VALUE IN OWNER?                          
         BE    KATP0120            NO                                           
         CLC   RSTAOWN,SPACES      ANY ENTRY IN OWNER?                          
         BE    KATP0100            NO  - INSERT OWNER                           
         OC    RSTAOWN,RSTAOWN     ANY ENTRY IN OWNER?                          
         BNZ   KATP0120            YES                                          
KATP0100 EQU   *                                                                
         MVC   RSTAOWN,PLOWNERD    INSERT REPLACEMENT OWNER                     
KATP0120 EQU   *                                                                
         CLC   PLREGION,SPACES     ANY VALUE IN TVB REGION?                     
         BE    KATP0160            NO                                           
         CLC   RSTATVB,SPACES      ANY ENTRY IN TVB REGION?                     
         BE    KATP0140            NO  - INSERT TVB REGION                      
         OC    RSTATVB,RSTATVB     ANY ENTRY IN TVB REGION?                     
         BNZ   KATP0160            YES                                          
KATP0140 EQU   *                                                                
         MVC   RSTATVB,PLREGION    INSERT REPLACEMENT TVB REGION                
KATP0160 EQU   *                                                                
         CLC   PLFREQ,SPACES       ANY VALUE IN CHANNEL NUMBER?                 
         BE    KATP0195            NO                                           
         CLC   RSTACHAN,SPACES     ANY ENTRY IN CHAN#?                          
         BE    KATP0180            NO  - INSERT CHAN#                           
         OC    RSTACHAN,RSTACHAN   ANY ENTRY IN CHAN#?                          
         BNZ   KATP0195            YES                                          
KATP0180 EQU   *                                                                
         LA    RF,PLFREQ                                                        
         SR    RE,RE                                                            
         LA    R0,4                                                             
KATP0182 EQU   *                                                                
         CLI   0(RF),C' '          SPACE CHARACTER?                             
         BE    KATP0184            YES                                          
         LA    RE,1(RE)                                                         
KATP0184 EQU   *                                                                
         LA    RF,1(RF)                                                         
         BCT   R0,KATP0182         GO BACK FOR NEXT                             
         BCTR  RE,0                BACK DOWN 1 FOR EX                           
         EX    RE,KATP0190                                                      
         B     KATP0192                                                         
KATP0190 PACK  DUB(8),PLFREQ(0)                                                 
KATP0192 EQU   *                                                                
         CVB   RF,DUB              CONVERT TO BINARY                            
         STCM  RF,3,RSTACHAN       INSERT REPLACEMENT FREQUENCY                 
KATP0195 EQU   *                                                                
         LA    R6,RSTAELEM                                                      
KATP0200 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    KATP0400            YES                                          
         CLI   0(R6),8             EXTENDED DESC ELT?                           
         BE    KATP0210            YES                                          
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     KATP0200            GO BACK FOR NEXT                             
KATP0210 EQU   *                                                                
         USING RSTAXXEL,R6                                                      
         CLC   PLMKTABR,SPACES     ANY VALUE IN MARKET ABBREV?                  
         BE    KATP0240            NO                                           
         CLC   RSTAMKTC,SPACES     ANY ENTRY IN MARKET ABBR?                    
         BE    KATP0220            NO  - INSERT MARKET ABBR?                    
         OC    RSTAMKTC,RSTAMKTC   ANY ENTRY IN MARKET ABBR?                    
         BNZ   KATP0240            YES                                          
KATP0220 EQU   *                                                                
         MVC   RSTAMKTC,PLMKTABR   INSERT REPLACEMENT MARKET ABBR               
KATP0240 EQU   *                                                                
         CLC   PLTIMZON,SPACES     ANY VALUE IN TIME ZONE?                      
         BE    KATP0280            NO                                           
         CLC   RSTATZ,SPACES       ANY ENTRY IN TIME ZONE?                      
         BE    KATP0260            NO  - INSERT TIME ZONE?                      
         OC    RSTATZ,RSTATZ       ANY ENTRY IN TIME ZONE?                      
         BNZ   KATP0280            YES                                          
KATP0260 EQU   *                                                                
         MVC   RSTATZ(1),PLTIMZON  INSERT REPLACEMENT TIME ZONE                 
KATP0280 EQU   *                                                                
         CLC   PLFAX,SPACES        ANY VALUE IN FAX?                            
         BE    KATP0320            NO                                           
         CLC   RSTAOFAX,SPACES     ANY ENTRY IN FAX?                            
         BE    KATP0300            NO  - INSERT FAX?                            
         OC    RSTAOFAX,RSTAOFAX   ANY ENTRY IN FAX?                            
         BNZ   KATP0320            YES                                          
KATP0300 EQU   *                                                                
         MVC   RSTAOFAX,PLFAX      INSERT REPLACEMENT FAX?                      
KATP0320 EQU   *                                                                
         B     KATP0440                                                         
KATP0400 EQU   *                                                                
         MVC   P+1(26),=C'NO EXTENDED DESCRIPTOR ELT'                           
         GOTO1 REPORT                                                           
KATP0440 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   KATP0480            NO                                           
         BAS   RE,PUTSTA           REWRITE RECORD                               
KATP0480 EQU   *                                                                
         L     RF,STAUPDTD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAUPDTD                                                      
         CLC   KATZCTR,=F'200'                                                  
         BH    KATP0020            GO BACK FOR NEXT                             
         MVC   P+1(23),=C'STATION RECORD: UPDATED'                              
         GOTO1 REPORT                                                           
         ZICM  RF,RSTALEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RSTAREC),RSTAREC,C'DUMP',(RF),=C'2D'          
         B     KATP0020            GO BACK FOR NEXT                             
KATP0900 EQU   *                                                                
         CLOSE KZFILE                                                           
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
KATPCOS  DC    C'BBF'                                                           
         DC    C'KKU'                                                           
         DC    C'HKF'                                                           
         DC    C'EEA'                                                           
         DC    C'CCR'                                                           
         DC    C'SK4'                                                           
         DC    C'*K3'                                                           
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
KZFILE   DCB   DDNAME=KZFILE,DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=8000,  X        
               MACRF=(GL,PM),EODAD=KATP0900                                     
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
LASTOFF  DS    CL2                                                              
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
**PAN#1  DC    CL21'167REREP1H02B05/01/02'                                      
         END                                                                    
