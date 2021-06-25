*          DATA SET REREP1H04  AT LEVEL 015 AS OF 05/01/02                      
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
* FEB01/93 (BU ) --- INITIAL ENTRY:  WOTV/WOOD FIX                  *           
*                                                                   *           
* OCT01/93 (BU ) --- REDO FORECAST BUCKETS TO BDCST MONTH BASIS     *           
*                                                                   *           
* SEP12/94 (BU ) --- SPL SWEEP FOR 0% AND CONTRACT DOLLARS          *           
*                                                                   *           
* JUN13/95 (BU ) --- BUYLINE SWEEP FOR MISSING TRANSFER EXAMPLES    *           
*                                                                   *           
* JUN21/95 (BU ) --- ADD DATVAL TEST                                *           
*                                                                   *           
* AUG24/95 (BU ) --- ADD RECORD PURGE FOR BIL (B4) RECORDS.         *           
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
*        DC    AL1(REQFRST),AL3(B4PURGE)  PURGE RECORDS FROM B4                 
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(DATEDUMP) DATE ROUTINE(S) EXERCIZER             
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(DATEVAL)  DATVAL TEST                           
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*        DC    AL1(REQFRST),AL3(POSD)     UPDATE FORECAST BUCKETS               
*                                  ALL IO DONE FROM REQFRST                     
*        DC    AL1(REQFRST),AL3(FIXINV)   FIX INVENTORY RECORDS                 
*                                  ALL IO DONE FROM REQFRST                     
         DC    AL1(REQFRST),AL3(SWEEPSTA) SWEEP STATION RECORDS                 
*                                  ALL IO DONE FROM REQFRST                     
*                                                                               
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
*        QUESTOR+5  =  PURGE MARKET RECORDS                                     
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
         MVC   KEY+20(2),QRECORD+20      INSERT REP CODE                        
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
         L     RF,STACTR+16                                                     
         LA    RF,1(RF)            INCREMENT STATION COUNT                      
         ST    RF,STACTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0010            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0010            GO BACK FOR NEXT RECORD                      
B4PU0100 EQU   *                                                                
         CLI   QUESTOR+5,C'Y'      DELETE MARKETS?                              
         BNE   B4PU0150            NO                                           
         MVC   P+1(25),=C'STARTING MARKET  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'2B'           SET TYPE FOR MARKET                          
         MVC   KEY+21(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     B4PU0120                                                         
B4PU0110 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0120 EQU   *                                                                
         CLC   KEY(23),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   B4PU0150            NO  - FINISHED MARKETS                       
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,MKTCTR+16                                                     
         LA    RF,1(RF)            INCREMENT MARKET  COUNT                      
         ST    RF,MKTCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0110            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0110            GO BACK FOR NEXT RECORD                      
B4PU0150 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DELETE SALESPERSONS?                         
         BNE   B4PU0200            NO                                           
         MVC   P+1(25),=C'STARTING S/P     DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,6               SET TYPE FOR SALESPERSON                     
         MVC   KEY+22(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     B4PU0170                                                         
B4PU0160 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0170 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   B4PU0200            NO  - FINISHED SALESPERSONS                  
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,SLSCTR+16                                                     
         LA    RF,1(RF)            INCREMENT S/P     COUNT                      
         ST    RF,SLSCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0160            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0160            GO BACK FOR NEXT RECORD                      
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
         CLC   KEY+25(2),QRECORD+20      SAME REP?                              
         BNE   B4PU0210            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,AGYCTR+16                                                     
         LA    RF,1(RF)            INCREMENT AGENCY  COUNT                      
         ST    RF,AGYCTR+16                                                     
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
         CLC   KEY+25(2),QRECORD+20     SAME REP?                               
         BNE   B4PU0260            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,AGYCTR+16                                                     
         LA    RF,1(RF)            INCREMENT AGENCY  COUNT                      
         ST    RF,AGYCTR+16                                                     
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
         CLC   KEY+25(2),QRECORD+20      SAME REP?                              
         BNE   B4PU0310            NO  - NOT RIGHT REP                          
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,ADVCTR+16                                                     
         LA    RF,1(RF)            INCREMENT ADVERT  COUNT                      
         ST    RF,ADVCTR+16                                                     
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
         MVC   KEY+2(2),QRECORD+20                                              
         GOTO1 HIGH                START READ                                   
         B     B4PU0420                                                         
B4PU0410 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0420 EQU   *                                                                
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   B4PU0450            NO  - FINISHED CONTRACTS                     
         CLC   KEY+2(2),QRECORD+20       SAME REP?                              
         BNE   B4PU0450            NO  - FINISHED CONTRACTS                     
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,CONCTR+16                                                     
         LA    RF,1(RF)            INCREMENT CON/BUY COUNT                      
         ST    RF,CONCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0410            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0410            GO BACK FOR NEXT RECORD                      
B4PU0450 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0B'           SET TYPE FOR BUYLINES                        
         MVC   KEY+16(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     B4PU0470                                                         
B4PU0460 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
B4PU0470 EQU   *                                                                
         CLI   KEY,X'0B'           BUYLINE  RECORD?                             
         BNE   B4PU0500            NO  - FINISHED BUYLINES                      
         CLC   KEY+16(2),QRECORD+20                                             
         BNE   B4PU0500            NO  - NOT RIGHT REP - FINISHED               
         BAS   RE,B4PRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,B4PRINT                                                       
         L     RF,CONCTR+16                                                     
         LA    RF,1(RF)            INCREMENT CON/BUY COUNT                      
         ST    RF,CONCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    B4PU0460            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     B4PU0460            GO BACK FOR NEXT RECORD                      
B4PU0500 EQU   *                                                                
         LA    R1,STACTR                                                        
B4PU0600 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    B4PU0700            YES - FINISHED                               
         MVC   P+1(16),0(R1)       MOVE TABLE DESCRIPTION                       
         L     R2,16(R1)           LOAD COUNT                                   
         EDIT  (R2),(8,P+20)                                                    
         GOTO1 REPORT                                                           
         LA    R1,20(R1)                                                        
         B     B4PU0600            GO BACK FOR NEXT                             
B4PU0700 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 5                                                                
B4PRINT  NTR1                                                                   
         CLI   QOPTION2,C'P'       PRINT DETAILS?                               
         BNE   B4PR0020            NO                                           
         MVC   P+1(34),KEY         SHOW PRE-KEY                                 
         GOTO1 REPORT                                                           
B4PR0020 EQU   *                                                                
         XIT1                                                                   
         DS   0F                                                                
*               1.3.5.7.9.1.3.5.7.9                                             
STACTR   DC   C'STATIONS PURGED '                                               
         DC   F'0'                                                              
SLSCTR   DC   C'SALESPNS PURGED '                                               
         DC   F'0'                                                              
AGYCTR   DC   C'AGENCIES PURGED '                                               
         DC   F'0'                                                              
ADVCTR   DC   C'ADVERTS  PURGED '                                               
         DC   F'0'                                                              
CONCTR   DC   C'CON/BUYS PURGED '                                               
         DC   F'0'                                                              
MKTCTR   DC   C'MARKETS  PURGED '                                               
         DC   F'0'                                                              
         DC   F'0'                 DELIMITER                                    
         EJECT                                                                  
*                                                                               
*   FIXINV:  FIX UTS INVENTORY AFTER CONVERT                                    
*                                                                               
FIXINV   NTR1                                                                   
*                                                                               
         LA    RE,KEY                                                           
         USING RINVREC,RE                                                       
         XC    KEY,KEY             YES                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP(2),=CL2'UT'                                             
         GOTO1 HIGH                START READ                                   
         CLC   KEY(12),KEYSAVE                                                  
         BE    FXINV100                                                         
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
FXINV050 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   FXINVEX                                                          
*                                                                               
FXINV100 EQU   *                                                                
         CLI   KEY+RINVKSRC-RINVREC,0    BYPASS HEADERS                         
         BE    FXINV050                                                         
*                                                                               
         MVC   KEYSAV2,KEY                                                      
*                                                                               
         LA    R5,IO1                                                           
         ST    R5,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
*  BUILD AND READ THE FU RECORD                                                 
*  R5 = A(UT RECORD)                                                            
*                                                                               
         MVC   KEY+RINVKREP-RINVREC(2),=CL2'FU'                                 
         MVC   KEY+RINVKINV-RINVREC(3),RINVOINV-RINVREC(R5)                     
         MVI   KEY+RINVKINV+3-RINVREC,X'00'                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FXINVBYP                                                         
         LA    R6,IO2                                                           
         ST    R6,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
*  UPDATE THE UT RECORD                                                         
*  R5 = A(UT RECORD)    R6 = A(FU RECORD)                                       
*                                                                               
         MVC   RINVOINV-RINVREC(3,R5),RINVOINV-RINVREC(R6)                      
*                                                                               
*  RE-READ UT RECORD                                                            
*                                                                               
         MVC   KEY(27),KEYSAV2                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD MUST EXIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,IO3                                                           
         ST    RE,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
*  WRITE THE UT RECORD OUT                                                      
*                                                                               
*        CLI   QOPTION2,C'T'       TEST RUN?                                    
*        BE    FXINV300            YES - DON'T REWRITE IT                       
         LA    RE,IO1              YES                                          
         ST    RE,AIOAREA          SET TYPE FOR MARKET                          
         GOTO1 PREC                                                             
*                                                                               
*  PRINT FIRST 200 CHANGES GET NEXT RECORD                                      
*                                                                               
FXINV300 CLC   COUNT,=F'200'                                                    
         BH    FXINV050                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',IO3,C'DUMP',80,=C'1D'                 
         GOTO1 =V(PRNTBL),DMCB,=C'FUREC',IO2,C'DUMP',80,=C'1D'                  
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',IO1,C'DUMP',80,=C'1D'                  
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     FXINV050                                                         
*                                                                               
*  BYPASS THIS RECORD NO MATCH ON OLD FILE                                      
*                                                                               
FXINVBYP MVC   KEY(27),KEYSAV2                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD MUST EXIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FXINV050                                                         
*                                                                               
FXINVEX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SWEEPSTA: IDENTIFY STATIONS NOT ON FILE                                     
*                                                                               
SWEEPSTA NTR1                                                                   
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'CC'           INSERT STATION-HIGH RECORD TYPE              
         MVC   KEY+1(2),QRECORD+2  INSERT REP CODE                              
         GOTO1 HIGH                START READ                                   
         B     SWEE0040                                                         
SWEE0020 EQU   *                                                                
         MVC   KEY(27),KEYSAVEX    RESET KEY                                    
         MVI   KEY+8,X'FF'         SKIP-READ THE STATION TO NEXT ONE            
         GOTO1 HIGH                RESTART CONTRACT SEQUENCE                    
SWEE0040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'KEY FOUND:'                                           
         MVC   P+12(27),KEY                                                     
         MVC   P+42(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   KEYSAVEX,KEY        SAVE THE KEY FOR RESTART                     
         CLC   KEY(03),KEYSAVE     SAME TYPE/REP?                               
         BNE   SWEE0400            NO  - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,2               SET UP TO READ STATION                       
         MVC   KEY+20(2),QRECORD+2 INSERT REP CODE                              
         MVC   KEY+22(5),KEYSAVEX+3                                             
*                                  INSERT STATION CALLS                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SWEE0100            NO                                           
         MVC   P+1(15),=C'FOUND: CON/STA='                                      
         MVC   P+16(5),KEYSAVEX+3                                               
         MVI   P+21,C'/'                                                        
         MVC   P+22(5),KEY+22                                                   
         GOTO1 REPORT                                                           
         B     SWEE0020            GO BACK FOR NEXT                             
SWEE0100 EQU   *                                                                
         MVC   P+1(15),=C'MISSD: CON/   ='                                      
         MVC   P+16(5),KEYSAVEX+3                                               
         GOTO1 REPORT                                                           
         B     SWEE0020            GO BACK FOR NEXT                             
SWEE0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
KEYSAVEX DS    CL27                                                             
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR FILTERED STATION LOOKING FOR COMBO ORDERS -              
*      ORDERS WITH X'17' ELEMENTS.  IF FOUND, CHECK DATE RANGE FOR              
*      X'04' ELEMENTS WITHIN DATE RANGE.  IF NOT FOUND, DISPLAY                 
*      CONTRACT #, INSERT ELEMENTS.                                             
*                                                                               
POSA     NTR1                                                                   
*                                                                               
         MVC   QSTART+4(2),=C'01'  INSERT 'DAYS' IN DATE                        
         MVC   QEND+4(2),=C'01'    DITTO                                        
         GOTO1 DATCON,DMCB,(0,QSTART),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(3,WORK+3)                                  
         LA    R2,RCONELEM         FIND X'17' ELEMENT                           
POSA0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    MODEEXIT            YES - NO X'17' ELEMENT                       
         CLI   0(R2),X'17'                                                      
         BNE   POSA0010            NOT X'17' ELEMENT                            
         LA    R2,RCONELEM         X'17' FOUND - LOOK FOR INV ELTS              
POSA0020 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    POSA0030            YES - NO X'04' ELEMENT OR NOT                
*                                     WITHIN CLOSED PERIOD                      
         CLI   0(R2),X'04'                                                      
         BNE   POSA0020            NOT X'04' ELEMENT                            
         CLC   WORK(2),2(R2)       START DATE VS BUCKET DATE                    
         BH    POSA0020            BUCKET EARLIER THAN START DATE               
         B     MODEEXIT            BUCKET WITHIN CLOSED PERIOD - EXIT           
POSA0030 EQU   *                                                                
         MVC   P+5(10),=C'CONTRACT= '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   MODEEXIT            NO                                           
*                                                                               
*   IF UPDATE CODE IS NEEDED, IT GOES HERE!! MAYBE.                             
*                                                                               
         BAS   RE,PUTCON           REWRITE RECORD                               
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR FILTERED STATION LOOKING FOR CONTRACTS WITH              
*      CLOSED-OUT BUCKET FOR REQUESTED MONTH.                                   
*      IF FOUND, DISPLAY CONTRACT #.                                            
*                                                                               
POSB     NTR1                                                                   
*                                                                               
         MVC   QSTART+4(2),=C'01'  INSERT 'DAYS' IN DATE                        
         MVC   QEND+4(2),=C'01'    DITTO                                        
         GOTO1 DATCON,DMCB,(0,QSTART),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(3,WORK+3)                                  
         LA    R2,RCONELEM         FIND X'04' ELEMENT                           
POSB0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    MODEEXIT            YES - NO X'04' ELEMENT                       
*                                     FOR REQUESTED PERIOD                      
         CLI   0(R2),X'04'                                                      
         BNE   POSB0010            NOT X'04' ELEMENT                            
         CLC   WORK(2),2(R2)       START DATE VS BUCKET DATE                    
         BH    POSB0010            BUCKET DATE BEFORE START DATE                
         CLC   WORK+3(2),2(R2)     END   DATE VS BUCKET DATE                    
         BL    POSB0010            BUCKET DATE AFTER  END   DATE                
         OC    6(4,R2),6(R2)       ANY MONEY IN BUCKET                          
         BNZ   POSB0010            YES - SKIP IT                                
         MVC   P+5(10),=C'CONTRACT= '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,KEY+28,P+25,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR ORDERS WITH 0% SPL AND CONTRACT DOLLARS                  
*      IF FOUND, DISPLAY CONTRACT #.                                            
*                                                                               
POSE     NTR1                                                                   
*                                                                               
         LA    R2,RCONELEM         FIND X'06' ELEMENT                           
POSE0020 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    MODEEXIT            YES - NO X'06' ELEMENT                       
         CLI   0(R2),X'06'                                                      
         BNE   POSE0020            NOT X'08' ELEMENT                            
         USING RCONSPEL,R2                                                      
*                                                                               
         TM    RCONSPES,X'40'      REPD STA $$ = ZERO?                          
         BNO   MODEEXIT            NO  - EXIT                                   
*                                  YES - CHECK FOR ORDER DOLLARS                
         DROP R2                                                                
         LA    R2,RCONELEM         SCAN X'03' ELEMENTS                          
         SR    RF,RF                                                            
POSE0040 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    POSE0060            YES - CHECK TOTAL ORDERED                    
         CLI   0(R2),X'03'                                                      
         BNE   POSE0040            NOT X'03' ELEMENT                            
         USING RCONBKEL,R2                                                      
*                                                                               
         MVC   DUB(4),RCONBKAM                                                  
         A     RF,DUB              ACCUMULATE ORDER DOLLARS                     
         B     POSE0040            GO BACK FOR NEXT BUCKET                      
*                                                                               
         DROP  R2                                                               
POSE0060 EQU   *                                                                
         LTR   RF,RF               ANY VALUE IN REGISTER?                       
         BZ    MODEEXIT            NO  - DON'T DISPLAY CONTRACT                 
*                                  YES - DISPLAY CONTRACT                       
         MVC   P+5(10),=C'CONTRACT= '                                           
         EDIT  (RF),(10,P+25),2                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
****>>>  GOTO1 HEXOUT,DMCB,KEY+28,P+40,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR FILTERED STATION LOOKING FOR COMBO ORDERS -              
*      ORDERS WITH X'17' ELEMENTS.  IF FOUND, DISPLAY CONTRACT #                
*                                                                               
POSC     NTR1                                                                   
*                                                                               
         LA    R2,RCONELEM         FIND X'17' ELEMENT                           
POSC0010 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    MODEEXIT            YES - NO X'17' ELEMENT                       
         CLI   0(R2),X'17'                                                      
         BNE   POSC0010            NOT X'17' ELEMENT                            
*                                                                               
*   X'17' ELEMENT FOUND - DISPLAY CONTRACT NUMBER                               
*                                                                               
         MVC   P+5(10),=C'CONTRACT= '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   SCAN CONTRACTS FOR ORDERS WITH X'23' ELEMENTS.  IF FOUND,                   
*      DELETE THEM AND REGENERATE THEM ON BROADCAST MONTH BASIS.                
*                                                                               
POSD     NTR1                                                                   
*                                                                               
         BAS   RE,INITIAL                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           SET UP KEY FOR BLAIR                         
         MVC   KEY+2(2),=C'BL'     INSERT BLAIR USER ID                         
         GOTO1 HIGH                GET FIRST RECORD                             
         B     POSD0080                                                         
POSD0040 EQU   *                                                                
         GOTO1 SEQ                 GET NEXT RECORD                              
POSD0080 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME REC TYPE/USER?                          
         BE    POSD0100            YES                                          
         BAS   RE,SHOWTOTS         NO  - SHOW TOTALS, FINISHED                  
         B     MODEEXIT                                                         
POSD0100 EQU   *                                                                
         GOTO1 GETCON              YES - READ THE CONTRACT RECORD               
         MVC   FIRSTCON,RCONLEN    SAVE LENGTH                                  
         L     RF,PROCCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
         CLC   RCONDATE+3(3),=X'5F0101'                                         
         BL    POSD0040            FLIGHT ENDED AFTER JAN01/95                  
         CLC   =C'CUT',QUESTOR+7   CUT-OFF REQUESTED?                           
         BNE   POSD0110            NO  - GO FOR IT ALL                          
         C     RF,=F'1000'         YES - CUT-OFF TEST                           
         BNH   POSD0110            CUT-OFF NOT REACHED                          
         BAS   RE,SHOWTOTS         SHOW TOTALS, FINISHED                        
         B     MODEEXIT                                                         
POSD0110 EQU   *                                                                
         LA    R4,RCONELEM         FIND X'12' ELEMENT (SAR)                     
POSD0120 EQU   *                                                                
         ZIC   R1,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R4),0             END OF RECORD?                               
         BE    POSD0040            YES - NO X'12' ELEMENT                       
         CLI   0(R4),X'12'                                                      
         BNE   POSD0120            NOT X'12' ELEMENT                            
*                                                                               
*   X'12' ELEMENT FOUND - SET USING                                             
*                                                                               
         USING RSARXEL,R4                                                       
*                                                                               
         OC    RSARXBGT,RSARXBGT                                                
         BZ    POSD0040            NO BUDGET: GO BACK FOR NEXT                  
*                                     RECORD                                    
*                                                                               
         LA    R2,RCONELEM         FIND X'23' ELEMENT                           
POSD0160 EQU   *                                                                
         ZIC   R1,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   0(R2),0             END OF RECORD?                               
         BE    POSD0180            YES - NO X'23' ELEMENT                       
         CLI   0(R2),X'23'                                                      
         BE    POSD0040            X'23' ELEMENT:  SPREAD DONE                  
         B     POSD0160                                                         
POSD0180 EQU   *                                                                
         L     RF,CHGDCTR          INCREMENT PROCESSED COUNT                    
         LA    RF,1(RF)                                                         
         ST    RF,CHGDCTR                                                       
*                                                                               
*   NO X'23' ELEMENT FOUND - DISPLAY CONTRACT NUMBER                            
*                                                                               
         CLC   =C'PRINTIT',QUESTOR PRINT DISPLAY INFO?                          
         BNE   POSD0200            NO                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+1(08),=C'PRE-FIX:'                                             
         GOTO1 REPORT                                                           
         ZICM  RF,RCONLEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
         B     POSD0220                                                         
POSD0200 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+1(08),=C'ADJUSTED'                                             
         GOTO1 REPORT                                                           
POSD0220 EQU   *                                                                
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    NEW23ELT,NEW23ELT   SET NEW ELEMENT                              
         MVC   NEW23ELT(2),=X'230A'                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,DMCB             SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEW23ELT+4)                            
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
GENF0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,DAYTABLE         ACCUMULATE TOTAL DAYS                        
GENF0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    GENF0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     GENF0030            GO BACK FOR NEXT                             
GENF0040 EQU   *                                                                
         ST    RF,TOTDAYS          SAVE IT FOR LATER                            
         MVC   FULL,RSARXBGT       LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,FULL                                                          
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,TOTDAYS          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,TOTDAYS          SAVE $$ PER DAY                              
         LA    R2,DAYTABLE                                                      
GENF0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    GENF0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     GENF0050            GO BACK FOR NEXT                             
GENF0060 EQU   *                                                                
         BAS   RE,FINALEN          CHECK FINAL LENGTH                           
         CLC   =C'PRINTIT',QUESTOR PRINT DISPLAY INFO?                          
         BNE   GENF0080            NO                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+1(09),=C'POST-FIX:'                                            
         GOTO1 REPORT                                                           
         ZICM  RF,RCONLEN,2        LOAD RECORD LENGTH                           
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
GENF0080 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHERS                  
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   POSD0040            NO                                           
         BAS   RE,PUTCON           REWRITE RECORD                               
         B     POSD0040            GO GET NEXT RECORD                           
         EJECT                                                                  
FINALEN  NTR1                                                                   
         CLC   FIRSTCON,RCONLEN    START VS FINISH LENGTH                       
         BE    FINA0020            SAME                                         
         BH    FINA0040            MADE SMALLER                                 
         L     RF,BIGRCTR          MADE LARGER                                  
         LA    RF,1(RF)                                                         
         ST    RF,BIGRCTR                                                       
         B     FINA0060                                                         
FINA0020 EQU   *                                                                
         L     RF,SAMESIZE         SAME SIZE OUTPUT/INPUT                       
         LA    RF,1(RF)                                                         
         ST    RF,SAMESIZE                                                      
         B     FINA0060                                                         
FINA0040 EQU   *                                                                
         L     RF,SMLRCTR          MADE SMALLER                                 
         LA    RF,1(RF)                                                         
         ST    RF,SMLRCTR                                                       
FINA0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GENDAYS  NTR1                                                                   
         MVC   CYCLEDAT,RCONDATE   CONTRACT FLIGHT DATES                        
*                                                                               
*   EXTRA PRINT LINE IS USED TO SET UP BROADCAST MONTH ARRAY.                   
*                                                                               
         LA    R2,MYP              A(DAYTABLE)                                  
         XC    MYP,MYP             INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,CYCLEDAT),(0,DAYTABLE)                            
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 =V(GETBROAD),DMCB,(0,DAYTABLE),DAYTABLE+6,0,0                    
         MVC   BRDWEEKS,DMCB       INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   DMCB,X'FF'          ERROR?                                       
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 DATCON,DMCB,(0,DAYTABLE+6),(3,BRDSTART)                          
*                                  INSERT START DATE IN TABLE                   
         GOTO1 DATCON,DMCB,(0,DAYTABLE+12),(3,BRDEND)                           
*                                  INSERT END   DATE IN TABLE                   
         CLC   CYCLEDAT+3(3),BRDEND                                             
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,DAYTABLE+6)                            
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 ADDAY,DMCB,DAYTABLE+6,DAYTABLE,(RF)                              
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    DAYTABLE(56),DAYTABLE     CLEAR THE WORKAREA                     
         LA    R2,MYP              RESET A(BDCST MONTH TABLE)                   
         LA    R3,DAYTABLE                                                      
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
SPREDAYS NTR1                                                                   
         LA    R2,DAYTABLE         A(DAYTABLE)                                  
         LA    R3,MYP              A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,RCONDATE   IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
*                                  CONVERT FLIGHT START DATE                    
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,WORK+6)                                
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BRDSTART),(0,WORK)                                
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
         MVC   2(2,R2),DMCB+8                                                   
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
GENBUCKS NTR1                                                                   
         MVC   NEW23ELT+2(2),0(R2) INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,TOTDAYS             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'1000'         ADD FOR ROUNDING                             
         D     RE,=F'1000'         DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         SR    RE,RE               MULTIPLY BY 100 FOR PENNIES                  
         M     RE,=F'100'                                                       
         ST    RF,FULL                                                          
         MVC   NEW23ELT+6(4),FULL  INSERT INTO X'23' ELEMENT                    
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW23ELT,0             
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
SHOWTOTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE A PAGE-BREAK                           
         MVC   P+1(21),=C'TOTAL CONTRACTS READ:'                                
         EDIT  PROCCTR,(15,P+40),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL CONTRACTS CHANGED:'                             
         EDIT  CHGDCTR,(15,P+40),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'CONTRACTS REDUCED   :'                                
         EDIT  SMLRCTR,(15,P+40),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'CONTRACTS ENLARGED  :'                                
         EDIT  BIGRCTR,(15,P+40),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'CONTRACTS SAME SIZE :'                                
         EDIT  SAMESIZE,(15,P+40),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATEVAL:  PASS VARYING FORMATS INTO DATVAL, AND DISPLAY OUTPUT              
*       RESULTS.                                                                
*                                                                               
DATEVAL  NTR1                                                                   
         MVC   P+25(16),=C'DATVAL EXERCIZER'                                    
         GOTO1 REPORT                                                           
         MVC   P+25(16),=C'****************'                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(15),=C'INPUT:         '                                      
         MVC   P+16(6),=C'8/7-5 '                                               
         MVC   P+24(15),=C'OUTPUT:        '                                     
         GOTO1 =V(DATVAL),DMCB,(1,P+16),P+32                                    
         GOTO1 REPORT                                                           
         MVC   P+1(15),=C'INPUT:         '                                      
         MVC   P+16(6),=C'8/17-5 '                                              
         MVC   P+24(15),=C'OUTPUT:        '                                     
         GOTO1 =V(DATVAL),DMCB,(1,P+16),P+32                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DATEDUMP:  PASS VARYING FORMATS INTO DATCON, AND DISPLAY OUTPUT             
*       RESULTS.                                                                
*                                                                               
DATEDUMP NTR1                                                                   
         MVC   P+25(25),=C'DATE ROUTINE(S) EXERCIZER'                           
         GOTO1 REPORT                                                           
         MVC   P+25(25),=C'*************************'                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(15),=C'INPUT:  EBCDIC '                                      
         MVC   P+16(6),=C'000223'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(0,P+16),(3,WORK)                                    
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(3),=X'000217'                                               
         MVC   P+24(15),=C'OUTPUT:  EBCDIC'                                     
         GOTO1 DATCON,DMCB,(3,P+16),(5,WORK)                                    
         MVC   P+42(7),WORK                                                     
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(3),=X'640217'                                               
         MVC   P+24(15),=C'OUTPUT:  EBCDIC'                                     
         GOTO1 DATCON,DMCB,(3,P+16),(5,WORK)                                    
         MVC   P+42(7),WORK                                                     
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  EBCDIC '                                      
         MVC   P+16(6),=C'000223'                                               
         MVC   P+24(15),=C'OUTPUT:  COMP  '                                     
         GOTO1 DATCON,DMCB,(0,P+16),(2,WORK)                                    
         GOTO1 HEXOUT,DMCB,WORK,P+42,2,=C'TOG'                                  
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(3),=X'640217'                                               
         MVC   P+24(15),=C'OUTPUT:  COMP  '                                     
         GOTO1 DATCON,DMCB,(3,P+16),(2,WORK)                                    
         GOTO1 HEXOUT,DMCB,WORK,P+42,2,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  EBCDIC '                                      
         MVC   P+16(6),=C'930225'                                               
         MVC   P+24(15),=C'OUTPUT:  EBCDIC'                                     
         MVC   P+50(15),=C'ADDAY:   +3000 '                                     
         GOTO1 ADDAY,DMCB,P+16,P+42,3000                                        
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  EBCDIC '                                      
         MVC   P+16(6),=C'930225'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(0,P+16),(19,WORK)                                   
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         MVC   P+50(15),=C'JULIAN DATE    '                                     
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  EBCDIC '                                      
         MVC   P+16(6),=C'000225'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(0,P+16),(19,WORK)                                   
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         MVC   P+50(15),=C'JULIAN DATE    '                                     
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(6),=X'000225'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(3,P+16),(19,WORK)                                   
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         MVC   P+50(22),=C'JULIAN DATE: YEAR = 00'                              
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(6),=X'640225'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(3,P+16),(19,WORK)                                   
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         MVC   P+50(29),=C'JULIAN DATE: YEAR = 64 (2000)'                       
         GOTO1 REPORT                                                           
****************************************************                            
         MVC   P+1(15),=C'INPUT:  HEX    '                                      
         MVC   P+16(6),=X'C80225'                                               
         MVC   P+24(15),=C'OUTPUT:  HEX   '                                     
         GOTO1 DATCON,DMCB,(3,P+16),(19,WORK)                                   
         GOTO1 HEXOUT,DMCB,WORK,P+42,3,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,P+16,WORK,3,=C'TOG'                                  
         MVC   P+16(6),WORK                                                     
         MVC   P+50(29),=C'JULIAN DATE: YEAR = C8 (2100)'                       
         GOTO1 REPORT                                                           
****************************************************                            
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
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015REREP1H04 05/01/02'                                      
         END                                                                    
