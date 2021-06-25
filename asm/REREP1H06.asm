*          DATA SET REREP1H06  AT LEVEL 006 AS OF 05/01/02                      
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
         TITLE 'REREP1H06 - PURGE:  ALLNY RECORDS    '                          
*********************************************************************           
*                                                                   *           
*        REREP1H06 --- PURGE: ALLNY RECORDS                         *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JUN20/96 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
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
         DC    AL1(REQFRST),AL3(AQPURGE)  PURGE RECORDS FROM AQ                 
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
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
*   AQPURGE:  QUESTOR BYTES INDICATE WHICH RECORDS TO PURGE.                    
*        QUESTOR+0  =  PURGE STATIONS                                           
*        QUESTOR+1  =  PURGE SALESPERSONS                                       
*        QUESTOR+2  =  PURGE GENERAL RECORDS                                    
*        QUESTOR+3  =  PURGE ADVERTISER RECORDS                                 
*        QUESTOR+4  =  PURGE CONTRACTS + BUYLINES                               
*      KEYS ARE RETRIEVED FOR REP, AND CONTROL BYTE IS SET TO                   
*        DELETE, AND KEY REWRITTEN.  DUMP WILL THEREAFTER BYPASS                
*        RECORD.                                                                
*                                                                               
AQPURGE  NTR1                                                                   
*                                                                               
         CLI   QUESTOR+0,C'Y'      DELETE STATIONS?                             
         BNE   AQPU0100            NO                                           
         MVC   P+1(25),=C'STARTING STATION DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,2               SET TYPE FOR STATION                         
         MVC   KEY+20(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0020                                                         
AQPU0010 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0020 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0100            NO  - FINISHED STATIONS                      
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,STACTR+16                                                     
         LA    RF,1(RF)            INCREMENT STATION COUNT                      
         ST    RF,STACTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0010            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0010            GO BACK FOR NEXT RECORD                      
AQPU0100 EQU   *                                                                
AQPU0150 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DELETE SALESPERSONS?                         
         BNE   AQPU0200            NO                                           
         MVC   P+1(25),=C'STARTING S/P     DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,6               SET TYPE FOR SALESPERSON                     
         MVC   KEY+22(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     AQPU0170                                                         
AQPU0160 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0170 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0200            NO  - FINISHED SALESPERSONS                  
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,SLSCTR+16                                                     
         LA    RF,1(RF)            INCREMENT S/P     COUNT                      
         ST    RF,SLSCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0160            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0160            GO BACK FOR NEXT RECORD                      
AQPU0200 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DELETE GENERAL RECORDS?                      
         BNE   AQPU0400            NO                                           
*                                  YES - BEGIN WITH OFFICE RECORDS              
         MVC   P+1(25),=C'STARTING OFFICE  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'04'           SET TYPE FOR OFFICE RECORDS                  
         MVC   KEY+23(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     AQPU0220                                                         
AQPU0210 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0220 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME KEY THRU REP?                           
         BNE   AQPU0250            NO  - FINISHED OFFICE 04 RECORDS             
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,OFFCTR+16                                                     
         LA    RF,1(RF)            INCREMENT OFFICE  COUNT                      
         ST    RF,OFFCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0210            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0210            GO BACK FOR NEXT RECORD                      
AQPU0250 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'44'           SET TYPE FOR OFFICE 2NDARY RECORDS           
         MVC   KEY+23(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     AQPU0270                                                         
AQPU0260 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0270 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME KEY THRU REP?                           
         BNE   AQPU0272            NO  - FINISHED OFFICE 44 RECORDS             
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,OFF2CTR+16                                                    
         LA    RF,1(RF)            INCREMENT OFFICE2 COUNT                      
         ST    RF,OFF2CTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0260            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0260            GO BACK FOR NEXT RECORD                      
AQPU0272 EQU   *                                                                
         MVC   P+1(25),=C'STARTING REGION  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,3               SET TYPE FOR REGION                          
         MVC   KEY+23(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0274                                                         
AQPU0273 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0274 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0275            NO  - FINISHED REGIONS                       
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,REGCTR+16                                                     
         LA    RF,1(RF)            INCREMENT REGION  COUNT                      
         ST    RF,REGCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0273            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0273            GO BACK FOR NEXT RECORD                      
AQPU0275 EQU   *                                                                
         MVC   P+1(25),=C'STARTING OFFBUD  DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'19'           SET TYPE FOR OFFICE BUDGET                   
         MVC   KEY+17(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0277                                                         
AQPU0276 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0277 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0278            NO  - FINISHED OFFBUDS                       
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,OBUDCTR+16                                                    
         LA    RF,1(RF)            INCREMENT OFFICE BUDGET COUNT                
         ST    RF,OBUDCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0276            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0276            GO BACK FOR NEXT RECORD                      
AQPU0278 EQU   *                                                                
         MVC   P+1(27),=C'STARTING PROG TYPE DELETION'                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'19'           SET TYPE FOR OFFICE BUDGET                   
         MVC   KEY+24(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0280                                                         
AQPU0279 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0280 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0281            NO  - FINISHED PROGRAM TYPES                 
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,PGTYCTR+16                                                    
         LA    RF,1(RF)            INCREMENT PROGRAM TYPE  COUNT                
         ST    RF,PGTYCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0279            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0279            GO BACK FOR NEXT RECORD                      
AQPU0281 EQU   *                                                                
         MVC   P+1(27),=C'STARTING STD COMMT DELETION'                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'2E'           SET TYPE FOR STD COMMENT                     
         MVC   KEY+15(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0283                                                         
AQPU0282 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0283 EQU   *                                                                
         CLC   KEY(17),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0284            NO  - FINISHED STANDARD COMMENT              
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,SCOMCTR+16                                                    
         LA    RF,1(RF)            INCREMENT STANDARD COMMT COUNT               
         ST    RF,SCOMCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0282            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0282            GO BACK FOR NEXT RECORD                      
AQPU0284 EQU   *                                                                
         MVC   P+1(27),=C'STARTING RADAR     DELETION'                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'33'           SET TYPE FOR RADAR                           
         MVC   KEY+17(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0286                                                         
AQPU0285 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0286 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0287            NO  - FINISHED RADAR                         
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,RADRCTR+16                                                    
         LA    RF,1(RF)            INCREMENT RADAR COUNT                        
         ST    RF,RADRCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0285            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0285            GO BACK FOR NEXT RECORD                      
AQPU0287 EQU   *                                                                
         MVC   P+1(27),=C'STARTING OFFICE COMT DELETE'                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'34'           SET TYPE FOR OFFICE COMMENT                  
         MVC   KEY+20(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0289                                                         
AQPU0288 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0289 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0290            NO  - FINISHED OFFICE COMMENT                
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,OCOMCTR+16                                                    
         LA    RF,1(RF)            INCREMENT OCOM  COUNT                        
         ST    RF,OCOMCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0288            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0288            GO BACK FOR NEXT RECORD                      
AQPU0290 EQU   *                                                                
         MVC   P+1(27),=C'STARTING LABEL DELETION    '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'36'           SET TYPE FOR LABEL                           
         MVC   KEY+17(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0292                                                         
AQPU0291 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0292 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0293            NO  - FINISHED LABEL                         
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,LABLCTR+16                                                    
         LA    RF,1(RF)            INCREMENT LABEL COUNT                        
         ST    RF,LABLCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0291            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0291            GO BACK FOR NEXT RECORD                      
AQPU0293 EQU   *                                                                
         MVC   P+1(27),=C'STARTING TYPE  DELETION    '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'30'           SET TYPE FOR TYPE                            
         MVC   KEY+17(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0295                                                         
AQPU0294 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0295 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0296            NO  - FINISHED TYPE                          
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,TYPECTR+16                                                    
         LA    RF,1(RF)            INCREMENT TYPE  COUNT                        
         ST    RF,TYPECTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0294            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0294            GO BACK FOR NEXT RECORD                      
AQPU0296 EQU   *                                                                
         MVC   P+1(27),=C'STARTING SET   DELETION    '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'38'           SET TYPE FOR SET                             
         MVC   KEY+19(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0298                                                         
AQPU0297 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0298 EQU   *                                                                
         CLC   KEY(21),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0299            NO  - FINISHED SETS                          
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,SETSCTR+16                                                    
         LA    RF,1(RF)            INCREMENT SETS  COUNT                        
         ST    RF,SETSCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0297            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0297            GO BACK FOR NEXT RECORD                      
AQPU0299 EQU   *                                                                
         MVC   P+1(27),=C'STARTING TERR  DELETION    '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'3D'           SET TYPE FOR TERRITORY                       
         MVC   KEY+23(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0301                                                         
AQPU0300 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0301 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0302            NO  - FINISHED TERRITORY                     
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,TERRCTR+16                                                    
         LA    RF,1(RF)            INCREMENT SETS  COUNT                        
         ST    RF,TERRCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0300            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0300            GO BACK FOR NEXT RECORD                      
AQPU0302 EQU   *                                                                
         MVC   P+1(27),=C'STARTING CON TYPE DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'32'           SET TYPE FOR CONTRACT TYPE                   
         MVC   KEY+24(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0304                                                         
AQPU0303 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0304 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0305            NO  - FINISHED CON TYPE                      
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,CTYPCTR+16                                                    
         LA    RF,1(RF)            INCREMENT CONTYPE COUNT                      
         ST    RF,CTYPCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0303            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0303            GO BACK FOR NEXT RECORD                      
AQPU0305 EQU   *                                                                
         MVC   P+1(27),=C'STARTING BUDGET   DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'13'           SET TYPE FOR BUDGET                          
         MVC   KEY+16(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0307                                                         
AQPU0306 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0307 EQU   *                                                                
         CLC   KEY(18),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0308            NO  - FINISHED CON TYPE                      
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,BUDGCTR+16                                                    
         LA    RF,1(RF)            INCREMENT BUDGET  COUNT                      
         ST    RF,BUDGCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0306            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0306            GO BACK FOR NEXT RECORD                      
AQPU0308 EQU   *                                                                
         MVC   P+1(27),=C'STARTING EOM      DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'18'           SET TYPE FOR EOM                             
         MVC   KEY+24(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0310                                                         
AQPU0309 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0310 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0311            NO  - FINISHED EOM RECORD                    
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,EOMSCTR+16                                                    
         LA    RF,1(RF)            INCREMENT EOM     COUNT                      
         ST    RF,EOMSCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0309            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0309            GO BACK FOR NEXT RECORD                      
AQPU0311 EQU   *                                                                
         MVC   P+1(27),=C'STARTING DAYPART  DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'24'           SET TYPE FOR DAYPART                         
         MVC   KEY+24(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0313                                                         
AQPU0312 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0313 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0314            NO  - FINISHED DAYPART RECORD                
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,DYPTCTR+16                                                    
         LA    RF,1(RF)            INCREMENT DAYPART COUNT                      
         ST    RF,DYPTCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0312            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0312            GO BACK FOR NEXT RECORD                      
AQPU0314 EQU   *                                                                
         MVC   P+1(27),=C'STARTING DYPT DEF DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'26'           SET TYPE FOR DAYPART DEF                     
         MVC   KEY+20(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0316                                                         
AQPU0315 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0316 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0317            NO  - FINISHED DAYPART RECORD                
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,DYDFCTR+16                                                    
         LA    RF,1(RF)            INCREMENT DAYPART DEF COUNT                  
         ST    RF,DYDFCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0315            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0315            GO BACK FOR NEXT RECORD                      
AQPU0317 EQU   *                                                                
         MVC   P+1(27),=C'STARTING COMMISSN DELETION '                          
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'29'           SET TYPE FOR DAYPART DEF                     
         MVC   KEY+11(2),QRECORD+20      INSERT REP CODE                        
         GOTO1 HIGH                START READ                                   
         B     AQPU0319                                                         
AQPU0318 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0319 EQU   *                                                                
         CLC   KEY(13),KEYSAVE     SAME KEY THROUGH REP?                        
         BNE   AQPU0320            NO  - FINISHED COMMISS RECORD                
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,COMMCTR+16                                                    
         LA    RF,1(RF)            INCREMENT COMMISSION  COUNT                  
         ST    RF,COMMCTR+16                                                    
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0318            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0318            GO BACK FOR NEXT RECORD                      
AQPU0320 EQU   *                                                                
AQPU0400 EQU   *                                                                
         LTR   R0,R0                                                            
         CLI   QUESTOR+4,C'Y'      DELETE CON/BUY RECORDS?                      
         BNE   AQPU0500            NO                                           
         MVC   P+1(25),=C'STARTING CON/BUY DELETION'                            
         GOTO1 REPORT                                                           
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0C'           SET TYPE FOR CONTRACT                        
         MVC   KEY+2(2),QRECORD+20                                              
         GOTO1 HIGH                START READ                                   
         B     AQPU0420                                                         
AQPU0410 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0420 EQU   *                                                                
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   AQPU0450            NO  - FINISHED CONTRACTS                     
         CLC   KEY+2(2),QRECORD+20       SAME REP?                              
         BNE   AQPU0450            NO  - FINISHED CONTRACTS                     
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,CONCTR+16                                                     
         LA    RF,1(RF)            INCREMENT CON/BUY COUNT                      
         ST    RF,CONCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0410            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0410            GO BACK FOR NEXT RECORD                      
AQPU0450 EQU   *                                                                
         XC    KEY,KEY             YES                                          
         MVI   KEY,X'0B'           SET TYPE FOR BUYLINES                        
         MVC   KEY+16(2),QRECORD+20                                             
         GOTO1 HIGH                START READ                                   
         B     AQPU0470                                                         
AQPU0460 EQU   *                                                                
         GOTO1 SEQ                 SEQUENTIAL READ                              
AQPU0470 EQU   *                                                                
         CLI   KEY,X'0B'           BUYLINE  RECORD?                             
         BNE   AQPU0500            NO  - FINISHED BUYLINES                      
         CLC   KEY+16(2),QRECORD+20                                             
         BNE   AQPU0500            NO  - NOT RIGHT REP - FINISHED               
         BAS   RE,AQPRINT                                                       
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         BAS   RE,AQPRINT                                                       
         L     RF,BUYCTR+16                                                     
         LA    RF,1(RF)            INCREMENT CON/BUY COUNT                      
         ST    RF,BUYCTR+16                                                     
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    AQPU0460            YES - DON'T TRY TO WRITE                     
         GOTO1 WRITE               REWRITE KEY                                  
         B     AQPU0460            GO BACK FOR NEXT RECORD                      
AQPU0500 EQU   *                                                                
         LA    R1,STACTR                                                        
AQPU0600 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    AQPU0700            YES - FINISHED                               
         MVC   P+1(16),0(R1)       MOVE TABLE DESCRIPTION                       
         L     R2,16(R1)           LOAD COUNT                                   
         EDIT  (R2),(8,P+20)                                                    
         GOTO1 REPORT                                                           
         LA    R1,20(R1)                                                        
         B     AQPU0600            GO BACK FOR NEXT                             
AQPU0700 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 5                                                                
AQPRINT  NTR1                                                                   
         CLI   QOPTION2,C'P'       PRINT DETAILS?                               
         BNE   AQPR0020            NO                                           
         MVC   P+1(34),KEY         SHOW PRE-KEY                                 
         GOTO1 REPORT                                                           
AQPR0020 EQU   *                                                                
         XIT1                                                                   
         DS   0F                                                                
*               1.3.5.7.9.1.3.5.7.9                                             
STACTR   DC   C'STATIONS PURGED '                                               
         DC   F'0'                                                              
SLSCTR   DC   C'SALESPNS PURGED '                                               
         DC   F'0'                                                              
CONCTR   DC   C'CONTRCTS PURGED '                                               
         DC   F'0'                                                              
BUYCTR   DC   C'BUYS     PURGED '                                               
         DC   F'0'                                                              
REGCTR   DC   C'REGIONS  PURGED '                                               
         DC   F'0'                                                              
OFFCTR   DC   C'OFFICE   PURGED '                                               
         DC   F'0'                                                              
OFF2CTR  DC   C'OFFICE2  PURGED '                                               
         DC   F'0'                                                              
OBUDCTR  DC   C'OFF BDGT PURGED '                                               
         DC   F'0'                                                              
PGTYCTR  DC   C'PTYPES   PURGED '                                               
         DC   F'0'                                                              
SCOMCTR  DC   C'STD CMTS PURGED '                                               
         DC   F'0'                                                              
CTYPCTR  DC   C'CON TYPE PURGED '                                               
         DC   F'0'                                                              
RADRCTR  DC   C'RADAR    PURGED '                                               
         DC   F'0'                                                              
OCOMCTR  DC   C'OCOMMENT PURGED '                                               
         DC   F'0'                                                              
LABLCTR  DC   C'LABELS   PURGED '                                               
         DC   F'0'                                                              
TYPECTR  DC   C'TYPES    PURGED '                                               
         DC   F'0'                                                              
SETSCTR  DC   C'SETS     PURGED '                                               
         DC   F'0'                                                              
TERRCTR  DC   C'TERRITS  PURGED '                                               
         DC   F'0'                                                              
BUDGCTR  DC   C'BUDGETS  PURGED '                                               
         DC   F'0'                                                              
EOMSCTR  DC   C'EOMS     PURGED '                                               
         DC   F'0'                                                              
DYPTCTR  DC   C'DAYPARTS PURGED '                                               
         DC   F'0'                                                              
DYDFCTR  DC   C'DPT DEFS PURGED '                                               
         DC   F'0'                                                              
COMMCTR  DC   C'COMMISS  PURGED '                                               
         DC   F'0'                                                              
         DC   F'0'                 DELIMITER                                    
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REREP1H06 05/01/02'                                      
         END                                                                    
