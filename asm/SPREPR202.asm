*          DATA SET SPREPR202  AT LEVEL 209 AS OF 09/10/04                      
*          DATA SET SPREPR202A AT LEVEL 120 AS OF 06/01/98                      
*PHASE SPR202A,*                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SPBYGENB                                                               
         TITLE 'SPREPR202 (SPR202) - REP -> SPOT BUY X-FER UPDATE'              
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR202 (SPR202) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRANSFER   *          
*                          SPOT FILE UPDATING AND REPORT GENERATOR   *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
*  QOPT2    =  IF 'Y' DO SPBYGEN DISPLAYS                            *          
*           =  IF 'D' FORCE DUMP WHERE REQUESTED DURING TESTING      *          
*                                                                    *          
*  QOPT3    =  IF 'L' DO NOT SEND OUTPUT TO THE PRINT QUEUES         *          
*  QOPT4    =  IF 'N' SEND NOWRITE FLAG TO SPBYGEN                   *          
*  QOPT5    =  IF 'N' IGNORE 'USED'/RE-RUN FLAGS AND DATE CHECK      *          
*                 'M' IGNORE 'USED'/RE-RUN FLAGS ONLY                *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*                                                                    *          
* WHILE NOT EOF ON SORTED FILE                                       *          
*     -PERFORM A REP BREAK                                           *          
*     -PERFORM A ESTIMATE BREAK                                      *          
*     -PERFORM A STATION BREAK                                       *          
*     -PERFORM A CONTRACT BREAK                                      *          
*     -PROCESS A DETAIL RECORD                                       *          
*          -PRODUCE A DETAIL PRINT LINE                              *          
*          -UPDATE THE SPOTPAK FILE                                  *          
*          -WRITE A REPFILE UPDATE RECORD                            *          
* END WHILE                                                          *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JUL15/91 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*  APR17/92 (BU ) --- >EXPANDED COMMENTS FOR UNDERSTANDABILITY.      *          
*                                                                    *          
*  APR23/92 (BU ) --- >APPLY BUY MISSED ELEMENTS AGAINST SPSPECL     *          
*                      TABLE TO GET CORRECT NUMBER OF SPOTS          *          
*                                                                    *          
*  MAY01/92 (BU ) --- >CHANGE ERROR MESSAGE IF BUY LINE XCL'D HAD    *          
*                      PAID OR BILLED SPOTS                          *          
*                                                                    *          
*  JUN08/92 (BU ) --- >TEMPORARY CHANGE TO EXCLUDE ACTIVITY FOR      *          
*                      STATION WOTV, PENDING SPOT-SIDE FIX           *          
*                                                                    *          
*  JUN15/92 (BU ) ---> TEMPORARY CHANGE (ABOVE) COMMENTED OUT, LEFT  *          
*                      IN PLACE FOR FUTURE NEEDS.                    *          
*                                                                    *          
*                                                                    *          
*  JUN15/92 (BU ) ---> FIX TRANSFER OF BUY COMMENT, P=(STRING)       *          
*                                                                    *          
*  SEP22/92 (BU ) ---> MG UPDATE:  CHECK EXACT DATE, THEN CHECK      *          
*                      MONDAY DATE FOR MATCH...                      *          
*                                                                    *          
*  OCT02/92 (BU ) ---> ADD CREDIT CHECK TO PULL OUT CREDIT SPOTS     *          
*                                                                    *          
*  NOV03/92 (BU ) ---> MG/CREDIT UPDATE:  AFTER EXACT/MONDAY DATE    *          
*                      CHECKS, CHECK WITHIN WEEK OF TABLE ENTRY      *          
*                                                                    *          
*  FEB03/93 (BU ) ---> UPGRADE TO MAKE REP FILE ASSIGNMENT SOFT      *          
*                                                                    *          
*  MAR24/93 (BU ) ---> PASS ADDRESSES TO SPBYGEN TO PERMIT DISPLAYS  *          
*                                                                    *          
*  APR15/93 (BU ) ---> FOR 'RETRANSFER ERROR#9', REPLACE MESSAGE     *          
*                      WITH 'NOTIFY DDS'                             *          
*                                                                    *          
*  JUL29/93 (BU ) ---> SKIP 'USED' RECORDS HIGHER UP IN LOGIC..      *          
*                                                                    *          
*  AUG04/93 (BU ) ---> NO 'CREDIT' MESSAGES TO RECAP LISTING.        *          
*                                                                    *          
*  DEC30/93 (BU ) ---> BYPASS RECORDS WHICH CAUSE 'STOPS'!!          *          
*                                                                    *          
*  JUL01/94 (BU ) ---> BYPASS ERROR (SPACE KEY) RECORDS!!            *          
*                                                                    *          
*  SEP15/94 (BU ) ---> WORKAREA REARRANGE:  OVERLAYING PORTIONS OF   *          
*                      SPWORKD, CLOBBERING VALUES                    *          
*                                                                    *          
*  DEC29/95 (BU ) ---> BYPASS RECORDS WITH NO STATION                *          
*                                                                    *          
*  SEP09/96 (BU ) ---> TEST VERSION:  RADNY DUMPS                    *          
*                                                                    *          
*  DEC09/96 (BU ) ---> ADD REPDEMO TO REP LIST SQUETAB               *          
*                                                                    *          
*  NOV13/97 (BU ) ---> ADD NPTVNY  TO REP LIST SQUETAB               *          
*                                                                    *          
*  MAR30/98 (BU ) ---> BUY COMMENT TRANSFER                          *          
*                                                                    *          
*  JUL28/99 (BU ) ---> MAKE RADNY/KRGNY RUNS SPLIT BASED ON QUESTOR  *          
*                                                                    *          
*  JAN27/00 (BU ) ---> EST DATE NG:                                  *          
*                                                                    *          
*  FEB03/00 (BU ) ---> SAVE ORIGINAL BUYLINE NUMBER IN SPIDDATA      *          
*                                                                    *          
*  FEB24/00 (BU ) ---> CANCELS: USE RBUYCHGI CODES TO RECOGNIZE      *          
*                                                                    *          
*  FEB08/01 (BU ) ---> CHANGE RETENTION CODE TO 120 HOURS            *          
*                                                                    *          
*  OCT22/01 (BU ) ---> SET UP FOR FOX SPORTS NET                     *          
*                                                                    *          
*  MAR20/02 (BU ) ---> PASS TRADE FLAG TO SPOT                       *          
*                                                                    *          
*  MAY22/03 (BU ) ---> UPDATE ERROR MESSAGES PER J. STEWART          *          
*                                                                    *          
*  SEP08/04 (BU ) ---> SET UP FOR LOTUS ENTRAVISION                  *          
*                                                                    *          
*  SEP10/04 (BU ) ---> LOTUS: SEND REPORT TO LA                      *          
*                                                                    *          
*                      ***  END TOMBSTONE  ***                       *          
**********************************************************************          
*                                                                               
SPR202   CSECT                                                                  
         NMOD1 0,SPR202,R9,R8,RR=R2                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,RA                                                    
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         ST    R2,RELO                                                          
         STM   R2,RC,SAVEREGS                                                   
         STM   R8,RA,SPR2R8                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         BAS   RE,MAINLINE                                                      
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE NTR1                                                                   
*                                                                               
         GOTO1 =A(INIT),DMCB,(RC),RR=RELO                                       
         BNZ   MAINERR                                                          
*                                                                               
         BAS   RE,DORESET          RESET TABLES, ADDRESSES                      
MAIN0020 EQU   *                                                                
         LA    RE,SORTREC                                                       
         L     RF,AOLDSORT                                                      
         LA    R1,SORTRECX-SORTREC                                              
         MOVE  ((RF),(R1)),(RE)                                                 
MAIN0030 EQU   *                                                                
         LA    RE,SORTREC                                                       
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL (RE),(RF)                                                        
*                                                                               
         GET   DATAIN              RETRIEVE RECOV FILE I/P RECORD               
         ST    R1,ASORTREC                                                      
         LR    RE,R1                                                            
         LA    RF,SORTREC                                                       
         LA    R1,SORTRECX-SORTREC                                              
         MOVE  ((RF),(R1)),(RE)    MOVE IT TO SORTREC                           
         CLC   SORTREC(L'SORTKEY),EOFKEY                                        
         BE    MAIN0240            END OF FILE REACHED                          
*                                                                               
*   TEST FOR SPECIAL CONTRACT NUMBER                                            
***      MVC   P+1(09),=C'SORTREC :'                                            
***      MVC   P+12(40),SORTREC                                                 
***      GOTO1 REPORT                                                           
*                                                                               
*   SORTKCON IS REVERSED, COMP OF CONTRACT NUMBER                               
*                                                                               
**       CLC   SORTKCON,=X'99118209'                                            
**       BE    TEST0040            PROCESS, BUT DON'T SET AS TRADE              
**       CLC   SORTKCON,=X'79118209'                                            
**       BNE   MAIN0030            SKIP ALL BUT                                 
**       MVC   P+1(09),=C'SORTDATA:'                                            
**       MVC   P+12(96),SORTDATA                                                
**       GOTO1 REPORT                                                           
**       USING RBUYREC,RF                                                       
**       LA    RF,SORTDATA                                                      
**       OI    RBUYFLG2,X'02'           TURN ON TRADE FLAG                      
**       DROP  RF                                                               
**       MVC   P+1(09),=C'TRADEFLG:'                                            
**       MVC   P+12(96),SORTDATA                                                
**       GOTO1 REPORT                                                           
TEST0040 EQU   *                                                                
*   TEST FOR SPECIAL END                                                        
*                                                                               
         CLC   SORTKEY(2),=C'  '   INVALID KEY?                                 
         BE    MAIN0030            YES - SKIP REC W/O OLDSORT RESET             
***      CLC   SORTKEY(2),=C'F8'   FOX SPORTS NET ORDER?                        
***      BE    MAIN0030            YES - SKIP REC W/O OLDSORT RESET             
         OC    SORTKSTA(4),SORTKSTA                                             
*                                  ANY STATION IN FIELD?                        
         BZ    MAIN0030            NO  - SKIP REC W/O OLDSORT RESET             
*                                                                               
*                                                                               
*   SPECIAL TEST SETUPS:  RADNY/K9 FSNNY/F8                                     
*                                                                               
*   TEST                                                                        
***      MVC   P+1(08),=C'QUESTOR='                                             
***      MVC   P+9(12),QUESTOR                                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   QUESTOR,SPACES      ANY VALUE IN QUESTOR?                        
         BE    MAIN0036            NO  - CHECK/SKIP RADNY/FSNNY                 
         CLC   =C'CONTYPE',QUESTOR                                              
         BE    MAIN0036            YES - ACCEPT THIS                            
         CLC   =C'SELNY',QUESTOR   SPECIAL SELTEL PQUEUE ROUTING?               
         BE    MAIN0038            YES - IGNORE FOLLOWING TESTS                 
         CLC   =C'RADNY',QUESTOR   YES - RADNY RUN?                             
         BNE   MAIN0032            NO  - CHECK FSNNY RUN                        
         CLC   =C'K9',SORTKAGY     YES - RADNY RECORD?                          
         BNE   MAIN0030            NO  - SKIP WITH NO RESET                     
         B     MAIN0038            YES - RADNY: PROCESS                         
MAIN0032 EQU   *                                                                
         CLC   =C'FSNNY',QUESTOR   FSNNY RUN?                                   
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - UNKNOWN RUN                            
         CLC   =C'F8',SORTKAGY     FSNNY RECORD?                                
         BNE   MAIN0030            NO  - SKIP WITH NO RESET                     
         B     MAIN0038            YES - FSNNY: PROCESS                         
*                                                                               
MAIN0036 EQU   *                                                                
         CLC   =C'K9',SORTKAGY     RADNY RECORD?                                
         BE    MAIN0030            YES - SKIP WITH NO RESET                     
         CLC   =C'F8',SORTKAGY     FSNNY RECORD?                                
         BE    MAIN0030            YES - SKIP WITH NO RESET                     
*                                  NO  - PROCESS THIS RECORD                    
MAIN0038 EQU   *                                                                
         CLI   USEDFLAG,0          RESTART IN PROGRESS?                         
         BE    MAIN0040            NO                                           
         CLC   SORTSTAT(4),USED    RECORD SEEN ALREADY?                         
         BE    MAIN0020            YES - SKIP IT                                
MAIN0040 EQU   *                                                                
         MVI   GOTDATA,X'FF'       SET 'GOT DATA' FLAG                          
*                                                                               
         L     R2,AOLDSORT         CHECK FOR DUPLICATES                         
*                                  ONLY 1ST RECORD IS KEPT                      
         CLC   SORTREC(SORTKDAT-SORTKEY),0(R2)                                  
         BE    MAIN0200            'DUPLICATE' RECORD                           
*                                                                               
         CLI   MED,X'00'           FIRST PASS?                                  
         BE    MAIN0060            YES                                          
*                                                                               
*   THE FOLLOWING SITUATION SHOULD ONLY ARISE DURING TESTING, WHERE             
*     DATA FOR MIXED MEDIA (BOTH TV AND RADIO) MIGHT COME THROUGH.              
*     IN SUCH A CASE, A BREAK ON MEDIA SHOULD RESET THE MEDIA STORES.           
*                                                                               
         CLC   MED,SORTKMED        SAME MEDIA?                                  
         BE    MAIN0060            YES                                          
         BAS   RE,NEWMEDIA         SET MEDIA AGAIN                              
         BZ    *+6                                                              
         DC    H'0'                UNRECOGNIZED MEDIA                           
*                                                                               
MAIN0060 EQU   *                                                                
         CLI   USEDFLAG,0                                                       
         BE    MAIN0080                                                         
         CLC   SORTSTAT-SORTREC(4,R2),USED                                      
         BE    MAIN0100                    NO BREAK ON USED RECS                
MAIN0080 EQU   *                                                                
         BAS   RE,BREAK1                                                        
         BNZ   MAINERR                                                          
*                                                                               
MAIN0100 EQU   *                                                                
         CLC   SORTREC(SORTKMED-SORTKEY),0(R2)                                  
         BE    MAIN0120                                                         
         GOTO1 =A(NEXTAGY),DMCB,(RC),RR=RELO                                    
***      BNZ   MAINERR                                                          
         BNZ   MAIN0020            **TEST**  SKIP RECORD                        
MAIN0120 EQU   *                                                                
****>>>  CLC   SORTREC(SORTKSTA-SORTKEY),0(R2)                                  
         CLC   SORTREC(SORTKLN-SORTKEY),0(R2)                                   
         BE    MAIN0140            EQUAL THROUGH ESTIMATE NUMBER                
         BAS   RE,NEXTEST          NOT EQUAL:  SET FOR NEXT ESTIMATE            
         BNZ   MAINERR                                                          
MAIN0140 EQU   *                                                                
         CLC   SORTREC(SORTKLN-SORTKEY),0(R2)                                   
         BE    MAIN0160            EQUAL THROUGH STATION                        
         BAS   RE,NEXTSTA          NOT EQUAL: SET FOR NEXT STATION              
MAIN0160 EQU   *                                                                
         CLI   USEDFLAG,0                                                       
         BE    MAIN0180                                                         
         CLC   SORTSTAT(4),USED                                                 
         BE    MAIN0020            SKIP USED RECORDS                            
MAIN0180 EQU   *                                                                
         BAS   RE,BLDTAB           ADD BUY TO TABLE                             
         BNZ   MAINERR                                                          
         BAS   RE,BLDPRINT                                                      
         BNZ   MAINERR                                                          
MAIN0200 EQU   *                                                                
         CLI   QOPT4,C'N'          NO UPDATES                                   
         BE    MAIN0220                                                         
         MVC   SORTSTAT(4),USED    SET RECORD TO 'USED'                         
         LA    RE,SORTREC                                                       
         L     RF,ASORTREC                                                      
         LA    R1,SORTRECX-SORTREC                                              
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*   TEST                                                                        
*        MVC   P(25),SORTREC                                                    
*        GOTO1 REPORT                                                           
*   END TEST                                                                    
*                                                                               
*****>>  PUTX  DATAIN              REWRITE IN PLACE                             
MAIN0220 EQU   *                                                                
         B     MAIN0020            GO BACK FOR NEXT INPUT                       
MAIN0240 EQU   *                   TRANSFER HERE IN FILE EOF                    
         CLI   GOTDATA,0                                                        
         BNE   MAIN0280                                                         
         CLI   HEADIN,0                                                         
         BNE   MAIN0260                                                         
         MVC   RUNERROR(L'EMPTYF),EMPTYF                                        
         B     MAINERR                                                          
MAIN0260 EQU   *                                                                
         MVC   RUNERROR(L'NODATA),NODATA                                        
         B     MAINERR                                                          
MAIN0280 EQU   *                                                                
         BAS   RE,ENDRUN                                                        
         BNZ   MAINERR                                                          
*                                                                               
*        END MAINLINE / FINISH RUN                                              
*                                                                               
MAINGOOD EQU   *                                                                
         BAS   RE,CLOSEUP                                                       
         MVI   RCSUBPRG,3                                                       
         MVI   INRECAP,1                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(21),=C'*** END OF REPORT ***'                                  
         PRINT GEN                                                              
         GOTO1 REPORT                                                           
         PRINT NOGEN                                                            
         B     TESTGOOD                                                         
MAINERR  EQU   *                                                                
         BAS   RE,CLOSEUP                                                       
         MVI   RCSUBPRG,3                                                       
         MVI   INRECAP,1                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(40),=C'* * * ERROR ENCOUNTERED DURING RUN * * *'               
         GOTO1 REPORT                                                           
         MVC   P(60),RUNERROR                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=C'* * * RUN TERMINATED * * *'                             
         GOTO1 REPORT                                                           
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        NEWMEDIA - A BREAK ON MEDIA ALONE HAS BEEN ENCOUNTERED.                
*                                                                               
NEWMEDIA NTR1                                                                   
*                                                                               
         GOTO1 MEDGET,DMCB,(SORTKMED,SORTKAGY),DATAMGR,WORK                     
         CLI   DM3,X'FF'                                                        
         BNE   NMED50                                                           
         MVC   RUNERROR(L'BADMED),BADMED                                        
         B     TESTBAD                                                          
NMED50   EQU   *                                                                
         MVC   BAGYMD(1),WORK                                                   
         MVC   MED(1),SORTKMED                                                  
         MVC   MEDNM(10),WORK+1                                                 
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        BREAK1 --- PROCESS DATA BREAKS FOR PRINTING                            
*                                                                               
BREAK1   NTR1                                                                   
*                                                                               
         L     R2,AOLDSORT                                                      
         OC    0(2,R2),0(R2)                     FIRST TIME?                    
         BZ    BRK1GOOD                          YES                            
         CLC   SORTKEY(SORTKLN-SORTKEY),0(R2)    CONTRACT BREAK?                
         BNE   BRK105A                                                          
         CLC   SORTKCON(L'SORTKCON),SORTKCON-SORTKEY(R2)                        
         BE    BRK1GOOD                          NO, NO BREAKS AT ALL           
BRK105A  EQU   *                                                                
         XC    DM2,DM2                                                          
         CLI   QOPT4,C'N'                                                       
         BNE   BRK110                                                           
         MVC   DM2(4),=C'NOWR'     PASS NO-WRITE OPTION TO BUY GEN              
BRK110   EQU   *                                                                
         XC    DM5,DM5                                                          
         CLI   QOPT2,C'Y'          DO SPBYGEN DISPLAYS?                         
         BNE   BRK110A                                                          
         MVC   DM5(4),=C'DISP'     PASS SPBYGEN DISPLAY OPTION                  
BRK110A  EQU   *                                                                
         CLI   QOPT2,C'D'          DO SPBYGEN DUMP?                             
         BNE   BRK110B                                                          
         MVC   DM5(4),=C'DUMP'     PASS SPBYGEN DUMP OPTION                     
BRK110B  EQU   *                                                                
         CLI   QOPT2,C'B'          DO SPBYGEN BUYS?                             
         BNE   BRK110C                                                          
         MVC   DM5(4),=C'BUYS'     PASS SPBYGEN BUYS OPTION                     
BRK110C  EQU   *                                                                
         ZICM  R5,STAERR,1         STATION ERRORS AT BREAK?                     
         BNZ   BRK111              YES - SKIP BUYGEN FOR THIS BLOCK             
         ZICM  R5,ESTERR,1         ESTIMATE ERRORS AT BREAK?                    
         BZ    BRK115              NO  - OK TO BUYGEN THIS BLOCK                
BRK111   EQU   *                                                                
         L     RE,ABLDTAB          CYCLE BLOCK INSERTING ERROR CODE             
         L     RF,ABLDTABC                                                      
         USING SPBUYDTD,RE                                                      
BRK112   EQU   *                                                                
         CR    RE,RF                                                            
         BE    BRK112A                                                          
         STC   R5,SPERROUT                       SET PASSED ERROR               
         LA    RE,SPDETBKL(RE)                                                  
         B     BRK112                                                           
         DROP  RE                                                               
BRK112A  EQU   *                                                                
         MVI   STAERR,0            RESET ERROR FLAGS FOR NEXT PASS              
         MVI   ESTERR,0                                                         
         GOTO1 =A(BYGENERR),DMCB,(RC),RR=RELO                                   
         B     BRK125                                                           
BRK115   EQU   *                                                                
         MVC   DM3,REPORT          A(REPORT)                                    
         PRINT GEN                                                              
         GOTO1 =V(SPBYGEN),DMCB,SPBLOCK,,,P,RR=RELO                             
         PRINT NOGEN                                                            
         GOTO1 =A(BYGENERR),DMCB,(RC),RR=RELO    LINE-BY-LINE ERRS              
         BAS   RE,UPDATE                                                        
         BNZ   BRK1BAD                                                          
BRK125   EQU   *                                                                
         GOTO1 =A(DOPRINT),DMCB,(RC),RR=RELO                                    
****>>>  BAS   RE,DOPRINT                                                       
         BAS   RE,DORESET                                                       
         BAS   RE,DOCONCHG                       MUST ALSO BE CON BRK           
         CLC   SORTREC(SORTKLN-SORTKEY),0(R2)    STATION BREAK?                 
         BE    BRK130                            NO, CHECK CON BREAK            
         BAS   RE,DOSTACHG                       YES, PRINT IT                  
BRK130   EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(132),P2                                                        
         MVC   P2(132),SPACES                                                   
         GOTO1 REPORT                                                           
BRK150   EQU   *                                                                
         CLC   SORTREC(SORTKSTA-SORTKEY),0(R2)   ESTIMATE BREAK?                
         BE    BRK1GOOD                          NO, ALL DONE                   
         ZIC   R5,ALLOWLIN                                                      
         MVI   ALLOWLIN,4                                                       
         GOTO1 REPORT                                                           
         BAS   RE,DOESTCHG                                                      
         BAS   RE,DOADVPRD                                                      
         GOTO1 REPORT                                                           
         STC   R5,ALLOWLIN                                                      
BRK160   EQU   *                                                                
         CLC   SORTREC(SORTKMED-SORTKEY),0(R2)   AGENCY BREAK?                  
         BE    BRK170                            NO, ALL DONE                   
         GOTO1 REPORT                                                           
         BAS   RE,DOAGYCHG                                                      
         GOTO1 REPORT                                                           
         BAS   RE,ERRRECAP                                                      
BRK170   EQU   *                                                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
BRK1GOOD EQU   *                                                                
         B     TESTGOOD                                                         
BRK1BAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        UPDATE  --- UPDATE THE PRINT LINES FOR FILE ADDITIONS                  
*                    WRITE RECORDS TO THE REPOUT FILE                           
*                                                                               
UPDATE   NTR1                                                                   
*                                                                               
         L     R2,APNTTAB                                                       
         USING PLINED,R2                                                        
         L     R3,APNTTABC                                                      
         L     R4,ABLDTAB                                                       
         USING SPBUYDTD,R4                                                      
*                                                                               
UDAT10   EQU   *                                                                
         CR    R2,R3                                                            
         BE    UDAT90                                                           
         CLI   SPBUYD,C'*'                                                      
         BNE   UDAT20                                                           
         EDIT  (B1,SPBUYD+1),(3,PSPOTLIN)                                       
UDAT20   EQU   *                                                                
         CLI   SPERROUT,0                   NO REP UPDATE ON ERROR              
         BNE   UDAT40                                                           
         CLI   SPMEMO,EREQBCAN              NO REP UPDATE ON CANCEL             
         BE    UDAT40                                                           
         CLI   SPMEMO,EREQCANP              NO REP UPDATE ON CANCEL             
*                                              WITH PAID SPOTS                  
         BE    UDAT40                                                           
         L     R5,SPTRACED                                                      
         L     R6,AOUTREC                                                       
         MVC   0(11,R6),0(R5)                                                   
         MVC   11(1,R6),SPBUYD+1                                                
         MVC   12(3,R6),SPXFERD                                                 
         MVC   15(4,R6),SPXFERT                                                 
         PUT   REPOUT,(6)                                                       
         LR    RE,R6                                                            
         LA    RF,1000                                                          
         XCEFL                                                                  
UDAT40   EQU   *                                                                
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R4,SPDETBKL(R4)                                                  
         B     UDAT10                                                           
*                                                                               
UDAT90   EQU   *                                                                
*                                                                               
UDATGOOD EQU   *                                                                
         B     TESTGOOD                                                         
UDATBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 2                                                                
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*        DORESET --- RESET ALL THE TABLES                                       
*                                                                               
DORESET  NTR1                                                                   
*                                                                               
         L     RE,ABLDTAB                                                       
         ST    RE,ABLDTABC                                                      
         L     RF,=F'85000'                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,ABSPECS                                                       
         ST    RE,ABSPECSC                                                      
         L     RF,=F'39000'                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,ATRACET                                                       
         ST    RE,ATRACETC                                                      
         L     RF,=F'3900'                                                      
         XCEFL                                                                  
*                                                                               
         L     RE,APNTTAB                                                       
         ST    RE,APNTTABC                                                      
         L     RF,=F'100000'                                                    
         XCEFL                                                                  
*                                                                               
         L     RE,AOUTREC                                                       
         LA    RF,1000                                                          
         XCEFL                                                                  
*                                                                               
RSETGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        BLDTAB --- BUILD A DETAIL TABLE ENTRY FROM THE CURRENT RECORD          
*                                                                               
BLDTAB   NTR1                                                                   
*                                                                               
         LA    R2,SORTDATA                                                      
         USING RBUYREC,R2                                                       
         CLI   RBUYCHGI,C'C'       BUY CANCELLED?                               
         BE    BTAB0005            YES - CANCEL THE SPOTS                       
         CLI   RBUYCHGI+1,C'C'     BUY CANCELLED?                               
         BE    BTAB0005            YES - CANCEL THE SPOTS                       
         TM    RBUYCNTL,X'80'                                                   
         BZ    BTAB10                                                           
BTAB0005 EQU   *                                                                
         BAS   RE,DOCANCEL         REMOVE XCL'D, DEL'D SPOTS FROM               
*                                    SPOT RECORD                                
         B     BTABGOOD                                                         
BTAB10   EQU   *                                                                
         L     R3,ABLDTABC                                                      
         USING SPBUYDTD,R3                                                      
*                                                                               
         NI    SPFLAGS,X'FF'-X'80'      TURN OFF TRADE FLAG                     
         TM    RBUYFLG2,X'02'           TRADE BUY?                              
         BNO   BTAB11                   NO                                      
         OI    SPFLAGS,X'80'            YES - SET 'TRADE' INDICATOR             
BTAB11   EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'08',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BE    BTAB15                                                           
         MVC   RUNERROR(L'SPOTMISS),SPOTMISS                                    
         B     BTABBAD                                                          
BTAB15   EQU   *                                                                
         L     R4,DM4                                                           
         USING RBUYSPEL,R4                                                      
*                                                                               
         CLI   SORTKLN,0                                                        
         BE    BTAB20                                                           
         MVI   SPBUYD+0,X'00'                                                   
         MVC   SPBUYD+1(1),SORTKLN                                              
         MVI   SPBUYD+2,X'01'                                                   
         OC    RBUYSPDT,RBUYSPDT                                                
         BNZ   BTAB18                                                           
         MVC   SPXFERD,=X'FFFFFF'                                               
         B     BTAB20                                                           
BTAB18   EQU   *                                                                
         MVC   SPXFERD,RBUYSPDT                                                 
         MVC   SPXFERT,RBUYSPTM                                                 
BTAB20   EQU   *                                                                
* -----  SCHEDULE NUMBER???                                                     
         MVC   SPMSLEN,RBUYSPP1                                                 
         DROP  R4                                                               
*                                                                               
         CLI   RBUYDPT,0                                                        
         BE    BTAB21A                                                          
         CLI   RBUYDPT,C' '                                                     
         BE    BTAB21A                                                          
         MVC   SPDAYPT,RBUYDPT                                                  
         B     BTAB21B                                                          
BTAB21A  EQU   *                                                                
         MVI   SPDAYPT,C'R'                                                     
BTAB21B  EQU   *                                                                
*                                                                               
         MVC   SPSPWEEK,RBUYNW                                                  
         TM    RBUYDUR,X'80'                                                    
         BNZ   BTAB22                                                           
         CLC   RBUYDUR,=H'120'                                                  
         BH    BTAB22                                                           
         MVC   SPLEN(1),RBUYDUR+1                                               
         B     BTAB25                                                           
BTAB22   EQU   *                                                                
         MVI   SPMEMO,EREQLONG                SPOT LEN WARNING MSG              
         MVI   SPLEN,120                                                        
BTAB25   EQU   *                                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R2)),(0,0),(0,0)               
         L     R4,DM4                                                           
         USING RBUYDTEL,R4                                                      
         MVC   SPSTADAT,RBUYDTST   FIRST EFF DATE ELEMENT                       
         MVC   SPENDDAT,RBUYDTED                                                
         MVC   SPWEEKS,RBUYDTWK                                                 
         GOTO1 XLATALT,DMCB,RBUYDTIN,SPALT                                      
         BZ    BTAB30                                                           
         MVI   SPMEMO,EREQWEEK     ALT-WEEK WARNING MSG                         
BTAB30   EQU   *                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),X'03'         MORE EFF DATE ELEMENTS?                      
         BNE   BTAB40 NO  -                                                     
         MVC   SPENDDAT,RBUYDTED   YES - REPLACE END DATE WITH                  
*                                          NEW END DATE                         
         ZIC   RE,SPWEEKS                                                       
         ZIC   RF,RBUYDTWK                                                      
         AR    RE,RF                                                            
         STC   RE,SPWEEKS          ACCUMULATE NUMBER OF WEEKS                   
         B     BTAB30                                                           
*                                                                               
*   NOTE:  RESULTS REFLECT BUY AS START/END ACROSS ALL EFFECTIVE                
*   DATES, WITH NUMBER OF WEEKS SUM OF WEEKS WITHIN EFF DATE ELTS               
*                                                                               
BTAB40   EQU   *                                                                
         DROP  R4                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R2)),(0,0),(0,0)               
         L     R4,DM4                                                           
         USING RBUYDYEL,R4                                                      
         MVC   SPDAY,RBUYDAYS                                                   
         MVC   SPSEDAY,RBUYDYIN                                                 
         MVC   SPSTATIM,RBUYDYT1                                                
         MVC   SPENDTIM,RBUYDYT2                                                
         DROP  R4                                                               
         ZIC   RF,1(R4)               SET 'ERROR' MSG FOR MULTIPLE              
         AR    R4,RF                  DAY AND TIMES                             
         CLI   0(R4),X'02'                                                      
         BNE   BTAB70                                                           
         MVI   SPMEMO,EREQDTS                                                   
BTAB70   EQU   *                                                                
         MVC   SPCOST(3),RBUYCOS+1                                              
*                                                                               
         MVI   SPTRACEL,11                                                      
         L     R4,ATRACETC                                                      
         ST    R4,SPTRACED                                                      
         MVC   0(11,R4),RBUYKREP   LOAD TRACE INFORMATION                       
         LA    R4,11(R4)                                                        
         ST    R4,ATRACETC                                                      
*                                                                               
         ZAP   WORK(5),=P'99999999'                                             
         PACK  WORK+10(1),SORTKCON+3(1)     INVERT THE SEQUENCE                 
         PACK  WORK+11(1),SORTKCON+2(1)     OF THE DIGITS                       
         PACK  WORK+12(1),SORTKCON+1(1)                                         
         PACK  WORK+13(1),SORTKCON+0(1)     MOVE IN SIGN AND                    
         MVI   WORK+14,X'0C'                SHIFT ONE DECIMAL PLACE             
         SRP   WORK+10(5),64-1,0            TO RIGHT                            
         SP    WORK(5),WORK+10(5)           BEFORE SUBTRACTING FROM             
         SRP   WORK(5),1,0                  NINES AND SHIFTING 1 TO             
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),WORK(4)                                                 
         EDIT  (P5,DUB+3),(8,SPIDDATA),FILL=0,ZERO=NOBLANK                      
         EDIT  (1,SORTKBUY+4),(3,SPIDDATA+09),FILL=0,ZERO=NOBLANK               
         MVC   SPIDSUP(2),RBUYKREP                                              
*                                                                               
*   TEST PRINT                                                                  
*        MVC   P+1(09),=C'SPIDDATA='                                            
*        MVC   P+10(18),SPIDDATA                                                
*        GOTO1 REPORT                                                           
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
         LA    RF,SPBYCOM1         SET A(1ST COMMENT IN TABLE ENTRY)            
         ST    RF,ABUYCOM          SAVE OFF ADDR                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'04',(R2)),(0,0),(0,0)               
         CLI   DM4,0               BUY COMMENT ELEMENT FOUND?                   
         BNE   BTAB80              NO  - EXIT THIS SECTION                      
         L     R5,DM4                                                           
BTAB75   EQU   *                   YES                                          
         CLC   2(2,R5),=C'P='      PROGRAM NAME IN BUY COMMENT?                 
         BNE   BTAB78              NO  -                                        
         OC    SPDESC,SPDESC       YES - PROGRAM NAME FOUND ALREADY?            
         BNZ   BTAB80              YES - DON'T SAVE AGAIN                       
         ZIC   RF,1(R5)            NO  - MOVE PROG NAME TO DESCRIPT             
         SH    RF,=H'3'                                                         
         CH    RF,=H'17'                                                        
         BNH   BTAB76                                                           
         LA    RF,17               SET MAX LENGTH                               
BTAB76   EQU   *                                                                
         EX    RF,BTABEX1          MOVE BY LENGTH                               
         OC    SPDESC,SPACES       FLOAT IN SPACES                              
         B     BTAB79              GO CHECK NEXT ELEMENT                        
BTABEX1  MVC   SPDESC(0),4(R5)                                                  
BTAB78   EQU   *                   MOVE BUY COMMENT TO TABLE                    
         ZIC   RF,1(R5)            TAKE ELEMENT LENGTH                          
         SH    RF,=H'3'            SUBTRACT L(CNTRL+1 FOR EX)                   
         L     RE,ABUYCOM          SET A(RECEIVING FIELD)                       
         EX    RF,BTAB78M          MOVE BY LENGTH                               
         OC    0(60,RE),0(RE)      FLOAT IN SPACES                              
         LA    RE,60(RE)           BUMP TO NEXT COMMENT FIELD                   
         ST    RE,ABUYCOM          REPLACE IT                                   
         B     BTAB79              LOOK FOR NEXT ELEMENT                        
BTAB78M  EQU   *                                                                
         MVC   0(0,RE),2(R5)       MOVE BY LENGTH                               
BTAB79   EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         CLI   0(R5),X'04'                                                      
         BE    BTAB75                                                           
BTAB80   EQU   *                                                                
*                                                                               
         L     R4,ABSPECSC                                                      
         ST    R4,SPSPECL                                                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   BTAB200                                                          
         L     R5,DM4                                                           
         USING RBUYDTEL,R5                                                      
BTAB100  EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
         MVC   WORK+6(6),WORK      SAVE BUY START DATE                          
         MVI   ALTFLAG,0                                                        
         CLI   RBUYDTLN,5          PLAN BUY LINE?                               
         BE    BTAB105A            YES, DO THAT                                 
         ZIC   R6,RBUYDTWK                                                      
         LTR   R6,R6                                                            
         BZ    BTAB200             # OF WEEKS = ZERO                            
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    BTAB110             NO                                           
         MVI   ALTFLAG,1           YES - SET FLAG                               
         B     BTAB110                                                          
BTAB105A EQU   *                   BUY PLAN                                     
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'01',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   BTAB200                                                          
         L     RF,DM4                                                           
         ZIC   R6,RBUYNW                                                        
         LTR   R6,R6                                                            
         BZ    BTAB200             # WEEK = ZERO: FINISHED                      
*                                                                               
*  NOTE:  IF BUY PLAN, AND IT DROPS THROUGH HERE, 'RBUYDTNW' IS                 
*    NOT SET - IN FACT, IT DIDN'T EXIST IN THE ELEMENT.  THIS                   
*    COULD BE A PROBLEM.                                                        
*                                                                               
BTAB110  EQU   *                                                                
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,0(R4))                                 
         MVC   2(1,R4),RBUYDTNW                                                 
         DROP  R5                                                               
         LA    R4,3(R4)                                                         
         CLI   ALTFLAG,0                      EVERY WEEK?                       
         BNZ   BTAB150                        NO, MAKE IT ALTERNATING           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,7                                         
         B     BTAB160                                                          
BTAB150  EQU   *                                                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,14                                        
BTAB160  EQU   *                                                                
         BCT   R6,BTAB110                                                       
         ZIC   R6,1(R5)                                                         
         AR    R5,R6                                                            
         CLI   0(R5),X'03'                                                      
         BE    BTAB100                                                          
         GOTO1 MGAPPLY,DMCB,(R2)   APPLY BUY MISSED ELTS (MG'S)                 
         GOTO1 =A(CREDITS),DMCB,(RC),(RA),(R2),RR=RELO                          
*                                  APPLY BUY MISSED ELTS (CREDITS)              
BTAB200  EQU   *                                                                
         LA    R4,3(R4)                      END WITH A ZERO ENTRY              
         ST    R4,ABSPECSC                                                      
*                                                                               
         LA    R3,SPDETBKL(R3)                                                  
         ST    R3,ABLDTABC                                                      
*                                                                               
*        BLDTAB EXIT                                                            
*                                                                               
BTABGOOD EQU   *                                                                
         B     TESTGOOD                                                         
BTABBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 2                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*  AFTER EFFECTIVE DATES/SPOTS PER WEEK HAVE BEEN TABLED, SCAN                  
*    BUY RECORD FOR X'06' (MG BUY MISSED) ELEMENTS.  WHEN FOUND, FIND           
*    DATE IN TABLE, SUBTRACT NUMBER OF SPOTS MISSED FROM #/WEEK                 
*                                                                               
MGAPPLY  NTR1                                                                   
         L     R2,0(R1)            A(BUY RECORD)                                
         L     R3,ABLDTABC                                                      
         USING SPBUYDTD,R3                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'06',(R2)),(0,0),(0,0)               
         CLI   DM4,0               ANY ELEMENT FOUND?                           
         BNE   MGAP0020            NO  - FINISHED                               
         L     R4,DM4              A(1ST ELEMENT)                               
         USING RBUYMSEL,R4                                                      
MGAP0004 EQU   *                                                                
         L     R5,SPSPECL          A(DATE/#SPOTS PER WEEK TABLE)                
*                                                                               
*   SET UP DATE FOR EXACT DATE MATCH: CONVERT TO COMPRESSED FORMAT              
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYMSDT),(2,WORK+24)                             
*                                                                               
*   CONVERT MISSED DATE TO YYMMDD EBCDIC                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYMSDT),(0,WORK+6)                              
*                                                                               
*   GET DAY OF WEEK OF MISSED DATE                                              
*                                                                               
         GOTO1 =V(GETDAY),DMCB,WORK+6,WORK+12                                   
         MVC   WORK+12(6),WORK+6   LOAD FOR FINAL CONVERT                       
*                                       IF DATE IS A MONDAY                     
         CLI   DMCB,1              IS IT MONDAY?                                
         BE    MGAP0006            YES - USE IT AS IS                           
         ZIC   RF,DMCB             NO  - GET MONDAY DATE FROM IT                
         BCTR  RF,0                SUBTRACT 1 FROM 1                            
         LNR   RF,RF               MAKE IT NEGATIVE                             
*                                                                               
*   USE ADJUSTED DAY TO CALCULATE MONDAY OF WEEK                                
*                                                                               
         GOTO1 =V(ADDAY),DMCB,WORK+6,WORK+12,(RF)                               
MGAP0006 EQU   *                                                                
*                                                                               
*   GENERATE DATE IN COMPRESSED FORMAT                                          
*                                                                               
         GOTO1 DATCON,DMCB,WORK+12,(2,WORK)                                     
MGAP0008 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE?                                
         BE    MGAP0016            YES - LOOK FOR NEXT X'06'                    
         CLC   WORK(2),0(R5)       SAME DATE (MONDAY-ORIENTED)?                 
         BE    MGAP0012            YES - PROCESS                                
         CLC   WORK+24(2),0(R5)    SAME DATE (ORIGINAL)?                        
         BE    MGAP0012            YES - PROCESS                                
*                                                                               
*   EXACT DATE MATCHES HAVE FAILED.  NEXT CHECK WILL SEE IF DATE                
*      OF SPOT IS WITHIN WEEK RANGE OF TABLE ENTRY BY:                          
*        1.    CONVERTING TABLE DATE TO YYMMDD EBCDIC                           
*        2.    ADDING 6 DAYS TO DATE TO GIVE ONE-WEEK SPREAD                    
*        3.    CONVERTING NEW DATE BACK TO COMPRESSED FORMAT                    
*        4.    COMPARING ORIGINAL MAKE GOOD DATE TO THIS RANGE                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R5)),(0,WORK+30)                                
         GOTO1 =V(ADDAY),DMCB,WORK+30,WORK+36,6                                 
         GOTO1 DATCON,DMCB,(0,WORK+36),(2,WORK+30)                              
*                                                                               
         CLC   WORK+24(2),0(R5)    ORIG DATE VS WEEK START                      
         BL    MGAP0010            EARLIER - REJECTED                           
         CLC   WORK+24(2),WORK+30  ORIG DATE VS WEEK START+6 DAYS               
         BNH   MGAP0012            WITHIN - ACCEPT                              
MGAP0010 EQU   *                                                                
         LA    R5,3(R5)            NOT FOUND -                                  
         B     MGAP0008               GO BACK FOR NEXT                          
MGAP0012 EQU   *                                                                
         ZIC   RF,2(R5)            #SPOTS/WEEK FROM TABLE                       
         ZIC   RE,RBUYMSSP         MISSED #SPOTS FROM X'06'                     
         SR    RF,RE               SUBTRACT MISSED FROM TOTAL                   
         STC   RF,2(R5)            PUT IT BACK IN TABLE                         
MGAP0016 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT RECORD ELEMENT                  
         AR    R4,RF                                                            
         CLI   0(R4),X'06'         ANOTHER X'06'?                               
         BE    MGAP0004            YES - LOOP THROUGH TABLE                     
MGAP0020 EQU   *                                                                
         B     TESTGOOD            EXIT ROUTINE                                 
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        DOCANCEL --- PROCESS A DELETED/CANCEL REP BUY LINE                     
*                                                                               
DOCANCEL NTR1                                                                   
*                                                                               
         LA    R2,SORTDATA                                                      
         USING RBUYREC,R2                                                       
         L     R3,ABLDTABC                                                      
         USING SPBUYDTD,R3                                                      
         MVI   SPBUYD+0,X'00'                                                   
         MVI   SPBUYD+2,X'01'                                                   
         MVI   SPMEMO,EREQBCAN     ERROR MESSAGE SAYS                           
*                                    'BUY CANCELLED'                            
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'08',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BE    DCAN10                                                           
         MVC   RUNERROR(L'SPOTMISS),SPOTMISS                                    
         B     DCANBAD                                                          
DCAN10   EQU   *                                                                
         L     R4,DM4                                                           
         USING RBUYSPEL,R4                                                      
*                                                                               
         CLI   RBUYSPL#,0             NOTHING TO DO IF NOT TRANSFERED           
         BE    DCANGOOD                                                         
         LA    R5,SPBLOCK                                                       
         USING SPBUYBLK,R5                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BUYKEY,R6                                                        
         MVC   BUYKAM,SPAM                                                      
         MVC   BUYKCLT,SPCLT                                                    
         MVC   BUYKPRD,SPPOLP#                                                  
         MVC   BUYMSTA,SPMSTA                                                   
         MVC   BUYKEST,SPEST                                                    
         MVI   BUYKBUY+0,X'00'                                                  
         MVC   BUYKBUY+1(1),RBUYSPL#                                            
         MVC   SPBUYD+1(1),RBUYSPL#                                             
         MVI   BUYKBUY+2,X'01'                                                  
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DCAN30                                                           
         MVI   SPERROUT,EREQCANE                                                
         B     DCANBAD                                                          
DCAN30   EQU   *                                                                
         GOTO1 GET                                                              
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'0B',AREC),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   DCANGOOD                                                         
         L     R6,DM4                                                           
         USING REGELEM,R6                                                       
DCAN50   EQU   *                                                                
         OC    RPAY,RPAY           SKIP THIS SPOT IF PAID                       
         BNZ   DCAN54                                                           
         CLI   RLEN,X'0E'                                                       
         BL    DCAN52                                                           
*                                                                               
*   REMOVED PER MH.  9/15/94                                                    
*                                                                               
****>>>  OC    RPBILL,RPBILL       SKIP THIS SPOT IF BILLED                     
****>>>  BNZ   DCAN54                                                           
DCAN52   EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',SPTFILE),(X'0B',AREC),(2,2(R6))                 
         CLI   DM4,0                                                            
         BE    DCAN60                                                           
         DC    H'0'                                                             
DCAN54   EQU   *                                                                
         MVI   SPMEMO,EREQCANP     CHANGE ERROR MESSAGE TO                      
*                                    'SPOTS PAID: BUY CANCELLED'                
DCAN60   EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),X'0B'                                                      
         BE    DCAN50                                                           
         DROP  R6                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'01',AREC),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BE    DCAN70                                                           
         DC    H'0'                                                             
DCAN70   EQU   *                                                                
         L     R6,DM4                                                           
         USING BDELEM,R6                                                        
         MVC   BDCHG,TODAYB                                                     
         MVI   BDWHY,X'00'         RESET 1ST 2 LAST CHANGE INDICATORS           
         MVI   BDWHY2,X'00'                                                     
         MVI   BDWHY3,X'80'        SET SCHED CHANGE                             
         DROP  R6                                                               
DCAN90   EQU   *                                                                
         CLI   QOPT4,C'N'                                                       
         BE    DCAN99                     SKIP FILE WRITE                       
         GOTO1 PUT                 REWRITE MODIFIED SPOT RECORD                 
*                                    WITH SPOTS REMOVED.                        
DCAN99   EQU   *                                                                
*                                                                               
*        DOCANCEL EXIT                                                          
*                                                                               
DCANGOOD EQU   *                                                                
         LA    R3,SPDETBKL(R3)                                                  
         ST    R3,ABLDTABC                                                      
         B     TESTGOOD                                                         
DCANBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 2                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        XLATALT --- TRANSLATE REP ALTERNATE WEEK TO SPOT                       
*        NOTE:  AN INCORRECT CHANGE TO THE FLAG FIELD IN REP                    
*          RESULTED IN THE FAILURE TO SET THE 'X'80'' BIT FOR                   
*          'EVERY WEEK' IN SOME CASES.  THIS WILL BE HANDLED AS                 
*          IF 'X'80'' WERE ENTERED.                                             
*                                                                               
*                                                                               
*        P1     =    A(REP INPUT)                                               
*        P2     =    A(SPOT OUTPUT)                                             
*                                                                               
XLATALT  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         TM    0(R2),X'80'                                                      
         BZ    XALT10                                                           
         MVI   0(R3),C'O'                                                       
         B     XALTGOOD                                                         
XALT10   EQU   *                                                                
         TM    0(R2),X'40'                                                      
         BZ    XALT20                                                           
         MVI   0(R3),C'A'                                                       
         B     XALTGOOD                                                         
XALT20   EQU   *                                                                
         MVI   0(R3),C'O'          TREAT AS X'80'                               
         B     XALTGOOD                                                         
**       MVI   0(R3),X'00'                                                      
**       B     XALTBAD                                                          
*                                                                               
XALTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
XALTBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        BLDPRINT --- BUILD A DETAIL PRINT LINE FROM THE CURRENT RECORD         
*                                                                               
BLDPRINT NTR1                                                                   
*                                                                               
         LA    R2,SORTDATA                                                      
         USING RBUYREC,R2                                                       
         L     R3,ABLDTABC                                                      
         USING SPBUYDTD,R3                                                      
         LA    RF,SPDETBKL                                                      
         LCR   RF,RF                                                            
         AR    R3,RF                                                            
         L     R4,APNTTABC                                                      
         USING PLINED,R4                                                        
*                                                                               
         MVC   0(132,R4),SPACES                                                 
*                                                                               
         MVC   PSTATION,STAPRINT                                                
         OC    SPBUYD,SPBUYD                                                    
         BZ    BPNT10                                                           
         MVI   PUPDATE,C'*'                                                     
BPNT10   EQU   *                                                                
         EDIT  (B1,SPBUYD+1),(3,PSPOTLIN)                                       
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(X'13',SPSTADAT),(X'85',WORK)                        
         MVI   WORK+12,C'-'                                                     
         MVC   WORK+13(5),WORK+18                                               
         MVC   PSPOTDAT,WORK+7                                                  
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         TM    RBUYCNTL,X'80'                 SKIP SPOT SPOTS AND $'S           
         BNZ   BPNT30                          IF CANCELLED                     
         L     R5,ABSPECSC                                                      
         C     R5,ABSPECS                                                       
         BE    BPNT20                                                           
         SH    R5,=H'3'                                                         
BPNT15   EQU   *                                                                
         C     R5,ABSPECS                                                       
         BE    BPNT20                                                           
         SH    R5,=H'3'                                                         
         OC    0(3,R5),0(R5)                                                    
         BZ    BPNT20                                                           
         ZIC   RF,2(R5)                                                         
         AR    R7,RF                                                            
         B     BPNT15                                                           
BPNT20   EQU   *                                                                
         EDIT  (R7),(5,PSPOTSPT)                                                
         LR    RF,R7                                                            
*                                                                               
*  DON'T ACCUMULATE THE TOTAL SPOTS FOR THIS LINE UNTIL YOU KNOW                
*    WHETHER THE SPOTS ARE POSITIVE OR NEGATIVE (IE, A CREDIT)                  
*                                                                               
         ZICM  R5,SPCOST,3         CHECK COST FOR NEGATIVE                      
         SLL   R5,8                SHIFT VALUE TO HIGH-ORDER BYTE               
         SRA   R5,8                RESET VALUE WITH SIGN IF NEGATIVE            
         LTR   R5,R5               IS COST NEGATIVE?                            
         BNM   BPNT25              NO                                           
*                                                                               
         LNR   RF,RF               GET NEGATIVE VALUE OF                        
*                                    # OF SPOTS FOR SUBTRACTION                 
BPNT25   EQU   *                                                                
         A     RF,SPOTSTAS         NOW ACCUMULATE # OF SPOTS                    
         ST    RF,SPOTSTAS                                                      
         MR    R6,R5                                                            
         EDIT  (R7),(12,PSPOTDOL),2,MINUS=YES                                   
         LR    RF,R7                                                            
         A     RF,SPOTSTAD                                                      
         ST    RF,SPOTSTAD                                                      
BPNT30   EQU   *                                                                
         MVC   PREPREP,RBUYKREP                                                 
         MVC   PREPAGY,REPAGYC                                                  
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'08',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   BPNTBAD                                                          
         L     R5,DM4                                                           
         USING RBUYSPEL,R5                                                      
         LA    R6,PREPSTA                                                       
         MVC   0(4,R6),RBUYSPST                                                 
         CLI   3(R6),C' '                                                       
         BE    BPNT50                                                           
         LA    R6,1(R6)                                                         
BPNT50   EQU   *                                                                
         MVI   3(R6),C'-'                                                       
         MVC   4(1,R6),RBUYSPST+4                                               
         DROP  R5                                                               
         ZAP   WORK(5),=P'99999999'                                             
         PACK  WORK+10(1),SORTKCON+3(1)     INVERT THE SEQUENCE                 
         PACK  WORK+11(1),SORTKCON+2(1)     OF THE DIGITS                       
         PACK  WORK+12(1),SORTKCON+1(1)                                         
         PACK  WORK+13(1),SORTKCON+0(1)     MOVE IN SIGN AND                    
         MVI   WORK+14,X'0C'                SHIFT ONE DECIMAL PLACE             
         SRP   WORK+10(5),64-1,0            TO RIGHT                            
         SP    WORK(5),WORK+10(5)           BEFORE SUBTRACTING FROM             
         SRP   WORK(5),1,0                  NINES AND SHIFTING 1 TO             
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),WORK(4)                                                 
         EDIT  (P5,DUB+3),(8,PREPCON)                                           
         EDIT  (B1,RBUYKLIN),(3,PREPLIN)                                        
         EDIT  (B2,RBUYTSPT),(5,PREPSPT)                                        
         EDIT  (B4,RBUYTCOS),(12,PREPDOL),2,MINUS=YES                           
         L     RF,REPCONS                                                       
         ZICM  RE,RBUYTSPT,2                                                    
         SLL   RE,16               MOVE TO HIGH-ORDER TWO BYTES                 
         SRA   RE,16               RESET WITH SIGN SET IF NEGATIVE              
         AR    RF,RE                                                            
         ST    RF,REPCONS                                                       
         L     RF,REPCOND                                                       
         ZICM  RE,RBUYTCOS,4                                                    
         AR    RF,RE                                                            
         ST    RF,REPCOND                                                       
*                                                                               
         LA    R4,132(R4)                                                       
         ST    R4,APNTTABC                                                      
*                                                                               
*        BLDPRINT EXIT                                                          
*                                                                               
BPNTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
BPNTBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 2                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        DOSTACHG --- PROCESS AN AGENCY CLI/PRD/EST/STATION CHANGE              
*                                                                               
DOSTACHG NTR1                                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINESTA,R3                                                      
         MVC   PSTADASH(L'PSTADASH),DASHLIT                                     
         LA    R3,P2                                                            
         MVC   PSTALIT(L'STALIT),STALIT                                         
         EDIT  (B4,SPOTSTAS),(5,PSTASPT)                                        
         EDIT  (B4,SPOTSTAD),(12,PSTADOL),2,MINUS=YES                           
         DROP  R3                                                               
*                                                                               
         L     RF,SPOTESTS                                                      
         A     RF,SPOTSTAS                                                      
         ST    RF,SPOTESTS                                                      
         L     RF,SPOTESTD                                                      
         A     RF,SPOTSTAD                                                      
         ST    RF,SPOTESTD                                                      
         XC    SPOTSTAS,SPOTSTAS                                                
         XC    SPOTSTAD,SPOTSTAD                                                
*                                                                               
DSTAGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DOCONCHG --- PROCESS A REP ADV/PRD/CONTRACT CHANGE                     
*                                                                               
DOCONCHG NTR1                                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINECON,R3                                                      
         MVC   PCONDASH(L'PCONDASH),DASHLIT                                     
         LA    R3,P2                                                            
         MVC   PCONLIT(L'CONLIT),CONLIT                                         
         EDIT  (B4,REPCONS),(5,PCONSPT)                                         
         EDIT  (B4,REPCOND),(12,PCONDOL),2,MINUS=YES                            
         DROP  R3                                                               
*                                                                               
         L     RF,REPAPS                                                        
         A     RF,REPCONS                                                       
         ST    RF,REPAPS                                                        
         L     RF,REPAPD                                                        
         A     RF,REPCOND                                                       
         ST    RF,REPAPD                                                        
         XC    REPCONS,REPCONS                                                  
         XC    REPCOND,REPCOND                                                  
*                                                                               
DCONGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DOESTCHG --- PROCESS AN AGENCY CLI/PRD/ESTIMATE CHANGE                 
*                                                                               
DOESTCHG NTR1                                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINEEST,R3                                                      
         MVC   PESTLIT(L'ESTLIT),ESTLIT                                         
         EDIT  (B4,SPOTESTS),(5,PESTSPT)                                        
         EDIT  (B4,SPOTESTD),(12,PESTDOL),2,MINUS=YES                           
         DROP  R3                                                               
*                                                                               
         L     RF,SPOTAGYS                                                      
         A     RF,SPOTESTS                                                      
         ST    RF,SPOTAGYS                                                      
         L     RF,SPOTAGYD                                                      
         A     RF,SPOTESTD                                                      
         ST    RF,SPOTAGYD                                                      
         XC    SPOTESTS,SPOTESTS                                                
         XC    SPOTESTD,SPOTESTD                                                
*                                                                               
DESTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DOADVPRD --- PROCESS A REP ADV/PRD CHANGE                              
*                                                                               
DOADVPRD NTR1                                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINERAP,R3                                                      
         MVC   PRAPLIT(L'RAPLIT),RAPLIT                                         
         EDIT  (B4,REPAPS),(5,PRAPSPT)                                          
         EDIT  (B4,REPAPD),(12,PRAPDOL),2,MINUS=YES                             
         DROP  R3                                                               
*                                                                               
         L     RF,REPAGYS                                                       
         A     RF,REPAPS                                                        
         ST    RF,REPAGYS                                                       
         L     RF,REPAGYD                                                       
         A     RF,REPAPD                                                        
         ST    RF,REPAGYD                                                       
         XC    REPAPS,REPAPS                                                    
         XC    REPAPD,REPAPD                                                    
*                                                                               
DADVGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        DOAGYCHG --- PROCESS AN AGENCY CHANGE                                  
*                                                                               
DOAGYCHG NTR1                                                                   
*                                                                               
         LA    R3,P                           DO SPOT TOTALS                    
         USING PLINEEST,R3                                                      
         MVC   PESTLIT(L'SAGYLIT),SAGYLIT                                       
         EDIT  (B4,SPOTAGYS),(5,PESTSPT)                                        
         EDIT  (B4,SPOTAGYD),(12,PESTDOL),2,MINUS=YES                           
         DROP  R3                                                               
*                                                                               
         L     RF,SPOTRUNS                                                      
         A     RF,SPOTAGYS                                                      
         ST    RF,SPOTRUNS                                                      
         L     RF,SPOTRUND                                                      
         A     RF,SPOTAGYD                                                      
         ST    RF,SPOTRUND                                                      
         XC    SPOTAGYS,SPOTAGYS                                                
         XC    SPOTAGYD,SPOTAGYD                                                
*                                                                               
         LA    R3,P                           NOW DO REP TOTALS                 
         USING PLINERAP,R3                                                      
         MVC   PRAPLIT(L'RAGYLIT),RAGYLIT                                       
         EDIT  (B4,REPAGYS),(5,PRAPSPT)                                         
         EDIT  (B4,REPAGYD),(12,PRAPDOL),2,MINUS=YES                            
         DROP  R3                                                               
*                                                                               
         L     RF,REPRUNS                                                       
         A     RF,REPAGYS                                                       
         ST    RF,REPRUNS                                                       
         L     RF,REPRUND                                                       
         A     RF,REPAGYD                                                       
         ST    RF,REPRUND                                                       
         XC    REPAGYS,REPAGYS                                                  
         XC    REPAGYD,REPAGYD                                                  
*                                                                               
DAGYGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        ERRRECAP --- PRINT ALL THE ERRORS IN A RECAP                           
*                                                                               
ERRRECAP NTR1                                                                   
*                                                                               
***      CLI   QOPT3,C'L'          LOCAL REPORT?                                
***      BE    RCAP0040            YES - NO PRINT QUEUE                         
*                                                                               
*   SUMMARY REPORT BREAKOUT                                                     
*                                                                               
         CLI   SUMRYREP,C'Y'       SUMMARY REPORT BREAKOUT?                     
         BNE   RCAP0040            NO                                           
*                                                                               
         OC    SAVWORK6,SAVWORK6   ANY SYS ID IN SAVWORK6?                      
         BZ    RCAP0040            NO  - NO PRINT QUEUE                         
*                                                                               
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
*                                  CLOSE THE PRINT QUEUE                        
*                                  OPEN A REPORT FOR RECAP                      
         L     R3,VMASTC                                                        
         USING MASTD,R3                                                         
*                                                                               
         L     R4,MCVREMOT                                                      
         USING REMOTED,R4                                                       
         MVC   REMOTDST,SAVWORK6   INSERT SAVED WORK+6 VALUE                    
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'RSS'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
****     MVI   REMOTRET,C'R'      RETAIN 36 LIVE, 6 DEAD                        
         MVI   REMOTRET,C'I'      RETAIN 120 LIVE, 120 DEAD                     
         MVC   REMOTJID,=C'SRR'   SET REPORT CODE TO SR'RECAP'                  
         DROP  R3,R4                                                            
*                                  YES - CLOSE DETAIL REP OPEN RECAP            
RCAP0040 EQU   *                                                                
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   INRECAP,1                                                        
         CLOSE (RRECAP)                                                         
         OPEN  (RRECAP,(INPUT))                                                 
         LTR   RF,RF                                                            
         BNZ   RCAP0080                                                         
RCAP0060 EQU   *                                                                
         GET   RRECAP,P                                                         
         GOTO1 REPORT                                                           
         GET   RRECAP,P                                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     RCAP0060                                                         
*                                                                               
RCAP0080 EQU   *                                                                
         MVC   P(26),=C'NO RUN ERRORS ENCOUNTERED'                              
         GOTO1 REPORT                                                           
*                                                                               
RCAP0100 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(16),=C'* END OF RECAP *'                                       
         GOTO1 REPORT                                                           
         CLOSE (RRECAP)                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         MVI   INRECAP,0                                                        
         OPEN  (RRECAP,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    RCAP0120                                                         
         DC    H'0'                                                             
RCAP0120 EQU   *                                                                
*                                                                               
RCAP0200 EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        NEXTEST --- PROCESS AN ESTIMATE BREAK                                  
*                                                                               
NEXTEST  NTR1                                                                   
*                                                                               
         LA    R6,SPBLOCK                                                       
         USING SPBUYBLK,R6                                                      
*                                                                               
         MVC   REPADVC,SPACES          CLEAR ALL REP FIELDS IN HEAD             
         MVC   REPADVN,SPACES                                                   
         MVC   REPPRDC,SPACES                                                   
         MVC   REPPRDN,SPACES                                                   
         MVC   REPNC#,SPACES                                                    
         MVC   REPNCNM,SPACES                                                   
*                                                                               
         CLC   SORTREC(6),0(R2)        CHANGE IN CLIENT                         
         BE    NEST100                                                          
         MVC   CLT,SPACES              CLEAR HEADER FIELDS                      
         MVC   CLTNM,SPACES                                                     
         MVC   PRD,SPACES                                                       
         MVC   PRDNM,SPACES                                                     
         MVC   EST,SPACES                                                       
         MVC   ESTNM,SPACES                                                     
         XC    ESTST,ESTST                                                      
         XC    ESTND,ESTND                                                      
         GOTO1 CLPACK,DMCB,SORTKCLT,BCLT                                        
         CLI   DMCB,0                                                           
         BE    NEST10                                                           
         MVI   ESTERR,EREQBCLI                                                  
         B     NESTGOOD                                                         
NEST10   EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CKEY,R4                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NEST20                                                           
         MVI   ESTERR,EREQNCLI                                                  
         B     NESTGOOD                                                         
NEST20   EQU   *                                                                
         L     R5,AREC                                                          
         L     R4,ADCLT                                                         
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         MVC   CLT,SORTKCLT                                                     
         MVC   CLTNM,CNAME                                                      
         LA    RE,CLIST                                                         
         L     RF,ACPRDLST                                                      
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SAVCPROF,CPROF                                                   
         ST    R5,AREC                                                          
         DROP  R4                                                               
NEST100  EQU   *                                                                
         SPACE 2                                                                
         CLC   SORTREC(9),0(R2)        CHANGE IN PRODUCT                        
         BE    NEST200                                                          
         MVC   PRD,SPACES              CLEAR HEADER FIELDS                      
         MVC   PRDNM,SPACES                                                     
         MVC   EST,SPACES                                                       
         MVC   ESTNM,SPACES                                                     
         XC    ESTST,ESTST                                                      
         XC    ESTND,ESTND                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PKEY,R4                                                          
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,SORTKPRD                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NEST120                                                          
         MVI   ESTERR,EREQNPRD                                                  
         B     NESTGOOD                                                         
NEST120  EQU   *                                                                
         L     R5,AREC                                                          
         L     R4,ADPRD                                                         
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         MVC   PRD,SORTKPRD                                                     
         MVC   PRDNM,PNAME                                                      
         MVC   PRDCODE(2),PCODE                                                 
         ST    R5,AREC                                                          
         DROP  R4                                                               
*                                                                               
         GOTO1 GETBPRD,DMCB,PRD                                                 
         BNZ   NESTBAD                                                          
         MVC   BPRD(1),DM1                 SAVE PRODUCT NUMBER                  
         MVI   BPRDPOL,X'FF'               SET FOR BRAND POOL                   
*                                                                               
         XC    PRDP,PRDP                   CLEAR PIGGY PROD AREAS               
         XC    BPRDP,BPRDP                                                      
         OC    SORTKPRP,SORTKPRP           IS THERE A PIGGY PRODUCT?            
         BZ    NEST200                     ZERO MEANS NO                        
         MVC   PRDP(3),SORTKPRP                                                 
         GOTO1 GETBPRD,DMCB,PRDP                                                
         BNZ   NESTBAD                                                          
         MVC   BPRDP(1),DM1                SAVE PIGGY PRODUCT NUMBER            
         GOTO1 GETBPRD,DMCB,=C'POL'                                             
         BNZ   NESTBAD                                                          
         MVC   BPRDPOL(1),DM1              SAVE POOL PRODUCT NUMBER             
         SPACE 2                                                                
NEST200  EQU   *                           CHANGE IN ESTIMATE                   
         MVC   EST,SPACES                  CLEAR HEADER FIELDS                  
         MVC   ESTNM,SPACES                                                     
         XC    ESTST,ESTST                                                      
         XC    ESTND,ESTND                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,SORTKPRD                                                 
         MVC   EKEYEST,SORTKEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    NEST220                                                          
         MVI   ESTERR,EREQNEST                                                  
         B     NESTGOOD                                                         
NEST220  EQU   *                                                                
         L     R5,AREC                                                          
         L     R4,ADEST                                                         
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         MVC   BEST,SORTKEST                                                    
         EDIT  (B1,BEST),(3,EST)                                                
         MVC   ESTNM(20),EDESC                                                  
         MVC   ESTST,ESTART                                                     
         MVC   ESTND,EEND                                                       
         MVC   ESTDM,EDAYMENU                                                   
         MVC   ESTBOOK,EBOOK                                                    
         MVC   ESTOWK,EOWSDAY                                                   
         MVC   ESTREP,EREP                                                      
         MVC   ESTADJ,EHUTADJ                                                   
         MVC   ESTDEML,EDEMLST                                                  
         ST    R5,AREC                                                          
*                                                                               
*                                                                               
         CLI   MED,C'T'                                                         
         BNE   NEST230                  ONLY FOR TV                             
         XC    DMCB,DMCB                                                        
         MVC   DMCB+0(2),SPAGY                                                  
         MVC   DMCB+2(1),SPMED                                                  
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 DPTRD,DMCB,,(C'P',ADDPTTAB)                                      
*                                                                               
         L     RF,PRDBUFF          FIND PRODUCT BUFFER SLOT                     
         USING PTBUFFD,RF                                                       
         LA    RE,219              'POL' IS 220, THEN ZERO RELATIVE             
         MH    RE,PRDBUFLN                                                      
         LA    RF,0(RE,RF)                                                      
         MVC   PTDEMO,EDEMLST      SET DEMOS FOR THIS PRODUCT                   
         MVC   PTWGHT,EWGTLST                                                   
NEST230  EQU   *                                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   QSTART,ESTST                                                     
         MVC   QEND,ESTND                                                       
*                                                                               
         CLI   MED,C'T'                                                         
         BNE   NEST240                  ONLY FOR TV                             
         L     R4,MEDBUFF                                                       
         USING MEDBLOCK,R4                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'200'                                                 
         MVI   MEDBRAND,X'FF'                                                   
         MVI   MEDEXTDM,20                                                      
         MVC   WORK(2),ESTBOOK                                                  
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+4)                                  
         MVC   QBOOK1(4),WORK+4                                                 
         MVC   QHUT1(2),=C'  '                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 MEDDATE,DMCB,(RC)                                                
NEST240  EQU   *                                                                
         SPACE 2                                                                
*                                                                               
*                                           GET REP INFO FOR REPORT             
         MVC   REPCODE(2),SORTDATA+16       GET THE BUY'S REP CODE              
         LA    R4,SORTDATA+34               POINT TO THE FIRST ELEMENT          
NEST300  EQU   *                                                                
         CLI   0(R4),X'08'                  SPOTPAK INFO EL                     
         BE    NEST310                                                          
         ZICM  R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     NEST300                                                          
NEST310  EQU   *                                                                
         USING RBUYSPEL,R4                                                      
         MVC   REPADVC,RBUYSADV                                                 
         MVC   REPPRDC,RBUYSPRD                                                 
         DROP  R4                                                               
*                                                                               
         XC    REPKEY,REPKEY                                                    
         LA    R4,REPKEY                                                        
         USING RADVKEY,R4                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,REPADVC                                                 
         MVC   RADVKREP,REPCODE                                                 
         DROP  R4                                                               
         GOTO1 REPHIGH                                                          
         CLC   REPKEY(27),REPKSAVE                                              
         BE    NEST320                                                          
NEST315  EQU   *                                                                
         MVI   ESTERR,EREQRADV                                                  
         B     NESTGOOD                                                         
NEST320  EQU   *                                                                
         GOTO1 REPGET                                                           
         BNZ   NEST315             RECORD NOT FOUND: SKIP IT                    
         L     R4,AREC                                                          
         USING RADVREC,R4                                                       
         MVC   REPADVN,RADVNAME                                                 
         DROP  R4                                                               
*                                                                               
         XC    REPKEY,REPKEY                                                    
         LA    R4,REPKEY                                                        
         USING RPRDKEY,R4                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,REPADVC                                                 
         MVC   RPRDKPRD,REPPRDC                                                 
         MVC   RPRDKREP,REPCODE                                                 
         DROP  R4                                                               
         GOTO1 REPHIGH                                                          
         CLC   REPKEY(27),REPKSAVE                                              
         BE    NEST340                                                          
NEST335  EQU   *                                                                
         MVI   ESTERR,EREQRPRD                                                  
         B     NESTGOOD                                                         
NEST340  EQU   *                                                                
         GOTO1 REPGET                                                           
         BNZ   NEST335             RECORD NOT FOUND: SKIP IT                    
         L     R4,AREC                                                          
         USING RPRDREC,R4                                                       
         MVC   REPPRDN,RPRDNAME                                                 
         MVC   REPNC#,RPRDNET#                                                  
         MVC   REPNCNM,SPACES                                                   
         MVC   REPPPC,SPACES                                                    
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R4)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   NEST350                                                          
         L     R4,DM4                                                           
         USING RPRDNELM,R4                                                      
         MVC   REPNCNM,RPRDNDES                                                 
         MVC   REPPPC,RPRDNPNT                                                  
NEST350  EQU   *                                                                
         DROP  R4                                                               
*                                                                               
         OC    REPPPC,REPPPC                                                    
         BZ    NEST370                                                          
         CLC   REPPPC,SPACES                                                    
         BE    NEST370                                                          
         XC    REPKEY,REPKEY                                                    
         LA    R4,REPKEY                                                        
         USING RPTPKEY,R4                                                       
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPCODE                                                 
         MVC   RPTPKREC,REPPPC                                                  
         DROP  R4                                                               
         GOTO1 REPHIGH                                                          
         CLC   REPKEY(27),REPKSAVE                                              
         BE    NEST360                                                          
NEST355  EQU   *                                                                
         MVI   ESTERR,EREQRPTP                                                  
         B     NESTGOOD                                                         
NEST360  EQU   *                                                                
         GOTO1 REPGET                                                           
         BNZ   NEST355             RECORD NOT FOUND: SKIP IT                    
         L     R4,AREC                                                          
         USING RPTPREC,R4                                                       
         MVC   REPPPR,RPTPREP                                                   
         MVC   REPPPN,RPTPNAME                                                  
         MVC   REPPPO,RPTPOFF                                                   
         DROP  R4                                                               
         B     NEST380                                                          
NEST370  EQU   *                                                                
         MVC   REPPPC,SPACES                                                    
         MVC   REPPPR,SPACES                                                    
         MVC   REPPPN,SPACES                                                    
         MVC   REPPPO,SPACES                                                    
*                                                                               
NEST380  EQU   *                        GET AGENCY FROM A CONTRACT              
         XC    REPKEY,REPKEY                                                    
         LA    R4,REPKEY                                                        
         USING RCONKEY,R4                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,REPCODE                                                 
         PACK  WORK+0(1),SORTKCON+3(1)                                          
         PACK  WORK+1(1),SORTKCON+2(1)                                          
         PACK  WORK+2(1),SORTKCON+1(1)                                          
         PACK  WORK+3(1),SORTKCON+0(1)                                          
         MVC   RCONPCON,WORK                                                    
         DROP  R4                                                               
         GOTO1 REPHIGH                                                          
         CLC   REPKEY(27),REPKSAVE                                              
         BE    NEST390                                                          
NEST385  EQU   *                                                                
         MVI   ESTERR,EREQRCON                                                  
         B     NESTGOOD                                                         
NEST390  EQU   *                                                                
         GOTO1 REPGET                                                           
         BNZ   NEST385             RECORD NOT FOUND: SKIP IT                    
         L     R4,AREC                                                          
         USING RCONREC,R4                                                       
         MVC   REPAGYC+0,RCONKAGY                                               
         MVI   REPAGYC+4,C'-'                                                   
         MVC   REPAGYC+5,RCONKAOF                                               
         DROP  R4                                                               
*                                                                               
NEST400  EQU   *                        SET VALUES IN BUYGEN BLOCK              
         MVC   SPAGY,AGY                                                        
         MVC   SPMED,MED                                                        
         MVC   SPCLI,CLT                                                        
         MVC   SPPRD,PRD                                                        
         MVC   SPEST,BEST                                                       
         MVC   SPPRDP,PRDP                                                      
*                                                                               
         MVC   SPAM,BAGYMD                                                      
         MVC   SPCLT,BCLT                                                       
         MVC   SPPRD#,BPRD                                                      
         MVC   SPPRDP#,BPRDP                                                    
         MVC   SPPOLP#,BPRDPOL                                                  
         MVC   SPPRDCDS,PRDCODE                                                 
*                                                                               
         MVC   SPESTDMU,ESTDM                                                   
         MVC   SPESTSTR,ESTST                                                   
         MVC   SPESTEND,ESTND                                                   
         MVC   SPESTBK,ESTBOOK                                                  
         MVC   SPESTOWK,ESTOWK                                                  
         MVC   SPESTREP,ESTREP                                                  
         MVC   SPESTADJ,ESTADJ                                                  
         MVC   SPESTDEM,ESTDEML                                                 
*                                                                               
         MVC   SPCOMFAC,ACOMFACS                                                
         MVC   SPADCLT,ADCLT                                                    
         MVC   SPADPRD,ADPRD                                                    
         MVC   SPADEST,ADEST                                                    
         MVC   SPADBUY,ADBUY                                                    
         LA    RF,ADBUY                                                         
         ST    RF,SPAADBUY                                                      
         MVC   SPADSTAT,ADSTAT                                                  
         MVC   SPMARKET,ADMARKET                                                
         ST    RC,SPSPONCR                                                      
         MVC   SPMEDBUF,MEDBUFF                                                 
         MVC   SPMEDGB,MEDGETBY                                                 
         MVC   SPBUYDET,ABLDTAB                                                 
         LA    RF,SAVAPROF                                                      
         ST    RF,SPAAPROF                                                      
         LA    RF,SAVCPROF                                                      
         ST    RF,SPACPROF                                                      
*                                                                               
*        NEXTEST EXIT                                                           
*                                                                               
NESTGOOD EQU   *                                                                
         B     TESTGOOD                                                         
NESTBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        GETBPRD --- FIND THIS PRODUCT CODE IN THE CLIENT LIST                  
*                                                                               
*        P1     =   A(PRODUCT CODE)    *INPUT*                                  
*                                                                               
*        P1     =   CL1(PRODUCT CODE)  *OUTPUT*                                 
*                                                                               
GETBPRD  NTR1                                                                   
*                                                                               
         L     R3,0(R1)                                                         
         MVI   0(R1),X'00'                                                      
         L     R4,ACPRDLST                 FIND THE PRODUCT #                   
         LA    R5,220                      IN THE CLIENT LIST (MAX=220)         
GPRD10   EQU   *                                                                
         CLC   0(3,R4),0(R3)                                                    
         BE    GPRD20                                                           
         LA    R4,4(R4)                                                         
         BCT   R5,GPRD10                                                        
         MVC   RUNERROR(3),0(R3)                                                
         MVC   RUNERROR+5(L'NOPRDNUM),NOPRDNUM                                  
         B     GPRDBAD                                                          
GPRD20   EQU   *                                                                
         MVC   0(1,R1),3(R4)               SAVE PRODUCT NUMBER                  
*                                                                               
GPRDGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GPRDBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        NEXTSTA --- PROCESS STATION/MARKET BREAK                               
*                                                                               
NEXTSTA  NTR1                                                                   
*                                                                               
         LA    R6,SPBLOCK                                                       
         USING SPBUYBLK,R6                                                      
*                                                                               
         XC    STAERR,STAERR                                                    
*                                                                               
         XC    SPMKT(4),SPMKT                                                   
         XC    SPSTA(5),SPSTA                                                   
         XC    SPMSTA(5),SPMSTA                                                 
*                                                                               
         MVC   STA,SORTKSTA                                                     
         CLI   STA+4,C' '                                                       
         BNE   NSTA05                                                           
         MVI   STA+4,C'T'                                                       
NSTA05   EQU   *                                                                
         MVC   STAPRINT(7),SPACES                                               
         MVC   STAPRINT(4),SORTKSTA                                             
         LA    R2,STAPRINT+3                                                    
         CLI   0(R2),C' '                                                       
         BE    NSTA10                                                           
         LA    R2,1(R2)                                                         
NSTA10   EQU   *                                                                
         CLI   SORTKSTA+4,C' '                                                  
         BE    NSTA15                                                           
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(1,R2),SORTKSTA+4                                               
         LA    R2,1(R2)                                                         
         CLI   SORTKSTA+4,C'T'                                                  
         BNE   NSTA12                                                           
         MVI   0(R2),C'V'                                                       
         B     NSTA15                                                           
NSTA12   EQU   *                                                                
         MVI   0(R2),C'M'                                                       
NSTA15   EQU   *                                                                
*                                                                               
*        CLC   =C'WOTV',STA        **TEMPORARY**                                
*        BNE   NSTA17              **TEMPORARY**                                
*        MVI   STAERR,EREQSKIP     **TEMPORARY**                                
*        B     NSTAGOOD            **TEMPORARY**                                
NSTA17   EQU   *                   **TEMPORARY**                                
         CLI   STA+4,C'C'                COMBO STATION?                         
         BNE   NSTA20                    NO, KEEP GOING                         
         MVI   STAERR,EREQCOMB           SET DOWNSTREAM ERROR CODE              
         B     NSTAGOOD                  YES, DON'T PROCESS AND                 
*                                         FLAG ERROR LATER                      
*                                                                               
NSTA20   EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED(1),MED                                                   
         MVC   STAKCALL(5),STA                                                  
         MVC   STAKAGY(2),SORTKAGY                                              
         MVC   STAKCLT(3),CLT                                                   
         L     R5,AREC                                                          
         L     R4,ADSTAT                                                        
         ST    R4,AREC                                                          
         GOTO1 STAGET                                                           
         CLC   STAKEY(12),KEYSAVE                                               
         BE    NSTA100                                                          
         CLC   STAKEY(9),KEYSAVE                                                
         BNE   NSTA50                                                           
         CLC   STAKEY+9(3),=C'000'                                              
         BE    NSTA100                                                          
NSTA50   EQU   *                                                                
         MVI   STAERR,EREQSTAM           SET DOWNSTREAM ERROR CODE              
         ST    R5,AREC                                                          
         MVC   SORTKSTA,SPACES     CLEAR STATION FOR NO-FIND                    
         B     NSTAGOOD                                                         
NSTA100  EQU   *                                                                
         MVC   MKT(4),SMKT                                                      
         ST    R5,AREC                                                          
         DROP  R4                                                               
         SPACE 2                                                                
         GOTO1 MSPACK,DMCB,MKT,STA,WORK                                         
         MVC   BMKTSTA(5),WORK                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTKEY,R4                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED(1),MED                                                   
         MVC   MKTKMKT(4),MKT                                                   
         MVC   MKTKAGY(2),SORTKAGY                                              
         L     R5,AREC                                                          
         L     R4,ADMARKET                                                      
         ST    R4,AREC                                                          
         GOTO1 STAGET                                                           
         CLC   MKTKEY(8),KEYSAVE                                                
         BE    NSTA200                                                          
         MVI   STAERR,EREQMKTM           SET DOWNSTREAM ERROR CODE              
         ST    R5,AREC                                                          
         B     NSTAGOOD                                                         
NSTA200  EQU   *                                                                
         MVC   MKTNM(24),MKTNAME                                                
         ST    R5,AREC                                                          
         DROP  R4                                                               
*                                                                               
         MVC   SPMKT(4),MKT                                                     
         MVC   SPSTA(5),STA                                                     
         MVC   SPMSTA(5),BMKTSTA                                                
*                                                                               
         MVC   CARDREC,SPACES                                                   
         MVC   CARDRU3,=C'U3'                                                   
         MVC   CARDRAGY,SORTKAGY                                                
         MVC   CARDRMED,MED                                                     
         MVC   CARDRCLT,CLT                                                     
         MVI   CARDRPGR,C'N'                                                    
         MVI   CARDRMGR,C'N'                                                    
         MVC   CARDRPRD,PRD                                                     
         MVC   CARDRMKT,MKT                                                     
         EDIT  (B1,BEST),(3,CARDREST),FILL=0                                    
         MVC   CARDRED1,ESTST                                                   
         MVC   CARDRED2,ESTND                                                   
         MVC   CARDRREQ,=C'SR2 TRANSFER'                                        
         CLC   CARDHOLD,CARDREC                                                 
         BE    NSTA300                                                          
         PUT   CARDOUT,CARDREC                                                  
         MVC   CARDHOLD,CARDREC                                                 
NSTA300  EQU   *                                                                
*                                                                               
*        NEXTSTA EXIT                                                           
*                                                                               
NSTAGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        ENDRUN --- FINISH THE RUN                                              
*                                                                               
ENDRUN   NTR1                                                                   
*                                                                               
         BAS   RE,BREAK1                                                        
         BNZ   ENDRBAD                                                          
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     RE,MCVREMOT                                                      
         USING REMOTED,RE                                                       
         OC    REMOTKEY,REMOTKEY                                                
         BZ    ENDR50                                                           
******   GOTO1 PRINT,DMCB,=C'CLOSE'                                             
******   XC    REMOTKEY,REMOTKEY                                                
******   GOTO1 PRINT,DMCB,=C'OPEN'                                              
ENDR50   EQU   *                                                                
         DROP  RE,RF                                                            
*                                                                               
ENDRGOOD EQU   *                                                                
         B     TESTGOOD                                                         
ENDRBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        CLOSEUP --- CLOSE REPOUT AND CARDOUT                                   
*                                                                               
CLOSEUP  NTR1                                                                   
*                                                                               
         L     R6,AOUTREC                                                       
         MVI   0(R6),X'FF'                                                      
         MVC   1(20,R6),0(R6)                                                   
         PUT   REPOUT,(6)                                                       
         CLOSE (REPOUT)                                                         
*                                                                               
         CLOSE (CARDOUT)                                                        
*                                                                               
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         MVC   H1+000(10),MEDNM                                                 
         MVC   H1+094(33),AGYNM                                                 
         MVC   H2+094(33),AGYADR                                                
*                                                                               
         CLI   INRECAP,0                                                        
         BNE   HHOOK100                                                         
*                                                                               
         MVC   H4+010(03),CLT                                                   
         MVC   H4+014(20),CLTNM                                                 
         MVC   H5+010(03),PRD                                                   
         MVC   H5+014(20),PRDNM                                                 
         MVC   H6+010(03),EST                                                   
         MVC   H6+014(20),ESTNM                                                 
         MVC   H7+015(18),SPACES                                                
*                                                                               
*                                  SPECIAL TEST FOR BAD EST #                   
         CLC   =C'0000ANTI-SMOKING',ESTNM                                       
         BE    HHOOK50                                                          
*                                                                               
         OC    ESTST,ESTST                                                      
         BZ    HHOOK50                                                          
         GOTO1 =V(DATVAL),DMCB,(0,ESTST),WORK+0,RR=RELO                         
         OC    DM1,DM1                                                          
         BZ    HHOOK50                                                          
         MVI   H7+015,C'('                                                      
         GOTO1 DATCON,DMCB,(0,ESTST),(5,H7+016)                                 
         MVI   H7+024,C'-'                                                      
         GOTO1 DATCON,DMCB,(0,ESTND),(5,H7+025)                                 
         MVI   H7+033,C')'                                                      
HHOOK50  EQU   *                                                                
*                                                                               
         MVC   H4+063(04),REPADVC                                               
         MVC   H4+072(20),REPADVN                                               
         MVC   H5+063(03),REPPRDC                                               
         MVC   H5+072(20),REPPRDN                                               
         MVC   H6+063(08),REPNC#                                                
         MVC   H6+072(20),REPNCNM                                               
*                                                                               
         CLI   USEDFLAG,00                                                      
         BE    HHOOK100                                                         
         MVC   H2+000(21),=C'>>> RUN RESTARTED <<<'                             
*                                                                               
HHOOK100 EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 2                                                                
SAVEREGS DS    11A                                                              
         EJECT                                                                  
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (DIRECTORY)                     
*                                                                               
         SPACE 3                                                                
REPREAD  MVC   COMMAND,DMREAD                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPSEQ   MVC   COMMAND,DMRSEQ                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPHIGH  MVC   COMMAND,DMRDHI                                                   
         MVC   FILE,REPDIR                                                      
         MVC   REPKSAVE,REPKEY                                                  
         B     REPDIRL                                                          
         SPACE 2                                                                
REPDIRL  NTR1                                                                   
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         IC    R4,DMINBTS                                                       
         MVC   REPKSAVE,REPKEY                                                  
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,REPKSAVE,REPKEY                 
         MVC   4(1,R3),SPOTSYS                                                  
         B     DMCHECK                                                          
         SPACE 3                                                                
*                                                                               
*        REPPAK AND STATION COMMUNICATION WITH DATA MANAGER (FILE)              
*                                                                               
         SPACE 3                                                                
STAGET   MVC   COMMAND,DMREAD                                                   
         MVC   FILE,STATION                                                     
         MVC   KEYSAVE,KEY                                                      
         MVI   DMOUTBTS,X'EF'                                                   
         LA    R2,KEY                                                           
         L     R3,UTL                                                           
         B     REPFILL                                                          
REPGET   MVC   COMMAND,GETREC                                                   
         MVC   FILE,REPFIL                                                      
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         LA    R2,REPKEY+28                                                     
REPFILL  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,(R2),AREC,DMWORK,0              
         MVC   4(1,R3),SPOTSYS                                                  
         SPACE 3                                                                
DMCHECK  EQU   *                                                                
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         MVI   DMOUTBTS,X'FF'                                                   
         BNZ   DMERRS                                                           
         B     TESTGOOD                                                         
DMERRS   EQU   *                                                                
         CLC   =C'GETREC',COMMAND  TRYING TO READ RECORD?                       
         BE    TESTBAD             YES - WILL BE SKIPPED                        
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        THE TURNAROUND REPORT REQUEST CARD                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*CARDREC'                                                    
CARDREC  DS    0CL80                                                            
CARDRU3  DS    CL2   +00                                                        
CARDRAGY DS    CL2   +02                                                        
CARDRMED DS    CL1   +04                                                        
CARDRCLT DS    CL3   +05                                                        
CARDRPGR DS    CL1   +08                                                        
CARDRMGR DS    CL1   +09                                                        
         DS    CL1   +10                                                        
CARDRPRD DS    CL3   +11                                                        
CARDRMKT DS    CL4   +14                                                        
CARDRSTA DS    CL5   +18                                                        
CARDREST DS    CL3   +23                                                        
         DS    CL11  +26                                                        
CARDRED1 DS    CL6   +37             ESTIMATE START DATE                        
CARDRED2 DS    CL6   +43             ESTIMATE END DATE                          
         DS    CL19  +49                                                        
CARDRREQ DS    CL12  +68             REQUESTOR NAME                             
*                                                                               
         DS    0D                                                               
         DC    CL8'CARDHOLD'                                                    
CARDHOLD DS    CL80                                                             
*                                                                               
*        THE SORT RECORD                                                        
*                                                                               
         DS    0D                                                               
ASORTREC DS    A                                                                
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
         DS    CL4                                                              
SORTREC  DS    0CL1050                                                          
SORTKEY  DS    0CL40                                                            
SORTKAGY DS    CL2   +00                                                        
SORTKMED DS    CL1   +02                                                        
SORTKCLT DS    CL3   +03                                                        
SORTKPRD DS    CL3   +06                                                        
SORTKPRP DS    CL3   +09                                                        
SORTKEST DS    CL1   +12                                                        
SORTKSTA DS    CL5   +13                                                        
SORTKLN  DS    CL1   +18                                                        
SORTKCON DS    CL4   +19                                                        
SORTKBUY DS    CL5   +23                                                        
SORTKDAT DS    CL3   +28                                                        
SORTKTIM DS    CL4   +31                                                        
         DS    CL5   +35                                                        
         DS    0CL1  +40  * END OF SORT KEY *                                   
SORTSTAT DS    CL10                HOLDS THE STATUS OF THIS RECORD              
SORTDATA DS    CL1000                                                           
SORTRECX EQU   *                                                                
         EJECT                                                                  
*                                                                               
*        TEXTS                                                                  
*                                                                               
STALIT   DC    C'STATION CHANGE'                                                
CONLIT   DC    C'CONTRACT CHANGE'                                               
ESTLIT   DC    C'* ESTIMATE TOTAL *'                                            
RAPLIT   DC    C'* REPPAK ADV/PROD TOTAL *'                                     
RAGYLIT  DC    C'** REPPAK AGENCY TOTAL **'                                     
SAGYLIT  DC    C'** SPOTPAK AGENCY TOTAL **'                                    
RRUNLIT  DC    C'*** REPPAK RUN TOTAL ***'                                      
SRUNLIT  DC    C'*** SPOTPAK RUN TOTAL ***'                                     
DASHLIT  DC    40C'-'                                                           
REPDIR   DC    C'REPDIR  '                                                      
REPFIL   DC    C'REPFIL  '                                                      
REPFILE  DC    C'REPFILE '                                                      
*                                                                               
*        RUN ERROR TEXTS                                                        
*                                                                               
EMPTYF   DC    C'THE EXTRACT FILE IS EMPTY'                                     
NODATA   DC    C'THE EXTRACT HAS NO DATA'                                       
NOHEADER DC    C'THE EXTRACT FILE IS MISSING THE HEADER RECORD'                 
BADDATE  DC    C'THE EXTRACT FILE HAS THE WRONG DATE'                           
BADAGY   DC    C'THE SPOTPAK AGENCY HEADER RECORD IS MISSING'                   
BADMED   DC    C'THIS MEDIA CODE CANNOT BE FOUND IN SPOTPAK'                    
SYSID    DC    C'THIS AGENCY CANNOT BE FOUND IN THE CONTROL SYSTEM'             
NOSPOT   DC    C'THIS AGENCY HAD NOT BE CLEARED FOR SPOTPAK'                    
NOQUEUE  DC    C'A PRINT QUEUE ID CANNOT BE FOUND FOR THIS AGENCY'              
NOPRDNUM DC    C'THIS PRODUCT CANNOT BE FOUND IN THE CLIENT HEADER'             
SPOTMISS DC    C'SPOT DATA MISSING FROM REP BUY RECORD'                         
         EJECT                                                                  
*                                                                               
*        DCB'S                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***DCB**'                                                    
*                                                                               
DATAIN   DCB   DDNAME=DATAIN,DSORG=PS,RECFM=FB,LRECL=1050,             X        
               BLKSIZE=1050,MACRF=(GL,PM),EODAD=MAIN0240                        
*                                                                               
RRECAP   DCB   DDNAME=RRECAP,DSORG=PS,RECFM=FB,LRECL=132,              X        
               BLKSIZE=132,MACRF=(GM,PM),EODAD=RCAP0100                         
*                                                                               
REPOUT   DCB   DDNAME=REPOUT,DSORG=PS,RECFM=FB,LRECL=1000,             X        
               BLKSIZE=1000,MACRF=PM                                            
*                                                                               
CARDOUT  DCB   DDNAME=CARDOUT,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=80,MACRF=PM                                              
         EJECT                                                                  
*                                                                               
* LOCAL EQUATES                                                                 
*                                                                               
MAXDTYPS EQU   20                         MAX NUMBER OF DEMO TYPES              
*                                                                               
*        REP FILE LIST                                                          
*                                                                               
REPFLIST DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
*        LOCAL CONSTANTS                                                        
*                                                                               
USED     DC    CL4'USED'    STATUS TEXT                                         
EOFKEY   DC    40X'FF'                                                          
USEDFLAG DC    X'00'                                                            
HEADIN   DC    X'00'                                                            
GOTDATA  DC    X'00'                                                            
*                                                                               
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
RUNERROR DS    CL60                                                             
REPKEY   DS    CL34                                                             
REPKSAVE DS    CL34                                                             
PRDCODE  DS    CL2                                                              
PRDP     DS    CL3          PIGGY PRODUCT CODE                                  
BPRDP    DS    CL1          PIGGY PRODUCT CODE FROM CLIENT REC                  
BPRDPOL  DS    CL1          POOL PRODUCT CODE FROM CLIENT REC                   
ESTDM    DS    CL1          ESTIMATE HDR DAYPART MENU NUM                       
ESTBOOK  DS    CL2          ESTIMATE HDR BOOK                                   
ESTOWK   DS    CL1          ESTIMATE HDR OUT-OF-WEEK DAY                        
ESTREP   DS    CL2          ESTIMATE HDR REP                                    
ESTADJ   DS    CL1          ESTIMATE HUT ADJUSTMENT                             
ESTDEML  DS    (MAXDTYPS)XL3    EST HDR DEMO LIST                               
SAVAPROF DS    CL20                                                             
SAVCPROF DS    CL15                                                             
ELCODE   DS    X                                                                
SPOTSYS  DS    X            SPOT SYS NUMBER (TO AND FROM UTL)                   
REPSYS   DS    X            REP SYS NUMBER                                      
INRECAP  DS    X            TELL HEADHOOK WE ARE PROCESSING RECAP               
ALTFLAG  DS    X            ALTERNATE WEEK FLAG                                 
STAERR   DS    X            STATION/MARKET ERROR FLAG-CODE                      
ESTERR   DS    X            SPOTPAK/REPPAK HEADER ERROR FLAG-CODE               
SUMRYREP DS    CL1          Y = SUMMARY REPORT ONLY                             
SAVWORK6 DS    XL2                                                              
         DS    0D                                                               
SPBLOCKL DS    CL8                                                              
SPBLOCK  DS    CL132        SPOT BUY GENERATOR CONTROL BLOCK                    
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         EJECT                                                                  
*                                                                               
*        NEXTAGY --- PROCESS AN AGENCY BREAK                                    
*                                                                               
NEXTAGY  NMOD1 0,*NAGY*                                                         
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 SPOTCT,DMCB,SORTKAGY                                             
         BNZ   NAGYBAD                                                          
*                                                                               
         LA    R6,SPBLOCK                                                       
         USING SPBUYBLK,R6                                                      
*                                                                               
         XC    SPSPECRP,SPSPECRP        CLEAR SPECIAL TRADE REP                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'sDAR'                                                 
         MVC   WORK+4(2),SORTKAGY                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+32,DATAMGR                                
         CLC   WORK+32+6(3),=C'   '     ANY TRADE SPECIAL REP DIGITS?           
         BNH   NAGY30                   NO                                      
         PACK  DUB,WORK+32+6(3)         YES - CONVERT TO PACKED                 
         CVB   R1,DUB                   CONVERT TO BINARY                       
         STCM  R1,3,SPSPECRP            SAVE CONVERTED VALUE                    
NAGY30   EQU   *                                                                
*                                                                               
*   TEST PRINT                                                                  
**       MVC   P+1(09),=C'sDARDATA='                                            
**       MVC   P+10(2),SPSPECRP                                                 
**       MVC   P+15(64),WORK                                                    
**       GOTO1 REPORT                                                           
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
         GOTO1 GETMYAGY,DMCB,SORTKAGY                                           
         BNZ   NAGYBAD                                                          
*                                                                               
         GOTO1 SETQUEUE,DMCB,SORTKAGY                                           
         BNZ   NAGYBAD                                                          
*                                                                               
         GOTO1 MEDGET,DMCB,(SORTKMED,SORTKAGY),DATAMGR,WORK                     
         CLI   DM3,X'FF'                                                        
         BNE   NAGY50                                                           
         MVC   RUNERROR(L'BADMED),BADMED                                        
         B     NAGYBAD                                                          
NAGY50   EQU   *                                                                
         MVC   BAGYMD(1),WORK                                                   
         MVC   MED(1),SORTKMED                                                  
         MVC   MEDNM(10),WORK+1                                                 
*                                                                               
NAGYGOOD EQU   *                                                                
         B     TMD1GOOD                                                         
NAGYBAD  EQU   *                                                                
         B     TMD1BAD                                                          
         EJECT                                                                  
*                                                                               
*        SETQUEUE --- OPEN THE PRINT QUEUE FOR THIS AGENCY                      
*                                                                               
SETQUEUE NTR1                                                                   
*                                                                               
         XC    SAVWORK6,SAVWORK6   INITIALIZE SAVE AREA FOR SYS ID              
         CLI   QOPT3,C'L'          LOCAL/NO PRINT QUEUES?                       
         BNE   SQUE0020            NO  - SET REMOTE VALUES                      
                                                                                
*                                                                               
         L     R3,VMASTC           YES - SAVE ID FOR DIRECT, IF ANY             
         USING MASTD,R3                                                         
         L     R4,MCVREMOT                                                      
         USING REMOTED,R4                                                       
         MVC   SAVWORK6,REMOTDST   SAVE FOR DISPLAY                             
         B     SQUE0200                                                         
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
SQUE0020 EQU   *                                                                
         CLC   =C'SELNY',QUESTOR        FORCE SELTEL                            
         BNE   SQUE0040                                                         
         MVC   WORK(6),=C'SELNY '                                               
         B     SQUE0100                                                         
SQUE0040 EQU   *                                                                
         L     R2,0(R1)         GET PASSED AGY CODE                             
         LA    R3,SQUETAB                                                       
SQUE0060 EQU   *                                                                
         CLI   0(R3),X'00'                                                      
         BE    SQUE0220                                                         
         CLC   0(2,R2),0(R3)                                                    
         BE    SQUE0080                                                         
         LA    R3,LSQUETAB(R3)                                                  
         B     SQUE0060                                                         
SQUE0080 EQU   *                                                                
         MVC   WORK(6),2(R3)                                                    
*                                                                               
SQUE0100 EQU   *                                                                
         L     R4,AREC                                                          
         XC    0(25,R4),0(R4)                                                   
         MVI   0(R4),C'I'                                                       
         MVC   15(10,R4),SPACES                                                 
         MVC   15(6,R4),WORK                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,=C'CTFILE',(R4),(R4),0                       
         CLI   DMCB+8,0                                                         
         BNE   SQUE0160            NO ID RECORD FOUND                           
         LA    R4,28(R4)                                                        
*                                                                               
SQUE0120 CLI   0(R4),0                                                          
         BE    SQUE0160                                                         
         CLI   0(R4),X'02'                                                      
         BNE   SQUE0140                                                         
         MVC   WORK+6(2),2(R4)                                                  
         B     SQUE0180                                                         
*                                                                               
SQUE0140 SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SQUE0120                                                         
*                                                                               
SQUE0160 MVC   WORK+6(2),=H'17'   DEFAULT TO SJR                                
*                                                                               
SQUE0180 EQU   *                                                                
         L     R3,VMASTC                                                        
         USING MASTD,R3                                                         
         L     R4,MCVREMOT                                                      
         USING REMOTED,R4                                                       
         MVC   SAVWORK6,WORK+6     SAVE FOR POSSIBLE RECAP SPLIT                
         MVC   REMOTDST,WORK+6                                                  
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'RSS'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
****     MVI   REMOTRET,C'R'      RETAIN 36 LIVE, 6 DEAD                        
         MVI   REMOTRET,C'I'      RETAIN 120 LIVE, 120 DEAD                     
         MVC   REMOTJID,=C'SR2'                                                 
         DROP  R3,R4                                                            
*                                                                               
SQUE0200 EQU   *                                                                
         B     TMD1GOOD                                                         
SQUE0220 EQU   *                                                                
         MVC   RUNERROR(L'NOQUEUE),NOQUEUE                                      
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P(14),=C'BAD PRINTQUEUE'                                         
         GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         B     TMD1BAD                                                          
         SPACE 2                                                                
SQUETAB  EQU   *                                                                
         DC    CL2'IR',CL6'IRFL'   ROUTE PER P. GEORGE 7/25/97                  
LSQUETAB EQU   *-SQUETAB                                                        
***      DC    CL2'IR',CL6'IRNY'                                                
         DC    CL2'BL',CL6'BLRNY'                                               
         DC    CL2'PV',CL6'PETNY'                                               
         DC    CL2'K3',CL6'KRGDN ' ROUTE PER D. VALERIOTI 9/25/97               
***      DC    CL2'K3',CL6'KRGNY '                                              
         DC    CL2'K9',CL6'RADNY'                                               
         DC    CL2'RT',CL6'TRANY'                                               
         DC    CL2'KH',CL6'REPDEM'                                              
         DC    CL2'JS',CL6'JSNY'                                                
         DC    CL2'SJ',CL6'SJR'                                                 
         DC    CL2'V8',CL6'PMCNY'                                               
         DC    CL2'NP',CL6'NPTVNY'                                              
         DC    CL2'F8',CL6'FSNLAU'                                              
         DC    CL2'MR',CL6'KTVNY'    MAR7/02                                    
         DC    CL2'RA',CL6'RMRNY '   FEB/02 - REGIONAL MARKET RADIO             
         DC    CL2'2S',CL6'LERLA '   SEP/04 - LOTUS ENTRAVISION                 
         DC    X'00'                                                            
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*        SPOTCT --- LOOK AT THE CONTROL FILE FOR SPOTPAK INFO                   
*                                                                               
*        P1     =   A(SPOTPAK POWER CODE)                                       
*                                                                               
*                                                                               
SPOTCT   NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         L     R5,AREC                                                          
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'5'                    SYSTEM ACCESS RECORD               
         MVC   23(2,R5),0(R2)                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,=C'CTFILE',(R5),(R5),0                       
         CLI   DMCB+8,0                                                         
         BE    SPCT20                                                           
         MVC   RUNERROR(L'SYSID),SYSID       NO ID RECORD ERROR                 
         B     SPCTBAD                                                          
SPCT20   EQU   *                                                                
         LA    R5,28(R5)                     POINT TO THE START OF REC          
SPCT50   EQU   *                                                                
         CLI   0(R5),0                                                          
         BNE   SPCT60                                                           
         MVC   RUNERROR(L'NOSPOT),NOSPOT       NOT CLEARED FOR SPOT             
         B     SPCTBAD                                                          
SPCT60   EQU   *                                                                
         CLI   0(R5),X'21'                    SYSTEM ELEMENT?                   
         BNE   SPCT70                                                           
         CLI   2(R5),X'02'                    SPOT?                             
         BE    SPCT100                                                          
SPCT70   EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     SPCT50                                                           
SPCT100  EQU   *                                                                
         MVC   BAGY(1),4(R5)                  SPOTPAK AGY NUMBER                
*                                                                               
SPCTGOOD EQU   *                                                                
         B     TMD1GOOD                                                         
SPCTBAD  EQU   *                                                                
         B     TMD1BAD                                                          
         EJECT                                                                  
*                                                                               
*        GETMYAGY --- GET THE AGENCY NAME                                       
*                                                                               
*        P1     =   A(AGENCY CODE)                                              
*                                                                               
GETMYAGY NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYHDR,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GAGYBAD                                                          
         GOTO1 GET                                                              
         L     R4,AREC                                                          
         MVC   AGY(2),0(R2)                                                     
         MVC   AGYNM(33),AGYNAME                                                
         MVC   AGYADR(33),AGYADDR                                               
         MVC   SAVAPROF,AGYPROF                                                 
         DROP  R4                                                               
*                                                                               
GAGYGOOD EQU   *                                                                
         B     TMD1GOOD                                                         
GAGYBAD  EQU   *                                                                
         MVC   RUNERROR(L'BADAGY),BADAGY                                        
         B     TMD1BAD                                                          
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TMD1GOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TMD1EXIT                                                         
TMD1BAD  EQU   *                                                                
         LA    R0,1                                                             
TMD1EXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        BYGENERR --- PROCESS AN ERROR RETURN FROM SPBYGEN                      
*                                                                               
BYGENERR NMOD1 0,*BYER*                                                         
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         L     R2,APNTTAB                                                       
         USING PLINED,R2                                                        
         L     R3,APNTTABC                                                      
         L     R4,ABLDTAB                                                       
         USING SPBUYDTD,R4                                                      
*                                                                               
BERR10   EQU   *                                                                
         CR    R2,R3                                                            
         BE    BERR100                                                          
*                                                                               
         ZICM  R5,SPERROUT,1                                                    
         BZ    BERR20                                                           
         MVC   PSPOTSPT,DASHLIT                                                 
         MVC   PSPOTDOL,DASHLIT                                                 
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     RF,SPSPECL                                                       
         LTR   RF,RF                                                            
         BZ    BERR12B                                                          
BERR12A  EQU   *                                                                
         OC    0(3,RF),0(RF)                                                    
         BZ    BERR12B                                                          
         ZIC   RE,2(RF)                                                         
         AR    R7,RE                                                            
         LA    RF,3(RF)                                                         
         B     BERR12A                                                          
BERR12B  EQU   *                                                                
         LR    RF,R7                                                            
         LCR   RF,RF                                                            
         A     RF,SPOTSTAS                                                      
         ST    RF,SPOTSTAS                                                      
         ZICM  RF,SPCOST,3                                                      
         SLL   R5,8                SHIFT VALUE TO HIGH-ORDER BYTE               
         SRA   R5,8                RESET VALUE WITH SIGN IF NEGATIVE            
         MR    R6,RF                                                            
         LCR   R7,R7                                                            
         A     R7,SPOTSTAD                                                      
         ST    R7,SPOTSTAD                                                      
         B     BERR30                                                           
BERR20   EQU   *                                                                
         ZICM  R5,SPMEMO,1                                                      
         BZ    BERR90                                                           
BERR30   EQU   *                                                                
         CH    R5,NBERRHAF                                                      
         BH    BERRBAD                                                          
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R6,BERRTAB                                                       
         AR    R5,R6                                                            
         ZICM  R6,0(R5),3                      MESSAGE ADDR                     
         ZIC   R7,3(R5)                        MESSAGE LEN                      
         BCTR  R7,0                                                             
         EX    R7,BERREX                                                        
         CLI   SPERROUT,7                      TRANSFER ERROR?                  
         BNE   BERR90                          NO                               
         GOTO1 =A(INVXFER),DMCB,(RC),(R2),(R4),RR=RELO                          
BERR90   EQU   *                                                                
         LA    R2,132(R2)                                                       
         LA    R4,SPDETBKL(R4)                                                  
         B     BERR10                                                           
*                                                                               
BERR100  EQU   *                                                                
*                                                                               
BERRGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     BERREXIT                                                         
BERRBAD  EQU   *                                                                
         LA    R0,1                                                             
BERREXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 2                                                                
BERREX   MVC   PERROR(0),0(R6)                                                  
         SPACE 2                                                                
         DROP  R2,R4                                                            
         SPACE 2                                                                
BERRTAB  EQU   *                                                                
         DC    AL3(TRANWKS),AL1(L'TRANWKS)         * ERROR  1 *                 
         DC    AL3(TRANSCH),AL1(L'TRANSCH)         * ERROR  2 *                 
         DC    AL3(TRANDATE),AL1(L'TRANDATE)       * ERROR  3 *                 
         DC    AL3(TRANNSCH),AL1(L'TRANNSCH)       * ERROR  4 *                 
         DC    AL3(TRANDTYP),AL1(L'TRANDTYP)       * ERROR  5 *                 
         DC    AL3(TRANFULL),AL1(L'TRANFULL)       * ERROR  6 *                 
         DC    AL3(TRANRTRN),AL1(L'TRANRTRN)       * ERROR  7 *                 
         DC    AL3(TRANSPOT),AL1(L'TRANSPOT)       * ERROR  8 *                 
         DC    AL3(TRANCOMB),AL1(L'TRANCOMB)       * ERROR  9 *                 
         DC    AL3(TRANDTS),AL1(L'TRANDTS)       * WARNING 10 *                 
         DC    AL3(TRANLONG),AL1(L'TRANLONG)     * WARNING 11 *                 
         DC    AL3(TRANWEEK),AL1(L'TRANWEEK)     * WARNING 12 *                 
         DC    AL3(TRANBCAN),AL1(L'TRANBCAN)     * WARNING 13 *                 
         DC    AL3(TRANCANE),AL1(L'TRANCANE)       * ERROR 14 *                 
         DC    AL3(TRANSTAM),AL1(L'TRANSTAM)       * ERROR 15 *                 
         DC    AL3(TRANMKTM),AL1(L'TRANMKTM)       * ERROR 16 *                 
         DC    AL3(TRANBCLI),AL1(L'TRANBCLI)       * ERROR 17 *                 
         DC    AL3(TRANNCLI),AL1(L'TRANNCLI)       * ERROR 18 *                 
         DC    AL3(TRANNPRD),AL1(L'TRANNPRD)       * ERROR 19 *                 
         DC    AL3(TRANNEST),AL1(L'TRANNEST)       * ERROR 20 *                 
         DC    AL3(TRANRADV),AL1(L'TRANRADV)       * ERROR 21 *                 
         DC    AL3(TRANRPRD),AL1(L'TRANRPRD)       * ERROR 22 *                 
         DC    AL3(TRANRPTP),AL1(L'TRANRPTP)       * ERROR 23 *                 
         DC    AL3(TRANRCON),AL1(L'TRANRCON)       * ERROR 24 *                 
         DC    AL3(TRANCANP),AL1(L'TRANCANP)     * WARNING 25 *                 
         DC    AL3(TRANSKIP),AL1(L'TRANSKIP)     * WARNING 26 *                 
         DC    AL3(TRANCRED),AL1(L'TRANCRED)     * WARNING 27 *                 
BERRTABX EQU   *                                                                
BERRTABL EQU   BERRTABX-BERRTAB                                                 
NBERRTAB EQU   BERRTABL/4                                                       
NBERRHAF DC    AL2(NBERRTAB)                                                    
         EJECT                                                                  
*                                                                               
*        TRANSFER ERROR TEXTS                                                   
*                                                                               
TRANWKS  DC    C'<INVALID # OF WEEKS       >'             *ERROR=01*            
EREQWKS  EQU   1                                                                
TRANSCH  DC    C'<INVALID SCHEDULE #       >'             *ERROR=02*            
EREQSCH  EQU   2                                                                
*                                                                               
*RANDATE DC    C'<<< DATES ERROR <<<<<<<<<<<'             *ERROR=03*            
*                                                                               
TRANDATE DC    C'<DATES ERROR-CHECK ESTIMATE'             *ERROR=03*            
EREQDATE EQU   3                                                                
TRANNSCH DC    C'<SCHEDULE # MISSING       >'             *ERROR=04*            
EREQNSCH EQU   4                                                                
TRANDTYP DC    C'<DEMOS MISSING            >'             *ERROR=05*            
EREQDTYP EQU   5                                                                
*                                                                               
*RANFULL DC    C'<<< TOO MANY BUY LINES <<<<'             *ERROR=06*            
*                                                                               
TRANFULL DC    C'<MORE THAN 255 BUYLINES   >'             *ERROR=06*            
EREQFULL EQU   6                                                                
TRANRTRN DC    C'<INVALID RE-TRANSFER      >'             *ERROR=07*            
EREQRTRN EQU   7                                                                
*                                                                               
*RANSPOT DC    C'<<< TOO MANY SPOTS <<<<<<<<'           *WARNING=08*            
*                                                                               
TRANSPOT DC    C'<MORE THAN 169 SPOTS/SPLIT>'           *WARNING=08*            
EREQSPOT EQU   8                                                                
TRANCOMB DC    C'<COMBO STATION            >'           *WARNING=09*            
EREQCOMB EQU   9                                                                
*                                                                               
*RANDTS  DC    C'<<< MULTIPLE DAY-TIMES <<<<'           *WARNING=10*            
*                                                                               
TRANDTS  DC    C'ORBIT:UPDATE S/PAK MANUALLY'          *WARNING=10*             
EREQDTS  EQU   10                                                               
*                                                                               
*RANLONG DC    C'<<< SPOT LENGTH TOO LONG <<'           *WARNING=11*            
*                                                                               
TRANLONG DC    C'<INVALID SPOT LEN IN S/PAK>'           *WARNING=11*            
EREQLONG EQU   11                                                               
TRANWEEK DC    C'<ALT WEEK FLAG BAD        >'           *WARNING=12*            
EREQWEEK EQU   12                                                               
TRANBCAN DC    C'<BUY CANCELLED            >'           *WARNING=13*            
EREQBCAN EQU   13                                                               
*                                                                               
*RANCANE DC    C'<CANCELLED BUY MISSING    >'             *ERROR=14*            
*                                                                               
TRANCANE DC    C'<CANX IN REP, DELD IN S/PK>'             *ERROR=14*            
EREQCANE EQU   14                                                               
TRANSTAM DC    C'<STATION NOT IN SPOTPAK   >'             *ERROR=15*            
EREQSTAM EQU   15                                                               
TRANMKTM DC    C'<MARKET NOT IN SPOTPAK    >'             *ERROR=16*            
EREQMKTM EQU   16                                                               
TRANBCLI DC    C'<CLIENT CODE IS BAD       >'             *ERROR=17*            
EREQBCLI EQU   17                                                               
TRANNCLI DC    C'<CLIENT CODE NOT IN S/PAK >'             *ERROR=18*            
EREQNCLI EQU   18                                                               
TRANNPRD DC    C'<PRODUCT CODE NOT IN S/PAK>'             *ERROR=19*            
EREQNPRD EQU   19                                                               
TRANNEST DC    C'<ESTIMATE CODE NOT IN S/PAK'             *ERROR=20*            
EREQNEST EQU   20                                                               
TRANRADV DC    C'<ADV IN REPPAK NOT FOUND  >'             *ERROR=21*            
EREQRADV EQU   21                                                               
TRANRPRD DC    C'<PRD IN REPPAK NOT FOUND  >'             *ERROR=22*            
EREQRPRD EQU   22                                                               
TRANRPTP DC    C'<PPERSON NOT IN REP       >'             *ERROR=23*            
EREQRPTP EQU   23                                                               
TRANRCON DC    C'<CONTRACT DELETED FROM REP>'             *ERROR=24*            
EREQRCON EQU   24                                                               
TRANCANP DC    C'<PAID SPOTS: BUY XCLED    >'           *WARNING=25*            
EREQCANP EQU   25                                                               
TRANSKIP DC    C'<STATION SKIPPED: TEMP    >'           *WARNING=26*            
EREQSKIP EQU   26                                                               
TRANCRED DC    C'<BUY CONTAINS CREDITS     >'        *WARNING=27*               
EREQCRED EQU   27                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DOPRINT --- PRINT THE PRINT LINE TABLE                                 
*                                                                               
INVXFER  NMOD1 0,*IXFR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(PLINED)                              
         USING PLINED,R2                                                        
*                                                                               
         L     R4,8(R1)            RESET A(SPBUY DETAIL)                        
         USING SPBUYDTD,R4                                                      
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'SPERROUT='                                            
***      MVC   P+10(2),SPERROUT                                                 
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         ZIC   R6,SPERROUT+1       SUBERROR NUMBER                              
         BCTR  R6,0                MAKE ZERO RELATIVE                           
         MH    R6,=H'30'                                                        
         LA    RF,IXFRTABL         SET A(INVL XFR MESSAGES)                     
         AR    RF,R6               DISPLACE TO ERROR MESSAGE                    
         MVC   PERROR(LIXFRTAB),0(RF)                                           
*                                  MOVE ERROR MESSAGE TO LINE                   
         XIT1                                                                   
         DROP  R2,R4                                                            
*                1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5                            
IXFRTABL EQU   *                                                                
IXFR0001 DC    C'REPPAK PROD CODE MISSING      '                                
*                                                                               
LIXFRTAB EQU   *-IXFRTABL                                                       
IXFR0002 DC    C'PRODUCT CODES DO NOT MATCH    '                                
IXFR0003 DC    C'ESTIMATE IS NOT BRAND/POL     '                                
IXFR0004 DC    C'INACTIVE ERROR                '                                
IXFR0005 DC    C'SPOTPAK BUY IS MISSING        '                                
IXFR0006 DC    C'SPOT BUY CORRUPT: CALL DDS    '                                
IXFR0007 DC    C'INACTIVE ERROR                '                                
IXFR0008 DC    C'CHANGE MADE ON PAID BUYLINE   '                                
IXFR0009 DC    C'ASSOCIATED SPT BUY NOT ON FILE'                                
IXFR0010 DC    C'RATE CHANGE ON PAID BUYLINE   '                                
IXFR0011 DC    C'SPOT LEN CHANGE ON PAID BUYLN '                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DOPRINT --- PRINT THE PRINT LINE TABLE                                 
*                                                                               
DOPRINT  NMOD1 0,*DPRI*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         L     R2,APNTTAB                                                       
         USING PLINED,R2                                                        
         L     R3,APNTTABC                                                      
         ZIC   R5,ALLOWLIN                                                      
*                                                                               
DPNT10   EQU   *                                                                
         MVI   ALLOWLIN,4                                                       
         LA    R4,P                                                             
         USING PLINERCP,R4                                                      
         CR    R2,R3                                                            
         BE    DPNT100                                                          
         MVC   P(132),0(R2)                                                     
         GOTO1 REPORT              NO  - PRINT DETAIL                           
         CLI   PERROR,C' '                                                      
         BE    DPNT90                                                           
         CLI   PERROR,0                                                         
         BE    DPNT90                                                           
         L     R6,=A(TRANBCAN)                                                  
         A     R6,RELO                                                          
         CLC   PERROR(26),0(R6)           BUY CANCELLED?                        
         BE    DPNT90                     YES, NO RECAP                         
         L     R6,=A(TRANCANP)                                                  
         A     R6,RELO                                                          
         CLC   PERROR(26),0(R6)           BUY CANCELLED/PAID SPOTS?             
         BE    DPNT90                     YES, NO RECAP                         
         L     R6,=A(TRANCRED)                                                  
         A     R6,RELO                                                          
         CLC   PERROR(26),0(R6)           BUY CONTAINS CREDITS?                 
         BE    DPNT90                     YES, NO RECAP                         
         MVC   P,SPACES                                                         
         MVC   PRCPCLIT,=C'CLIENT'                                              
         MVC   PRCPPLIT,=C'PRODUCT'                                             
         MVC   PRCPELIT,=C'ESTIMATE'                                            
         MVC   PRCPCCOD,CLT                                                     
         MVC   PRCPPCOD,PRD                                                     
         MVC   PRCPECOD,EST                                                     
         MVC   PRCPCEXP,CLTNM                                                   
         MVC   PRCPPEXP,PRDNM                                                   
         MVC   PRCPEEXP,ESTNM                                                   
         PUT   RRECAP,(4)                                                       
         PUT   RRECAP,(2)                                                       
         MVC   P,SPACES                                                         
DPNT90   EQU   *                                                                
         LA    R2,132(R2)                                                       
         B     DPNT10                                                           
         DROP  R2,R4                                                            
*                                                                               
DPNT100  EQU   *                                                                
         STC   R5,ALLOWLIN                                                      
*                                                                               
DPNTGOOD EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        INIT --- SET INITIAL VALUES FOR THIS RUN                               
*                                                                               
INIT     NMOD1 0,*INIT*                                                         
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         LA    RF,HHOOK                                                         
         ST    RF,HEADHOOK                                                      
         L     RF,=V(HELLO)                                                     
         A     RF,RELO                                                          
         ST    RF,HELLO                                                         
*                                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         XC    MED,MED                                                          
         XC    RACCUMS(RACCUMSL),RACCUMS                                        
         XC    INRECAP,INRECAP                                                  
         XC    STAERR,STAERR                                                    
         XC    ESTERR,ESTERR                                                    
         MVC   REPADVC,SPACES                                                   
         MVC   REPADVN,SPACES                                                   
         MVC   REPPRDC,SPACES                                                   
         MVC   REPPRDN,SPACES                                                   
         MVC   REPNC#,SPACES                                                    
         MVC   REPNCNM,SPACES                                                   
*                                                                               
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',F'500000',F'500000',RR=RELO               
         OC    DM2(4),DM2                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R2,DM2                                                           
         MVC   0(8,R2),=C'*IOAREA*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,AREC                                                          
         ST    R2,AIOAREA1                                                      
         AH    R2,=H'6100'                                                      
         ST    R2,AIOAREA2                                                      
         AH    R2,=H'6100'                                                      
*                                                                               
         MVC   0(8,R2),=C'*BTABLE*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,ABLDTAB                                                       
         ST    R2,ABLDTABC                                                      
         A     R2,=F'85000'                                                     
*                                                                               
         MVC   0(8,R2),=C'*BSPECS*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,ABSPECS                                                       
         ST    R2,ABSPECSC                                                      
         A     R2,=F'39000'                                                     
*                                                                               
         MVC   0(8,R2),=C'*TTABLE*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,ATRACET                                                       
         ST    R2,ATRACETC                                                      
         A     R2,=F'3900'                                                      
*                                                                               
         MVC   0(8,R2),=C'*REPOUT*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,AOUTREC                                                       
         AH    R2,=H'1008'                                                      
*                                                                               
         MVC   0(8,R2),=C'*BLINES*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,APNTTAB                                                       
         ST    R2,APNTTABC                                                      
         A     R2,=F'100000'                                                    
*                                                                               
         MVC   0(8,R2),=C'*OLDSORT'                                             
         AH    R2,=H'8'                                                         
         ST    R2,AOLDSORT                                                      
         AH    R2,=H'1050'                                                      
*                                                                               
         MVC   0(8,R2),=C'*CPRDL**'                                             
         AH    R2,=H'8'                                                         
         ST    R2,ACPRDLST                                                      
*                                                                               
         L     RE,UTL                                                           
         MVC   SPOTSYS,4(RE)       SET SPOT SYSTEM ENTRY #                      
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'              FIND CONTROL FILE ID REC                  
         MVC   WORK+23(02),RCORIGID   LOAD AGENCY CODE                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND THE SYS AUTHORIZATION ELT               
INIT10   EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   INIT12              NOT FOUND                                    
         CLI   2(R1),X'08'         IS IT 'REP SYSTEM'?                          
         BE    INIT14              YES                                          
INIT12   EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   INIT10              NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
INIT14   EQU   *                                                                
         MVC   REPSYS,3(R1)        LOAD REP SYSTEM NUMBER                       
         L     RE,UTL                                                           
         MVC   4(1,RE),REPSYS                                                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AREC,DMWORK             
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSYS     RELOAD SPOT SYSTEM NUMBER                    
*                                                                               
         OPEN  (REPOUT,(OUTPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    INIT20                                                           
         DC    H'0'                                                             
INIT20   EQU   *                                                                
         OPEN  (RRECAP,(OUTPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    INIT25                                                           
         DC    H'0'                                                             
INIT25   EQU   *                                                                
         OPEN  (DATAIN,(UPDAT))                                                 
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    INIT30                                                           
         DC    H'0'                                                             
INIT30   EQU   *                                                                
         OPEN  (CARDOUT,(OUTPUT))                                               
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    INIT35                                                           
         DC    H'0'                                                             
INIT35   EQU   *                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',=C'NCTFILE X'                     
*                                                                               
         LA    RE,SORTREC                                                       
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL (RE),(RF)                                                        
         L     RE,AOLDSORT                                                      
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL (RE),(RF)                                                        
*                                                                               
         GET   DATAIN                  GET AND PROCESS THE HEADER               
         ST    R1,ASORTREC                                                      
         LR    RE,R1                                                            
         LA    RF,SORTREC                                                       
         LA    R1,SORTRECX-SORTREC                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   HEADIN,X'FF'                                                     
         OC    SORTKEY(2),SORTKEY                                               
         BNZ   INIT105                                                          
         CLC   SORTKEY+2(6),=C'HEADER'                                          
         BE    INIT110                                                          
INIT105  EQU   *                                                                
         MVC   RUNERROR(L'NOHEADER),NOHEADER                                    
         B     INITBAD                                                          
INIT110  EQU   *                                                                
         GOTO1 =V(DATVAL),DMCB,(0,SORTDATA),WORK+0,RR=RELO                      
         OC    DM1,DM1                                                          
         BZ    INIT115                                                          
         L     R2,VMASTC                                                        
         USING MASTD,R2                                                         
         GOTO1 =V(DATVAL),DMCB,(0,MCDATE),WORK+6,RR=RELO                        
         DROP  R2                                                               
         OC    DM1,DM1                                                          
         BZ    INIT115                                                          
         CLC   WORK+0(6),WORK+6                                                 
         BE    INIT120                                                          
INIT115  EQU   *                                                                
         CLI   QOPT5,C'N'          SKIP DATE CHECK?                             
         BE    INIT120             YES - IGNORE NON-MATCH....                   
         MVC   RUNERROR(L'BADDATE),BADDATE                                      
         B     INITBAD                                                          
INIT120  EQU   *                                                                
         CLC   SORTSTAT(4),USED                                                 
         BNE   INIT130                                                          
         CLI   QOPT5,C'N'                  IGNORE USED FLAG                     
         BE    INIT130                                                          
         CLI   QOPT5,C'M'                  IGNORE USED FLAG                     
         BE    INIT130                                                          
         MVI   USEDFLAG,X'FF'                                                   
         B     INIT150                                                          
INIT130  EQU   *                                                                
         LA    RE,SORTREC                                                       
         L     RF,AOUTREC                                                       
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         L     R0,AOUTREC                                                       
         PUT   REPOUT,(0)                                                       
*                                                                               
         CLI   QOPT4,C'N'                  NO UPDATES                           
         BE    INIT150                                                          
         MVC   SORTSTAT(4),USED                                                 
         LA    RE,SORTREC                                                       
         L     RF,ASORTREC                                                      
         LA    R1,SORTRECX-SORTREC                                              
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*   TEST                                                                        
*        MVC   P(25),SORTREC                                                    
*        GOTO1 REPORT                                                           
*   END TEST                                                                    
*                                                                               
****>    PUTX  DATAIN                                                           
INIT150  EQU   *                                                                
*                                                                               
*        INIT EXIT                                                              
*                                                                               
         XC    REPKEY,REPKEY                                                    
         MVI   REPKEY,X'01'                                                     
         MVC   REPKEY+25(2),QAGY                                                
         BAS   RE,REPHIGH                                                       
         CLC   REPKEY(27),REPKSAVE                                              
         BE    INIT160                                                          
         DC    H'0'                                                             
INIT160  EQU   *                                                                
         BAS   RE,REPGET                                                        
*                                                                               
         L     R4,AREC                                                          
         USING RREPREC,R4                                                       
         MVC   SUMRYREP,RREPPROF+13   Y = SUMMARY REPORT BREAKOUT               
         DROP  R4                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     INITEXIT                                                         
INITBAD  EQU   *                                                                
         LA    R0,1                                                             
INITEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  AFTER EFFECTIVE DATES/SPOTS PER WEEK HAVE BEEN TABLED, SCAN                  
*    BUY RECORD FOR X'07' (CR BUY MISSED) ELEMENTS.  WHEN FOUND, FIND           
*    DATE IN TABLE, SUBTRACT NUMBER OF SPOTS MISSED FROM #/WEEK                 
*                                                                               
CREDITS  NMOD1 0,*CRED*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE - 1ST REG)                 
         L     RA,4(R1)            RESET A(WORKSPACE - 2ND REG)                 
         L     R2,8(R1)            A(BUY RECORD)                                
         L     R3,ABLDTABC                                                      
         USING SPBUYDTD,R3                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'07',(R2)),(0,0),(0,0)               
         CLI   DM4,0               ANY ELEMENT FOUND?                           
         BNE   CRED0020            NO  - FINISHED                               
         MVI   SPMEMO,EREQCRED     WARNING MESSAGE:                             
*                                    BUY CONTAINS CREDITS                       
         L     R4,DM4              A(1ST ELEMENT)                               
         USING RBUYCREL,R4                                                      
CRED0004 EQU   *                                                                
         L     R5,SPSPECL          A(DATE/#SPOTS PER WEEK TABLE)                
*                                                                               
*   SET UP DATE FOR EXACT DATE MATCH: CONVERT TO COMPRESSED FORMAT              
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYCRDT),(2,WORK+24)                             
*                                                                               
*   CONVERT MISSED DATE TO YYMMDD EBCDIC                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYCRDT),(0,WORK+6)                              
*                                                                               
*   GET DAY OF WEEK OF MISSED DATE                                              
*                                                                               
         GOTO1 =V(GETDAY),DMCB,WORK+6,WORK+12                                   
         MVC   WORK+12(6),WORK+6   LOAD FOR FINAL CONVERT                       
*                                       IF DATE IS A MONDAY                     
         CLI   DMCB,1              IS IT MONDAY?                                
         BE    CRED0006            YES - USE IT AS IS                           
         ZIC   RF,DMCB             NO  - GET MONDAY DATE FROM IT                
         BCTR  RF,0                SUBTRACT 1 FROM 1                            
         LNR   RF,RF               MAKE IT NEGATIVE                             
*                                                                               
*   USE ADJUSTED DAY TO CALCULATE MONDAY OF WEEK                                
*                                                                               
         GOTO1 =V(ADDAY),DMCB,WORK+6,WORK+12,(RF)                               
CRED0006 EQU   *                                                                
*                                                                               
*   GENERATE DATE IN COMPRESSED FORMAT                                          
*                                                                               
         GOTO1 DATCON,DMCB,WORK+12,(2,WORK)                                     
CRED0008 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE?                                
         BE    CRED0016            YES - LOOK FOR NEXT X'07'                    
         CLC   WORK(2),0(R5)       SAME DATE (MONDAY-ORIENTED)?                 
         BE    CRED0012            YES - PROCESS                                
         CLC   WORK+24(2),0(R5)    SAME DATE (ORIGINAL)?                        
         BE    CRED0012            YES - PROCESS                                
*                                                                               
*   EXACT DATE MATCHES HAVE FAILED.  NEXT CHECK WILL SEE IF DATE                
*      OF SPOT IS WITHIN WEEK RANGE OF TABLE ENTRY BY:                          
*        1.    CONVERTING TABLE DATE TO YYMMDD EBCDIC                           
*        2.    ADDING 6 DAYS TO DATE TO GIVE ONE-WEEK SPREAD                    
*        3.    CONVERTING NEW DATE BACK TO COMPRESSED FORMAT                    
*        4.    COMPARING ORIGINAL MAKE GOOD DATE TO THIS RANGE                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R5)),(0,WORK+30)                                
         GOTO1 =V(ADDAY),DMCB,WORK+30,WORK+36,6                                 
         GOTO1 DATCON,DMCB,(0,WORK+36),(2,WORK+30)                              
*                                                                               
         CLC   WORK+24(2),0(R5)    ORIG DATE VS WEEK START                      
         BL    CRED0010            EARLIER - REJECTED                           
         CLC   WORK+24(2),WORK+30  ORIG DATE VS WEEK START+6 DAYS               
         BNH   CRED0012            WITHIN - ACCEPT                              
CRED0010 EQU   *                                                                
         LA    R5,3(R5)            NOT FOUND -                                  
         B     CRED0008               GO BACK FOR NEXT                          
CRED0012 EQU   *                                                                
         ZIC   RF,2(R5)            #SPOTS/WEEK FROM TABLE                       
         ZIC   RE,RBUYCRSP         MISSED #SPOTS FROM X'07'                     
         SR    RF,RE               SUBTRACT MISSED FROM TOTAL                   
         STC   RF,2(R5)            PUT IT BACK IN TABLE                         
CRED0016 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT RECORD ELEMENT                  
         AR    R4,RF                                                            
         CLI   0(R4),X'07'         ANOTHER X'07'?                               
         BE    CRED0004            YES - LOOP THROUGH TABLE                     
CRED0020 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SBUYRECD DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
REPADVD  DSECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
REPPRDD  DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
REPPTPD  DSECT                                                                  
       ++INCLUDE REGENPTP                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
REPCONDD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE SPBUYGEND                                                      
         EJECT                                                                  
       ++INCLUDE SPBUYDETDA                                                     
         EJECT                                                                  
*                                                                               
* DSECT FOR DETAIL PRINT LINE                                                   
*                                                                               
PLINED   DSECT                                                                  
PSTATION DS    CL6                                                              
         DS    CL1                                                              
PUPDATE  DS    CL1                                                              
         DS    CL1                                                              
PSPOTLIN DS    CL3                                                              
         DS    CL1                                                              
PSPOTDAT DS    CL11                                                             
         DS    CL1                                                              
PSPOTSPT DS    CL5                                                              
         DS    CL1                                                              
PSPOTDOL DS    CL11                                                             
         DS    CL5                                                              
PREPREP  DS    CL2                                                              
         DS    CL2                                                              
PREPAGY  DS    CL7                                                              
         DS    CL1                                                              
PREPSTA  DS    CL6                                                              
         DS    CL2                                                              
PREPCON  DS    CL8                                                              
         DS    CL2                                                              
PREPLIN  DS    CL3                                                              
         DS    CL1                                                              
PREPSPT  DS    CL5                                                              
         DS    CL1                                                              
PREPDOL  DS    CL11                                                             
         DS    CL1                                                              
PERROR   DS    CL30                TRANSFER ERROR MESSAGE                       
         SPACE 2                                                                
*                                                                               
* DSECT FOR STATION TOTAL PORTION                                               
*                                                                               
PLINESTA DSECT                                                                  
         DS    CL9                                                              
PSTADASH DS    CL33                                                             
         ORG   PSTADASH                                                         
PSTALIT  DS    CL14                                                             
         DS    CL2                                                              
PSTASPT  DS    CL5                                                              
         DS    CL1                                                              
PSTADOL  DS    CL11                                                             
         SPACE 2                                                                
*                                                                               
* DSECT FOR ESTIMATE TOTAL PORTION                                              
*                                                                               
PLINEEST DSECT                                                                  
         DS    CL1                                                              
PESTLIT  DS    CL18                                                             
         DS    CL6                                                              
PESTSPT  DS    CL5                                                              
         DS    CL1                                                              
PESTDOL  DS    CL11                                                             
         SPACE 2                                                                
*                                                                               
* DSECT FOR CONTRACT TOTAL PORTION                                              
*                                                                               
PLINECON DSECT                                                                  
         DS    CL63                                                             
PCONDASH DS    CL35                                                             
         ORG   PCONDASH                                                         
PCONLIT  DS    CL15                                                             
         DS    CL3                                                              
PCONSPT  DS    CL5                                                              
         DS    CL1                                                              
PCONDOL  DS    CL11                                                             
         SPACE 2                                                                
*                                                                               
* DSECT FOR REPPAK ADV/PRD TOTAL PORTION                                        
*                                                                               
PLINERAP DSECT                                                                  
         DS    CL49                                                             
PRAPLIT  DS    CL25                                                             
         DS    CL7                                                              
PRAPSPT  DS    CL5                                                              
         DS    CL1                                                              
PRAPDOL  DS    CL11                                                             
         SPACE 2                                                                
*                                                                               
* DSECT FOR ERROR RECAP LINE 1                                                  
*                                                                               
PLINERCP DSECT                                                                  
PRCPCLIT DS    CL6                                                              
         DS    CL2                                                              
PRCPCCOD DS    CL3                                                              
         DS    CL2                                                              
PRCPCEXP DS    CL20                                                             
         DS    CL2                                                              
PRCPPLIT DS    CL7                                                              
         DS    CL2                                                              
PRCPPCOD DS    CL3                                                              
         DS    CL2                                                              
PRCPPEXP DS    CL20                                                             
         DS    CL2                                                              
PRCPELIT DS    CL8                                                              
         DS    CL2                                                              
PRCPECOD DS    CL3                                                              
         DS    CL2                                                              
PRCPEEXP DS    CL20                                                             
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
         ORG   DPTTAB                                                           
         DS    0F                                                               
*                                                                               
*        SAVED REGS FOR NMOD ROUTINES                                           
*                                                                               
SPR2R8   DS    F                              SAVED REG 8                       
SPR2R9   DS    F                              SAVED REG 9                       
SPR2RA   DS    F                              SAVED REG A                       
*                                                                               
* LOCAL ADDRESSES                                                               
*                                                                               
RELO     DS    A                                                                
ABLDTAB  DS    A                          A(BLDTABLE)                           
ABLDTABC DS    A                          A(CURRENT BLDTABLE ENTRY)             
ABSPECS  DS    A                          A(BUY SPECS LIST)                     
ABSPECSC DS    A                          A(CURRENT BSPECS ENTRY)               
ATRACET  DS    A                          A(TRACETAB)                           
ATRACETC DS    A                          A(CURRENT TRACETAB ENTRY)             
APNTTAB  DS    A                          A(PRINTTAB)                           
APNTTABC DS    A                          A(CURRENT PRINTTAB ENTRY)             
AOUTREC  DS    A                          A(OUTREC)                             
AIOAREA1 DS    A                          A(IOAREA1)                            
AIOAREA2 DS    A                          A(IOAREA2)                            
AOLDSORT DS    A                          A(PREVIOUS SORT RECORD)               
ACPRDLST DS    A                          A(PRODUCT LIST FROM CLT HDR)          
HELLO    DS    A                          LINKED HELLO                          
ABUYCOM  DS    A                          A(BUY COMMENT IN TABLE)               
*                                                                               
* REPORT ACCUM'S                                                                
*                                                                               
RACCUMS  EQU   *                                                                
SPOTSTAS DS    F                          SPOTPAK-STA BREAK SPOTS               
SPOTSTAD DS    F                          SPOTPAK-STA BREAK $                   
SPOTESTS DS    F                          SPOTPAK-EST BREAK SPOTS               
SPOTESTD DS    F                          SPOTPAK-EST BREAK $                   
SPOTAGYS DS    F                          SPOTPAK-AGY BREAK SPOTS               
SPOTAGYD DS    F                          SPOTPAK-AGY BREAK $                   
SPOTRUNS DS    F                          SPOTPAK-RUN BREAK SPOTS               
SPOTRUND DS    F                          SPOTPAK-RUN BREAK $                   
REPCONS  DS    F                          REPPAK-CON BREAK SPOTS                
REPCOND  DS    F                          REPPAK-CON BREAK $                    
REPAPS   DS    F                          REPPAK-ADV/PRD BREAK SPOTS            
REPAPD   DS    F                          REPPAK-ADV/PRD BREAK $                
REPAGYS  DS    F                          REPPAK-AGY BREAK SPOTS                
REPAGYD  DS    F                          REPPAK-AGY BREAK $                    
REPRUNS  DS    F                          REPPAK-RUN BREAK SPOTS                
REPRUND  DS    F                          REPPAK-RUN BREAK $                    
RACCUMSX EQU   *                                                                
RACCUMSL EQU   RACCUMSX-RACCUMS                                                 
*                                                                               
*        HEADLINE NAME FIELDS (IN PRINTABLE FORM)                               
*                                                                               
REPCODE  DS    CL2                     REP CODE                                 
REPADVC  DS    CL4                         ADVERTISER CODE                      
REPADVN  DS    CL20                                   NAME                      
REPPRDC  DS    CL3                         PRODUCT CODE                         
REPPRDN  DS    CL20                                NAME                         
REPNC#   DS    CL8                         NETWORK CONTRACT NUMBER              
REPNCNM  DS    CL20                                         DESCRIPTION         
REPPPC   DS    CL3                         POINTPERSON CODE                     
REPPPN   DS    CL20                                    NAME                     
REPPPR   DS    CL2                                     REP                      
REPPPO   DS    CL2                                     OFFICE                   
REPAGYC  DS    CL7                         AGENCY CODE (W/ CITY)                
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE SPREPPTBUF                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'209SPREPR202 09/10/04'                                      
         END                                                                    
