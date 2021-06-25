*          DATA SET REREPTK02S AT LEVEL 085 AS OF 05/01/02                      
*PHASE RETK02A,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE REGENBUC                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPTK02 - TAKEOVER REPORT'                                    
*********************************************************************           
*                                                                   *           
*        REREPTK02 --- REPPAK TAKEOVER REPORT                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* OCT07/97 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* DEC08/97 (BU ) --- REARRANGE COLUMNS, ADD 'OLD $$'                *           
*                                                                   *           
* DEC12/97 (BU ) --- MODIFY DEVELOPMENT OF 'OLD $$'                 *           
*                                                                   *           
* FEB19/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* MAR11/98 (BU ) --- PROPERLY PROCESS DELETED RECORDS               *           
*                                                                   *           
* JUN35/98 (BU ) --- ADD ZERO RECORD CHECK                          *           
*                                                                   *           
* JAN13/99 (RHV) --- HANDLE NON-DELETED CANCELLED LINES             *           
*                                                                   *           
* APR01/99 (JRD) --- CHANGE FORMAT TO ALLOW AGY/ADV NAMES           *           
*                                                                   *           
* JUL13/00 (BU ) --- FIX MG=/CR= TESTING                            *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
RETK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RETK02,R7,RR=RE                                              
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
*                                  PRODUCTION OF THE REPORT WILL BE             
*                                     FROM THE 'RPTDONE' ROUTINE                
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
CARDSRCE EQU   80+Q2NCNUM-QREC2    -> NETWK CON# ON 2ND CARD                    
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
         MVC   SRCESAVE,CARDSRCE(RF)                                            
*                                  MOVE SOURCE REP NAME TO SAVE                 
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
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC,SORTREC                                                  
         XC    SORTREC2,SORTREC2                                                
         BAS   RE,SRCESIDE         ACCESS SOURCE INFORMATION                    
         OC    RECCOUNT,RECCOUNT   ANY RECORDS FOUND?                           
         BNZ   INIT0060            YES - PROCESS                                
         GOTO1 REPORT                                                           
         MVC   P+20(38),=C'**  NO CONTRACTS FOUND FOR TAKEOVER  **'             
         GOTO1 REPORT                                                           
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     MODEEXIT                                                         
INIT0060 EQU   *                                                                
         BAS   RE,RPTDONE          PRODUCE REPORT FROMF SORTED INFO             
         B     MODEEXIT                                                         
         EJECT                                                                  
MODEEXIT LTR   R0,R0                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SRCESIDE:  SWITCH TO SOURCE FILE, ACCESS INFORMATION ON                     
*        AVAILABILITY                                                           
*                                                                               
SRCESIDE NTR1                                                                   
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO,0                                              
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
*                                                                               
         MVC   WORK+15(06),SRCESAVE                                             
*                                                                               
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                      
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
SRCE0010 EQU   *                                                                
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   SRCE0015            NO                                           
         MVC   SRCEREP,2(R1)       YES - SAVE 2-CHAR REP ID                     
*                                                                               
*   TEST DISPLAY REP                                                            
*        MVC   P+1(08),=C'SRC REP:'                                             
*        MVC   P+10(2),SRCEREP                                                  
*        GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
         B     SRCE0020            BUMP TO NEXT ELEMENT                         
SRCE0015 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   SRCE0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    SRCE0030            YES                                          
SRCE0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   SRCE0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
SRCE0030 EQU   *                                                                
         MVC   NEWUTL,3(R1)        OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   SAVEVUTL,4(RE)      SAVE ORIGINAL SYSTEM NUMBER                  
         MVC   4(1,RE),NEWUTL      INSERT NEW UTL NUMBER                        
*                                                                               
*   TEST DISPLAY UTL                                                            
*        MVC   P+1(12),=C'ORIG/NEW UTL'                                         
*        MVC   P+16(1),SAVEVUTL                                                 
*        MVI   P+17,C'/'                                                        
*        MVC   P+18(1),NEWUTL                                                   
*        GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
*   OPEN REP FILE IF BOTH NOT ON SAME FILE                                      
*                                                                               
         CLC   SAVEVUTL,NEWUTL     SAME UTL NUMBERS?                            
         BE    SRCE0210            YES - DON'T OPEN AGAIN                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,AIO                        
*                                                                               
SRCE0210 EQU   *                                                                
*                                                                               
*   CYCLE THROUGH REP'S CONTRACTS IN SAME MANNER AS TAKEOVER ON-LINE            
*                                                                               
***>>>                                                                          
SRCE0220 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING RCON8TYP,R2                                                      
                                                                                
         MVI   RCON8ETP,X'8E'      STA/FLIGHT/CON#                              
*                                                                               
         MVC   RCON8ERP,SRCEREP    INSERT SOURCE REP                            
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
         CLC   KEY(8),KEYSAVE      SAME KEY TYPE/REP/STATION?                   
         BNE   SRCE0600            NO  - FINISHED SWEEP                         
*                                                                               
*   DOESN'T MATTER WHEN ORDER BEGAN.  WHAT IS IMPORTANT IS THAT IT              
*        WAS ACTIVE AFTER THE EFFECTIVE DATE OF THE TAKEOVER.                   
*        THEREFORE, THE TEST FOR START DATE IS DEACTIVATED                      
*                                                                               
         CLC   RCON8EFE,CEFFDATE   KEY END DATE VS EFFECTIVE DATE               
*                                  ENDED BEFORE EFF DATE?:  SKIP                
         BNL   SRCE0320            NO  - DATES INCLUDE EFFECTIVE DATE           
*                                                                               
*   TEST KEYS                                                                   
*        MVC   P+1(10),=C'SKIP: DATE'                                           
*        MVC   P+12(27),KEY                                                     
*        MVC   P+42(2),CEFFDATE                                                 
*        GOTO1 REPORT                                                           
*   TEST KEYS EXIT                                                              
*                                                                               
SRCE0300 EQU   *                                                                
         MVI   RCON8EID,10         SET TYPE UP TO SKIP TO NEXT CON              
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     SRCE0240            RESTART ON NEXT KEY                          
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
         CLI   RCONTYPE,C'N'       DON'T TAKE OVER NETWORK ORDERS               
         BE    SRCE0400                                                         
         CLI   RCONTYPE,C'X'       DON'T TAKE OVER NETWORK ORDERS               
         BE    SRCE0400                                                         
*                                                                               
*        NEED TO CHECK SOFT CONTYPE FOR NETWORK HERE                            
*                                                                               
*        CHECK CONFIRM/VERSION/WIP STATUS:  SKIP WIP'S                          
*                                                                               
*                                                                               
*                                                                               
         LA    RF,RCONELEM         FIND X'20' ELEMENT                           
         MVI   SCMBOFL1,C' '       CLEAR COMBO FLAG                             
SRCE0450 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    SRCE0400            YES - SKIP THIS ORDER                        
*                                     SHOULD HAVE BEEN THERE                    
         CLI   0(RF),X'17'         COMBO ELEMENT?                               
         BNE   SRCE0452            NO                                           
         MVI   SCMBOFL1,C'C'       YES - SET FLAG                               
SRCE0452 EQU   *                                                                
         CLI   0(RF),X'20'         SEND INFO ELEMENT?                           
         BE    SRCE0454            YES - CHECK IT                               
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     SRCE0450            GO BACK FOR NEXT                             
SRCE0454 EQU   *                                                                
         MVI   CNFORVER,0          CLEAR TO 'CONFIRMED ORDER' STATUS            
         TM    RCONSENF-RCONSEND(RF),X'02'                                      
*                                  LAST CONFIRMED BY STATION?                   
         BO    SRCE0460            YES - ACCEPT IT                              
         MVI   CNFORVER,1          SET TO 'VERSION' STATUS                      
         TM    RCONSENF-RCONSEND(RF),X'20'+X'10'                                
*                                  STA # ADVANCED?                              
         BO    SRCE0460            BOTH SIDES ON:  ACCEPT IT                    
         B     SRCE0400            IN PROCESS ON ONE SIDE OR                    
*                                     OTHER:  SKIP IT                           
SRCE0460 EQU   *                                                                
*                                                                               
**<<>>**                                                                        
         XC    ACCBUY$$,ACCBUY$$   CLEAR ACCUMULATOR                            
         BAS   RE,TAKEBUYS                                                      
         L     RE,ACCBUY$$         LOAD ACCUM'ED BUY DOLLARS                    
         STCM  RE,15,SDOLLARO      INSERT OLD $$ INTO SORTREC                   
*                                                                               
**<<>>**                                                                        
         L     RF,CONCOUNT         COUNT UP THE CONTRACTS                       
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
*                                                                               
*   TEST DISPLAY KEYS                                                           
*        MVC   P+1(08),=C'SELECTED'                                             
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4,=C'TOG'                              
*        MVC   P+22(27),RCONKEY                                                 
*        GOTO1 REPORT                                                           
*   TEST DISPLAY KEYS END                                                       
*                                                                               
         GOTO1 SORTGEN             GENERATE RECORD FOR SORT                     
         L     RF,RECCOUNT         INCREMENT RECORD COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,RECCOUNT                                                      
         B     SRCE0300            GO BACK FOR NEXT                             
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
***>>>                                                                          
*                                                                               
*   TAKEBUYS:  READ ALL BUYS FROM ORIGINAL ORDER, CUT BACK AS NEEDED,           
*        AND ACCUMULATE TAKEOVER ORDER DOLLARS.                                 
*                                                                               
TAKEBUYS NTR1  0,*TKBY*                                                         
***>>>   L     RC,0(R1)            RESET A(WORKSPACE)                           
***>>>                                                                          
*                                                                               
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                  DON'T READ DELETED BUYS                      
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON(4)                                           
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT BUY KEY TYPE                          
         MVC   KEY+16(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+18(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 HIGH                ACCESS FIRST KEY                             
         B     TKBY0080                                                         
TKBY0040 EQU   *                                                                
         GOTO1 SEQ                 ACCESS NEXT KEY                              
TKBY0080 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP/CON#?                      
         BNE   TKBY0320            NO  - FINISHED WITH CONTRACT                 
*                                                                               
         BAS   RE,GETBUY                                                        
*                                  RETRIEVE ORIGINAL BUY RECORD                 
*                                                                               
         CLI   RBUYCHGI,C'C'                                                    
         BE    TKBY0040            SKIP CANCELLED LINES                         
*                                                                               
         BAS   RE,BUYBACK          YES - CUT BACK BUYS AND REGENERATE           
*                                     ESTIMATE BUCKETS                          
         B     TKBY0040            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKBY0320 EQU   *                                                                
         MVC   KEY,KEYSAV2         RESET ORIGINAL CONTRACT KEY                  
         GOTO1 HIGH                READ TO RESET KEY SEQ                        
         B     EXIT                                                             
         EJECT                                                                  
***>>>                                                                          
*INSRT*>                                                                        
*                                                                               
*   BUYBACK:  ADJUSTS BUY FLIGHT DATES, BASED ON TAKEOVER DATE,                 
*        RECALCULATES VALUE OF BUYLINE, AND ACCUMULATES IT FOR                  
*        INSERTION INTO SORT RECORD                                             
*                                                                               
BUYBACK  NTR1                                                                   
         XC    SVTOTSPT,SVTOTSPT   CLEAR ACCUMULATORS                           
         XC    SVTOTWKS,SVTOTWKS                                                
*                                                                               
*                                                                               
*   PROCESS EACH 03 (EFFECTIVE DATE) ELEMENT:                                   
*        1.  IF END BEFORE TKO DATE, DROP ELEMENT                               
*        2.  IF START NOT EARLIER THAN TKO DATE, USE AS-IS                      
*        3.  IF START EARLIER, USE TKO DATE, ADJUST ELEMENT                     
*        4.  RECALCULATE TOTAL BUY FIGURES FROM 03 ELEMENT DETAILS              
*                                                                               
         LA    R3,RBUYELEM                                                      
BUYB0040 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0200            YES                                          
         CLI   0(R3),3             EFFECTIVE DATE ELEMENT?                      
         BNE   BUYB0180            NO  - BUMP TO NEXT ELEMENT                   
         USING RBUYDTEL,R3                                                      
*                                                                               
         CLC   BEFFDATE,RBUYDTED   TKO DATE VS EFF DATE END                     
         BH    BUYB0160            EFF DATE END PRIOR TKO DATE -                
*                                     DROP THE ELEMENT                          
*                                  EXAMPLE:  BEFFDATE = JUL1/97                 
*                                            EFF END  = JUN1/97                 
*                                                                               
         CLC   BEFFDATE,RBUYDTST   TKO DATE VS EFF DATE START                   
         BH    BUYB0060            EFF DATE START PRIOR TKO DATE                
*                                     CUT BACK ELEMENT - REGEN BUCKETS          
*                                  EXAMPLE:  BEFFDATE = JUL1/97                 
*                                            EFF STRT = JAN1/97                 
*                                                                               
         B     BUYB0140            EFF DATE START =/AFTER TKO DATE              
*                                     LEAVE ALONE - REGEN BUCKETS               
*                                  EXAMPLE:  BEFFDATE = JUL1/97                 
*                                            EFF STRT = JUL1/97 (OR)            
*                                            EFF STRT = SEP1/97                 
BUYB0060 EQU   *                                                                
*                                                                               
*   TKO DATE IS ALWAYS A MONDAY.  CHECK BUY START DATE.  IF NOT                 
*        MONDAY, ADJUST BEFFDATE INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  ORIGINAL BUY START DATE -> EBCDIC            
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    BUYB0080            YES - USE TKO DATE AS IS                     
*                                  NO  - OFFSET TO ORIG BUY START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,BEFFDATE),(0,WORK)                                
*                                  CONVERT TKO DATE TO EBCDIC                   
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT OOWR START TO BINARY                 
         CLC   RBUYDTED,RBUYDTST                                                
         BL    BUYB0160            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
         B     BUYB0100                                                         
BUYB0080 EQU   *                                                                
         MVC   RBUYDTST,BEFFDATE   RESET EFF START TO TKO DATE                  
BUYB0100 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  EBCDIC START DATE                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
*                                  EBCDIC END   DATE                            
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    BUYB0120            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
BUYB0120 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   BUYB0140            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    BUYB0130            YES - JUST RECALC # WEEKS                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  CONVERT BUY START DATE TO EBCDIC             
         GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
*                                  BUMP TO NEXT WEEK                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT NEW START TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
BUYB0130 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
BUYB0140 EQU   *                                                                
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         L     RF,SVTOTWKS         CALCULATE TOTAL NUMBER OF WEEKS              
         AR    RF,RE                                                            
         ST    RF,SVTOTWKS         SAVE TOTAL NUMBER OF WEEKS                   
         MVC   HALF,RBUYDTNW       GET SPOTS/WEEK                               
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         L     RF,SVTOTSPT         CALCULATE TOTAL NUMBER SPOTS                 
         AR    RF,RE                                                            
         ST    RF,SVTOTSPT         SAVE TOTAL NUMBER SPOTS                      
         B     BUYB0180                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
BUYB0160 EQU   *                                                                
         MVI   0(R3),X'FF'         SET ELEMENT FOR DELETION                     
BUYB0180 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     BUYB0040            GO BACK FOR NEXT ELEMENT                     
BUYB0200 EQU   *                                                                
*                                                                               
*   TEST BRANCH                                                                 
*        B     BUYB0320                                                         
*   TEST END                                                                    
*                                                                               
*   PROCESS EACH 05 (BUY MG REF ELT)/56 (MG SPLITOUT ELT)                       
*        1.  IF DATE BEFORE TKO DATE, DROP ELEMENT                              
*                                                                               
         LA    R3,RBUYELEM                                                      
         SR    RF,RF               CLEAR REG FOR INDICATOR                      
         MVI   MGBITFLG,0          CLEAR FLAG BITS                              
         XC    ELEM,ELEM           CLEAR ELEMENT BUILD AREA                     
BUYB0220 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0300            YES                                          
         CLI   0(R3),4             BUY COMMENT ELEMENT?                         
         BNE   BUYB0230            NO  -                                        
         LTR   RF,RF               YES - PRIOR COMMENT FOUND?                   
         BNZ   BUYB0230            YES - DON'T SAVE ADDRESS                     
         LR    RF,R3               NO  - SAVE A(COMMENT RECORD)                 
BUYB0230 EQU   *                                                                
         CLI   0(R3),5             BUY MG REF ELT?                              
         BE    BUYB0240            YES - PROCESS IT                             
         CLI   0(R3),X'56'         BUY MG SPLITOUT ELT?                         
         BE    BUYB0260            YES - PROCESS IT                             
         B     BUYB0280            NO  - BUMP TO NEXT ELT                       
BUYB0240 EQU   *                                                                
         USING RBUYMGEL,R3                                                      
*                                                                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0245            YES - DROP THEM ALL                          
*                                                                               
         CLC   RBUYMGD1,BEFFDATE   MAKEGOOD MISSED DATE < EFF DATE?             
*                                                                               
         DROP  R3                                                               
*                                                                               
         BNL   BUYB0280            NO  - LEAVE ELT ALONE                        
         OI    MGBITFLG,X'40'      INDICATE AT LEAST ONE X'05' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
BUYB0245 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
         CLC   =C'MG=',2(RF)       COMMENT INDICATES MAKEGOOD?                  
         BE    BUYB0250            YES                                          
         CLC   =C'MG>',2(RF)       NO  - COMMENT = MAKEGOOD MODIFIED?           
         BE    BUYB0280            YES                                          
         CLC   =C'CR=',2(RF)       COMMENT INDICATES CREDIT?                    
         BE    BUYB0250            YES                                          
         CLC   =C'CR>',2(RF)       NO  - COMMENT = CREDIT   MODIFIED?           
         BE    BUYB0280            YES                                          
*                                                                               
*   FOLLOWING DUMP IS REMOVED.  NEW M/G HAS X'04' ELEMENTS WHICH ARE            
*        NEITHER MG= OR CR=, SO THIS IS NO LONGER APPLICABLE.  IN THIS          
*        CASE, THE RECORDS ARE NOT BEING OUTPUT, SO MODIFYING THEM IS           
*        NOT NECESSARY.  THE PROCESSING IS UNCHANGED OTHER THAN                 
*        NO DUMP.                                                               
***      DC    H'0'                NO  - WHAT IS HERE???                        
BUYB0250 EQU   *                                                                
         MVC   RBUYKMLN,RBUYKLIN   SET MASTER LIN# = MAIN LIN#                  
         MVI   4(RF),C'>'          SET MODIFIED MAKEGOOD FLAG                   
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK UP 1 FOR EX                             
         EX    RE,BUYB0255         MOVE BY LENGTH                               
         MVI   0(RF),X'FF'         SET ELEMENT FOR DELETE                       
         LA    RE,1(RE)            RESET ORIGINAL LENGTH                        
         LA    R1,ELEM             SET A(NEW ELEMENT)                           
         AR    R1,RE               SET TO PAST LAST CHAR                        
         MVC   0(9,R1),=C'-TAKEOVER'                                            
         ZIC   RE,ELEM+1                                                        
         LA    RE,9(RE)            ADD 9 TO ELEMENT LENGTH                      
         STC   RE,ELEM+1           PUT NEW LENGTH BACK                          
         B     BUYB0258                                                         
BUYB0255 EQU   *                                                                
         MVC   ELEM(0),0(RF)       MOVE COMMENT BY LENGTH                       
BUYB0258 EQU   *                                                                
         B     BUYB0280            BUMP TO NEXT ELEMENT                         
BUYB0260 EQU   *                                                                
         USING RBYMGSEL,R3                                                      
*                                                                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0270            YES - DROP THEM ALL                          
*                                                                               
         CLC   RBMGMSDT,BEFFDATE   MAKEGOOD MISSED DATE < EFF DATE?             
         BNL   BUYB0280            NO  - LEAVE ELT ALONE                        
         OI    MGBITFLG,X'80'      INDICATE AT LEAST ONE X'56' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
         DROP  R3                                                               
*                                                                               
BUYB0270 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
BUYB0280 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     BUYB0220            GO BACK FOR NEXT ELEMENT                     
BUYB0300 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0            
*                                  DELETE EFF DATE ELTS PRIOR TO                
*                                     TKO DATE                                  
BUYB0320 EQU   *                                                                
         SR    RE,RE                                                            
*        L     RF,SVTOTSPT         LOAD TOTAL # SPOTS                           
*        M     RE,SVTOTWKS         TOT SPOTS * # WEEKS =                        
         GOTO1 BUCKUP,DMCB,RBUYREC                                              
         L     RF,ACCBUY$$                                                      
         ZICM  RE,RBUYTCOS,4                                                    
         AR    RF,RE                                                            
         ST    RF,ACCBUY$$         TOTAL COST OF BUY                            
         B     EXIT                                                             
*                                                                               
MGBITFLG DS    CL1                                                              
ELEM     DS    CL128                                                            
         EJECT                                                                  
         DS    0F                                                               
*                                                                               
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
BUCKUP   NTR1                                                                   
         MVC   DMCB(4),0(R1)                                                    
*                                                                               
* BUILD BUCKETS                                                                 
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         GOTO1 =V(REGENBUC),DMCB,,WORK2,WORK,RR=YES                             
*                                                                               
* ADD BUCKETS TO CONREC                                                         
         CLC   WORK2(2),=H'2'      NONE?                                        
         BE    UPXIT                                                            
*                                                                               
         SR    R4,R4                                                            
         MVC   HALF,WORK2                                                       
         LH    R5,HALF             LEN OF BUCKETS                               
         LA    R5,WORK2-1(R5)                                                   
         LA    R3,WORK2+2          1ST BUCKET                                   
*                                                                               
* ADD BUCKET TO CONREC (OR SUBTRACT)                                            
BU100    MVI   1(R3),10            K BUCKET LENGTH                              
*        GOTO1 ADDBUCK,DMCB,RCONREC,(R3)                                        
         MVI   1(R3),14            RESTORE LENGTH                               
         IC    R4,1(R3)                                                         
         BXLE  R3,R4,BU100         NEXT BUCKET                                  
UPXIT    B     EXIT                                                             
*                                                                               
WORK2    DS    CL256                                                            
         EJECT                                                                  
***>>>                                                                          
                                                                                
         SPACE 1                                                                
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*             SORT FILE                                                         
*             PRODUCE REPORT                                                    
*                                                                               
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
*                                     WITH REP UTL CODE                         
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),SAVEVUTL    INSERT ORIGINAL UTL NUMBER                   
*                                                                               
*   TEST DISPLAY UTL                                                            
*        MVC   P+1(12),=C'ORIG/NEW UTL'                                         
*        MVC   P+16(1),SAVEVUTL                                                 
*        MVI   P+17,C'/'                                                        
*        MVC   P+18(1),NEWUTL                                                   
*        GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
         OPEN  (RGWORK,OUTPUT)     OPEN WORK FILE                               
*                                                                               
*   OPEN ORIGINAL REP FILE IF BOTH NOT ON SAME FILE                             
*                                                                               
         CLC   SAVEVUTL,NEWUTL     SAME UTL NUMBERS?                            
         BE    RDON0020            YES - DON'T OPEN AGAIN                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,AIO                        
RDON0020 EQU   *                                                                
         BAS   RE,GETSORT          RETURN ORIGINAL FILE                         
*                                                                               
         CLI   STYP,X'FF'          EOF FIRST PASS?                              
         BE    RDON0300            YES                                          
         XC    KEY,KEY             NO  - CHECK FOR TAKEOVER VALUES              
         MVI   KEY,X'AD'           INSERT PASSIVE                               
         MVC   KEY+12(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+14(5),QSTATION                                               
         MVC   KEY+19(4),SOLDCON#  INSERT SOURCE CONTRACT NUMBER                
         GOTO1 HIGH                                                             
*                                                                               
*   TEST DISPLAY REP                                                            
*        MVC   P+1(08),=C'KEY:    '                                             
*        MVC   P+10(27),KEY                                                     
*        MVC   P+41(08),=C'KEYSAVE:'                                            
*        MVC   P+50(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST DISPLAY REP END                                                        
*                                                                               
         CLC   KEY(23),KEYSAVE     CONTRACT KEY FOUND?                          
*                                     CHECK THRU PREVIOUS CON#                  
         BNE   RDON0220            NO  - PUT OUT RECORD AS-IS                   
         BAS   RE,GETCON           YES - RETRIEVE RECORD                        
         LA    R6,RCONELEM                                                      
RDON0040 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    RDON0220            YES - NO X'2A' ELEMENT                       
         CLI   0(R6),X'2A'         TAKEOVER ELEMENT?                            
         BE    RDON0060            YES - CHECK IT OUT                           
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     RDON0040            GO BACK FOR NEXT                             
RDON0060 EQU   *                                                                
         USING RCONMMEL,R6                                                      
         CLC   RCONMMOR,SRCEREP    ORDER CAME FROM ORIG REP?                    
         BNE   RDON0220            NO  - PUT OUT RECORD AS-IS                   
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
*   ORDER WAS FOUND IN TAKEOVER KEY, AND IT IS FROM THE SAME REP.               
*        UPDATE ITS INFORMATION WITH VALUES FROM THE TARGET SIDE                
*        RECORD.                                                                
*                                                                               
         L     RF,TKOCOUNT         INCREMENT TAKEOVER COUNTER                   
         LA    RF,1(RF)                                                         
         ST    RF,TKOCOUNT                                                      
**>>**>>                                                                        
         MVC   SAGENCY(6),RCONKAGY INSERT AGENCY+ AGYOFF                        
         MVC   SADVERT,RCONKADV    INSERT ADVERTISER                            
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    RDON0080            NO                                           
         MVC   SPRODUCT(2),=C'C='                                               
         MVC   SPRODUCT+2(3),RCONPRD                                            
         B     RDON0140                                                         
RDON0080 EQU   *                                                                
         MVI   SCMBOFL2,C' '       CLEAR COMBO FLAG                             
         LA    RF,RCONELEM         RETRIEVE PRODUCT EXPANSION ELT               
RDON0100 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    RDON0140                                                         
         CLI   0(RF),5             EXPANSION ELEMENT?                           
         BE    RDON0120            YES                                          
         CLI   0(RF),X'17'         COMBO CONTROL ELEMENT?                       
         BE    RDON0130            YES                                          
RDON0110 EQU   *                                                                
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     RDON0100            GO BACK FOR NEXT ELEMENT                     
RDON0120 EQU   *                                                                
         MVC   SPRODUCT(20),2(RF)  INSERT PRODUCT EXPANSION                     
         B     RDON0110            GO BACK FOR X'17' ELT                        
RDON0130 EQU   *                                                                
         MVI   SCMBOFL2,C'C'       SET COMBO FLAG                               
RDON0140 EQU   *                                                                
         MVC   SCONTYPE,RCONTYPE   INSERT CONTRACT TYPE                         
         MVC   SSALPER,RCONSAL     INSERT SALESPERSON                           
         MVC   SFLITEST(6),RCONDATE                                             
*                                  INSERT FLIGHT START/END DATES                
         MVC   SNEWCON#,RCONKCON   INSERT CONTRACT NUMBER                       
         LA    R6,RCONELEM         ADD UP DOLLARS                               
         SR    RE,RE               CLEAR ACCUMULATOR                            
RDON0160 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    RDON0200            YES -                                        
         CLI   0(R6),3             ESTIMATE DOLLAR BUCKET?                      
         BNE   RDON0180            YES - ACCUMULATE                             
         ZICM  RF,6(R6),4          STRIP OUT DOLLARS                            
         AR    RE,RF                                                            
RDON0180 EQU   *                                                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         B     RDON0160            GO BACK FOR NEXT                             
RDON0200 EQU   *                                                                
         STCM  RE,15,SDOLLARN      INSERT NEW $$ INTO SORTREC                   
*                                                                               
RDON0220 EQU   *                                                                
         LA    R0,SORTREC                                                       
         PUT   RGWORK,(0)          WRITE SORTREC TO OUTPUT                      
*                                                                               
*        MVC   P+5(07),=C'WORKREC' **TEST**                                     
*        MVC   P+15(65),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
*                                                                               
         B     RDON0020            GO BACK FOR NEXT SORT REC                    
RDON0300 EQU   *                                                                
         CLOSE RGWORK                                                           
         OPEN  (RGWORK,INPUT)                                                   
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                  RESET SORT                                   
RDON0320 EQU   *                                                                
         GET   RGWORK,SORTREC      RETURN RGWORK INTO SORTREC                   
*                                  EOD ADDR = RDON0340                          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  PUT RETURNED RECORD TO SORT                  
         B     RDON0320            GO BACK FOR NEXT                             
RDON0340 EQU   *                                                                
         CLOSE RGWORK                                                           
**>>**>>                                                                        
*                                                                               
*                                                                               
RDON0400 EQU   *                                                                
*   TEST WHICH FILE                                                             
*        XC    KEY,KEY                                                          
*        MVI   KEY,1                                                            
*        GOTO1 HIGH                                                             
*        MVC   P+1(13),=C'ORIG SIDE KEY'                                        
*        MVC   P+16(27),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST WHICH FILE END                                                         
*                                                                               
         MVI   RCSUBPRG,0          SET INITIAL HEADING FOR OFFICE               
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   *+8                 NO                                           
         MVI   RCSUBPRG,2          SET INITIAL HEADING FOR AGENCY               
         CLI   QOPTION3,C'Y'       SHOW NAMES?                                  
         BNE   *+8                 NO                                           
         MVI   RCSUBPRG,5                                                       
                                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE TOP                               
         MVC   HEAD4+26(6),SRCESAVE                                             
*                                  INSERT SOURCE REP INTO HEADING               
         MVC   HEAD4+96(4),QSTATION                                             
         CLI   QSTATION+4,C' '     ANY MEDIA?                                   
         BE    RDON0420            NO  - DON'T INSERT '-'                       
         MVI   HEAD4+100,C'-'                                                   
         MVC   HEAD4+101(1),QSTATION+4                                          
*                                  INSERT MEDIA INTO STATION                    
RDON0420 EQU   *                                                                
         EDIT  CONCOUNT,(6,HEAD5+23)                                            
         EDIT  TKOCOUNT,(6,HEAD5+96)                                            
         GOTO1 DATCON,DMCB,(3,BEFFDATE),(5,HEAD6+21)                            
*                                                                               
         GOTO1 REPORT              FORCE OUT INITIAL HEADING                    
*                                                                               
         LA    RE,1                CHANGE TO SHORTENED HEADING OFFICE           
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   *+8                 NO                                           
         LA    RE,3                CHANGE TO SHORTENED HEADING AGENCY           
         CLI   QOPTION3,C'Y'       SHOW NAMES?                                  
         BNE   *+8                 NO                                           
         LA    RE,4                CHANGE TO ALTERNATE HEADINGS                 
         STC   RE,RCSUBPRG                                                      
*                                                                               
RDON0440 EQU   *                                                                
         BAS   RE,GETSORT                                                       
*                                                                               
         CLI   STYP,X'FF'          EOF                                          
         BE    RDON0800            YES                                          
*                                                                               
         CLI   QOPTION2,C'T'       TAKEOVERS ONLY ON REPORT?                    
         BNE   RDON0444            NO  - REPORT EVERYTHING                      
         OC    SNEWCON#,SNEWCON#   YES - NEW CONTRACT NUMBER ENTERED?           
         BZ    RDON0440            NO  - DON'T DISPLAY                          
RDON0444 EQU   *                                                                
         MVI   FNLTOTAL,C'N'       SET 'NO FINAL BREAK YET'                     
         BAS   RE,BREAKTST         TEST FOR BREAK(S)                            
         BAS   RE,HDRPRINT         PRINT HEADERS IF REQUIRED                    
*                                                                               
         LA    R6,P                SET A(PRINTLINE)                             
         USING PRINLIN,R6                                                       
*                                                                               
         ZICM  RF,SDOLLARO,4       ACCUMULATE FIGURES (OLD)                     
         L     RE,OFFDOLLO                                                      
         AR    RE,RF                                                            
         ST    RE,OFFDOLLO                                                      
         L     RE,AGYDOLLO                                                      
         AR    RE,RF                                                            
         ST    RE,AGYDOLLO                                                      
         L     RE,AOFDOLLO                                                      
         AR    RE,RF                                                            
         ST    RE,AOFDOLLO                                                      
         L     RE,FNLDOLLO                                                      
         AR    RE,RF                                                            
         ST    RE,FNLDOLLO                                                      
*                                                                               
         ZICM  RF,SDOLLARN,4       ACCUMULATE FIGURES (NEW)                     
         L     RE,OFFDOLLN                                                      
         AR    RE,RF                                                            
         ST    RE,OFFDOLLN                                                      
         L     RE,AGYDOLLN                                                      
         AR    RE,RF                                                            
         ST    RE,AGYDOLLN                                                      
         L     RE,AOFDOLLN                                                      
         AR    RE,RF                                                            
         ST    RE,AOFDOLLN                                                      
         L     RE,FNLDOLLN                                                      
         AR    RE,RF                                                            
         ST    RE,FNLDOLLN                                                      
*                                                                               
         CLI   QOPTION3,C'Y'       SHOW NAMES?                                  
         BNE   *+12                NO                                           
         BAS   RE,ADVPRIN                                                       
         B     RDON0472                                                         
*                                                                               
         MVC   DOFFICE,SOFFICE     INSERT OFFICE                                
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   RDON0450            NO                                           
         MVC   DOFFICE,SOFFIC2     YES - INSERT OFFIC2                          
RDON0450 EQU   *                                                                
         MVC   DAGENCY,SAGENCY     INSERT AGENCY                                
         OC    SAGYOFF,SAGYOFF     ANY AGENCY OFFICE?                           
         BZ    RDON0460            NO                                           
         MVI   DAGENCY+4,C'-'      YES - INSERT HYPHEN                          
         MVC   DAGYOFF,SAGYOFF     INSERT AGENCY OFFICE                         
RDON0460 EQU   *                                                                
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   RDON0470            NO                                           
         MVC   WORK(2),DOFFICE     YES - REARRANGE OFF/AGENCY+AOF               
         MVC   DOFFICE(7),DAGENCY  SLIDE AGENCY UP                              
         MVC   DAGENCY+2(5),SPACES CLEAR REST OF FIELD                          
         MVC   DOFFICE+9(2),WORK   REINSERT OFFICE CODE                         
*                                                                               
RDON0470 EQU   *                                                                
         MVC   DADVERT,SADVERT     INSERT ADVERTISER                            
RDON0472 EQU   *                                                                
         MVC   DPRODUCT,SPRODUCT   INSERT PRODUCT                               
         MVC   DCONTYPE,SCONTYPE   INSERT CONTRACT TYPE                         
         MVC   DSALPER,SSALPER     INSERT SALESPERSON                           
         GOTO1 DATCON,DMCB,(3,SFLITEST),(5,DFLITEST)                            
         GOTO1 DATCON,DMCB,(3,SFLITEND),(5,DFLITEND)                            
         EDIT  (B4,SDOLLARO),(11,DDOLLARO),2                                    
         EDIT  (B4,SDOLLARN),(11,DDOLLARN),2                                    
         GOTO1 HEXOUT,DMCB,SOLDCON#,WORK+32,4,=C'TOG'                           
         EDIT  (C8,WORK+32),(8,DOLDCON#)                                        
         MVC   DOLDCMBO,SCMBOFL1   SET COMBO FLAG                               
         OC    SNEWCON#,SNEWCON#                                                
         BZ    RDON0480            NO NEW CONTRACT NUMBER                       
         GOTO1 HEXOUT,DMCB,SNEWCON#,WORK+32,4,=C'TOG'                           
         EDIT  (C8,WORK+32),(8,DNEWCON#)                                        
         MVC   DNEWCMBO,SCMBOFL2   SET COMBO FLAG                               
RDON0480 EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         B     RDON0440            GO BACK FOR NEXT                             
RDON0800 EQU   *                                                                
         XC    SORTREC,SORTREC     CLEAR SORTREC FOR FINAL TOTALS               
         MVI   FNLTOTAL,C'Y'       SET 'NEED FINAL TOTAL'                       
         BAS   RE,BREAKTST         FINAL TOTALS                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   HDRPRINT:  CHECK FOR OFFICE/AGENCY OR AGENCY/OFFICE BREAKS AND              
*               PRINT HEADERS                                                   
HDRPRINT NTR1                                                                   
         CLI   QOPTION3,C'Y'       SHOW NAMES?                                  
         BNE   HDPR0800            NO - SET LAST FIELDS AND EXIT                
*                                                                               
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   HDPR0010            CODES FROM ORIG REP                          
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),NEWUTL      CODES FROM NEW REP, CHANGE FILES             
*                                                                               
HDPR0010 EQU   *                                                                
         CLI   QOPTION1,C'A'       AGENCY/OFFICE SEQUENCE?                      
         BE    HDPR0200            YES                                          
*                                  NO  - OFFICE/AGENCY                          
         CLC   LASTOFF,SOFFICE     SAME OFFICE?                                 
         BE    HDPR0020            YES - TEST AGENCY                            
*                                                                               
         GOTO1 REPORT                                                           
         LA    R1,1                R1 = INDENT FOR HEADER                       
         BAS   RE,OFFHDR           SHOW OFFICE HEADER                           
         LA    R1,5                R1 = INDENT FOR HEADER                       
         BAS   RE,AGYHDR           SHOW AGENCY HEADER                           
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,AOFHDR           SHOW AGYOFF HEADER                           
         B     HDPR0800                                                         
HDPR0020 EQU   *                                                                
         CLC   LASTAGY(4),SAGENCY  SAME AGENCY?                                 
         BE    HDPR0040            YES - TEST AGYOFF                            
         GOTO1 REPORT                                                           
         LA    R1,5                R1 = INDENT FOR HEADER                       
         BAS   RE,AGYHDR           NO  - SHOW AGENCY HEADER                     
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,AOFHDR           NO  - SHOW AGYOFF HEADER                     
         B     HDPR0800                                                         
HDPR0040 EQU   *                                                                
         CLC   LASTAGY+4(2),SAGYOFF                                             
*                                  AGENCY/OFFICE BREAK?                         
         BE    HDPR0800            NO  - NO BREAKS                              
         GOTO1 REPORT                                                           
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,AOFHDR           YES - SHOW AGYOFF HEADER                     
         B     HDPR0800                                                         
HDPR0200 EQU   *                                                                
         CLC   LASTAGY(4),SAGENCY  SAME AGENCY?                                 
         BE    HDPR0220            YES - TEST AGENCY                            
         GOTO1 REPORT                                                           
         LA    R1,1                R1 = INDENT FOR HEADER                       
         BAS   RE,AGYHDR           NO  - SHOW AGENCY HEADER                     
         LA    R1,5                R1 = INDENT FOR HEADER                       
         BAS   RE,AOFHDR           NO  - SHOW AGYOFF HEADER                     
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,OFFHDR           NO  - SHOW OFFICE HEADER                     
         B     HDPR0800                                                         
HDPR0220 EQU   *                                                                
         CLC   LASTAGY+4(2),SAGYOFF                                             
*                                  SAME AGENCY/OFFICE?                          
         BE    HDPR0240            YES - TEST AGYOFF                            
         GOTO1 REPORT                                                           
         LA    R1,5                R1 = INDENT FOR HEADER                       
         BAS   RE,AOFHDR           NO  - SHOW AGYOFF HEADER                     
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,OFFHDR           NO  - SHOW OFFICE HEADER                     
         B     HDPR0800                                                         
HDPR0240 EQU   *                                                                
         CLC   LASTOFF(2),SOFFIC2                                               
*                                  OFFICE BREAK?                                
         BE    HDPR0800            NO  - NO BREAKS                              
         GOTO1 REPORT                                                           
         LA    R1,9                R1 = INDENT FOR HEADER                       
         BAS   RE,OFFHDR           YES - SHOW OFFICE HEADER                     
         B     HDPR0800                                                         
HDPR0800 EQU   *                                                                
         MVC   LASTAGY(6),SAGENCY  INSERT LAST CODE                             
         MVC   LASTOFF,SOFFICE                                                  
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   HDPR0840            NO                                           
         MVC   LASTOFF,SOFFIC2     YES                                          
HDPR0840 EQU   *                                                                
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   EXIT                CODES FROM ORIG REP                          
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),SAVEVUTL     RESTORE ORGI UTL                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
AGYHDR   NTR1                                                                   
         LA    R6,P(R1)                                                         
         MVC   0(7,R6),=C'Agency:'                                              
         LA    R6,8(R6)                                                         
*                                                                               
         MVC   0(4,R6),SAGENCY                                                  
*                                                                               
         CLC   SAGENCY,LASTAGY                                                  
         BE    AGYHDX                                                           
*                                                                               
         AHI   R6,1                                                             
         CLI   0(R6),C' '                                                       
         BH    *-8                                                              
*                                                                               
         MVI   0(R6),C'('                                                       
*                                                                               
         MVC   1(07,R6),=C'UNKNOWN'                                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,SAGENCY                                               
         MVC   K.RAGYKAOF,SAGYOFF                                               
*                                                                               
         MVC   K.RAGYKREP,RCREPFL                                               
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   *+10                                                             
         MVC   K.RAGYKREP,SRCEREP                                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AGYHD05                                                          
*                                                                               
AGYHD02  DS    0H                                                               
         GOTO1 GREC                                                             
         L     R4,AIOAREA                                                       
         USING RAGYREC,R4                                                       
         MVC   AGYNAME,RAGYNAM1                                                 
         DROP  R4                                                               
*                                                                               
         MVC   1(L'AGYNAME,R6),AGYNAME                                          
*                                                                               
AGYHD05  DS    0H                                                               
         LA    R6,21(R6)                                                        
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BNH   *-6                                                              
*                                                                               
         MVI   1(R6),C')'                                                       
AGYHDX   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
AOFHDR   NTR1                                                                   
         LA    R6,P(R1)                                                         
         MVC   0(14,R6),=C'Agency Office:'                                      
         LA    R6,15(R6)                                                        
*                                                                               
         MVC   0(2,R6),SAGYOFF                                                  
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
OFFHDR   NTR1                                                                   
         LA    R6,P(R1)                                                         
         MVC   0(7,R6),=C'Office:'                                              
         LA    R6,8(R6)                                                         
*                                                                               
         MVC   0(2,R6),SOFFICE                                                  
*                                                                               
         CLC   LASTOFF,SOFFICE                                                  
         BE    OFFHDX                                                           
*                                                                               
         AHI   R6,1                                                             
         CLI   0(R6),C' '                                                       
         BH    *-8                                                              
*                                                                               
         MVI   0(R6),C'('                                                       
*                                                                               
         MVC   1(07,R6),=C'UNKNOWN'                                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING ROFFKEY,KEY                                                      
         MVI   K.ROFFKTYP,X'04'                                                 
         MVC   K.ROFFKOFF,SOFFICE                                               
         MVC   K.ROFFKREP,RCREPFL                                               
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   *+10                                                             
         MVC   K.ROFFKREP,SRCEREP                                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFFHD05                                                          
*                                                                               
OFFHD02  DS    0H                                                               
         GOTO1 GREC                                                             
         L     R4,AIOAREA                                                       
         USING ROFFREC,R4                                                       
         MVC   OFFNAME,ROFFNAME                                                 
         DROP  R4                                                               
*                                                                               
         MVC   1(L'OFFNAME,R6),OFFNAME                                          
*                                                                               
OFFHD05  DS    0H                                                               
         LA    R6,21(R6)                                                        
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BNH   *-6                                                              
*                                                                               
         MVI   1(R6),C')'                                                       
*                                                                               
OFFHDX   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
ADVPRIN  NTR1                                                                   
         LA    R6,P+1                                                           
         MVC   0(4,R6),SADVERT                                                  
*                                                                               
         CLC   LASTADV,SADVERT                                                  
         BE    ADVPRX                                                           
*                                                                               
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   ADVPR01             CODES FROM ORIG REP                          
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),NEWUTL      CODES FROM NEW REP, CHANGE FILES             
*                                                                               
ADVPR01  DS    0H                                                               
         AHI   R6,1                                                             
         CLI   0(R6),C' '                                                       
         BH    *-8                                                              
*                                                                               
         MVI   0(R6),C'('                                                       
*                                                                               
         MVC   1(07,R6),=C'UNKNOWN'                                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKADV,SADVERT                                               
         MVC   K.RADVKREP,RCREPFL                                               
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   *+10                                                             
         MVC   K.RADVKREP,SRCEREP                                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ADVPR05                                                          
*                                                                               
ADVPR02  DS    0H                                                               
         GOTO1 GREC                                                             
         L     R4,AIOAREA                                                       
         USING RADVREC,R4                                                       
         MVC   ADVNAME,RADVNAME                                                 
         DROP  R4                                                               
*                                                                               
         MVC   1(16,R6),ADVNAME                                                 
*                                                                               
ADVPR05  DS    0H                                                               
         LA    R6,18(R6)                                                        
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BNH   *-6                                                              
*                                                                               
         MVI   1(R6),C')'                                                       
*                                                                               
ADVPRX   DS    0H                                                               
         MVC   LASTADV,SADVERT                                                  
         OC    SNEWCON#,SNEWCON#                                                
         BNZ   EXIT                CODES FROM ORIG REP                          
*                                                                               
         L     R1,ADCONLST                                                      
         L     RE,VUTL-ADCONSD(R1)                                              
         MVC   4(1,RE),SAVEVUTL     RESTORE ORGI UTL                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   BREAKTST:  CHECK FOR OFFICE/AGENCY OR AGENCY/OFFICE BREAKS.                 
*                                                                               
BREAKTST NTR1                                                                   
         LA    R6,P                SET ADDRESSABILITY TO PRINTLINE              
         USING PRINLIN,R6                                                       
*                                                                               
         OC    LASTAGY,LASTAGY     FIRST PASS?                                  
         BZ    BTST0800            YES                                          
         CLI   QOPTION1,C'A'       AGENCY/OFFICE SEQUENCE?                      
         BE    BTST0200            YES                                          
*                                  NO  - OFFICE/AGENCY                          
         CLC   LASTOFF,SOFFICE     SAME OFFICE?                                 
         BE    BTST0020            YES - TEST AGENCY                            
         GOTO1 REPORT                                                           
         BAS   RE,AOFBREAK         NO  - SHOW AGYOFF BREAK                      
         BAS   RE,AGYBREAK         NO  - SHOW AGENCY BREAK                      
         BAS   RE,OFFBREAK         SHOW OFFICE BREAK                            
         B     BTST0800                                                         
BTST0020 EQU   *                                                                
         CLC   LASTAGY(4),SAGENCY  SAME AGENCY?                                 
         BE    BTST0040            YES - TEST AGYOFF                            
         GOTO1 REPORT                                                           
         BAS   RE,AOFBREAK         NO  - SHOW AGYOFF BREAK                      
         BAS   RE,AGYBREAK         NO  - SHOW AGENCY BREAK                      
         B     BTST0800                                                         
BTST0040 EQU   *                                                                
         CLC   LASTAGY+4(2),SAGYOFF                                             
*                                  AGENCY/OFFICE BREAK?                         
         BE    BTST0800            NO  - NO BREAKS                              
         GOTO1 REPORT                                                           
         BAS   RE,AOFBREAK         YES - SHOW AGYOFF BREAK                      
         B     BTST0800                                                         
BTST0200 EQU   *                                                                
         CLC   LASTAGY(4),SAGENCY  SAME AGENCY?                                 
         BE    BTST0220            YES - TEST AGENCY                            
         GOTO1 REPORT                                                           
         BAS   RE,OFFBREAK         NO  - SHOW OFFICE BREAK                      
         BAS   RE,AOFBREAK         NO  - SHOW AGYOFF BREAK                      
         BAS   RE,AGYBREAK         NO  - SHOW AGENCY BREAK                      
         B     BTST0800                                                         
BTST0220 EQU   *                                                                
         CLC   LASTAGY+4(2),SAGYOFF                                             
*                                  SAME AGENCY/OFFICE?                          
         BE    BTST0240            YES - TEST AGYOFF                            
         GOTO1 REPORT                                                           
         BAS   RE,OFFBREAK         NO  - SHOW OFFICE BREAK                      
         BAS   RE,AOFBREAK         NO  - SHOW AGYOFF BREAK                      
         B     BTST0800                                                         
BTST0240 EQU   *                                                                
         CLC   LASTOFF(2),SOFFIC2                                               
*                                  OFFICE BREAK?                                
         BE    BTST0800            NO  - NO BREAKS                              
         GOTO1 REPORT                                                           
         BAS   RE,OFFBREAK         YES - SHOW OFFICE BREAK                      
         B     BTST0800                                                         
BTST0800 EQU   *                                                                
         CLI   FNLTOTAL,C'Y'       FINAL TOTALS NEEDED?                         
         BNE   BTST0820            NO                                           
         BAS   RE,FNLBREAK         YES                                          
BTST0820 EQU   *                                                                
**                                                                              
** THE LAST CODES ARE NOW SET IN HDRPRINT                                       
**                                                                              
**       MVC   LASTAGY(6),SAGENCY  INSERT LAST CODE                             
**       MVC   LASTOFF,SOFFICE                                                  
**       CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
**       BNE   BTST0840            NO                                           
**       MVC   LASTOFF,SOFFIC2     YES                                          
BTST0840 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   OFFBREAK                                                                    
*                                                                               
OFFBREAK NTR1                                                                   
         MVC   P+46(13),=C'OFFICE TOTAL:'                                       
         EDIT  OFFDOLLO,(11,DDOLLARO),2                                         
         XC    OFFDOLLO,OFFDOLLO   CLEAR OFFICE DOLLARS                         
         EDIT  OFFDOLLN,(11,DDOLLARN),2                                         
         XC    OFFDOLLN,OFFDOLLN   CLEAR OFFICE DOLLARS                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
AGYBREAK NTR1                                                                   
         MVC   P+46(13),=C'AGENCY TOTAL:'                                       
         EDIT  AGYDOLLO,(11,DDOLLARO),2                                         
         XC    AGYDOLLO,AGYDOLLO   CLEAR AGENCY DOLLARS                         
         EDIT  AGYDOLLN,(11,DDOLLARN),2                                         
         XC    AGYDOLLN,AGYDOLLN   CLEAR AGENCY DOLLARS                         
         GOTO1 REPORT                                                           
***      MVC   P+46(13),=C'TEST MESSAGE:'                                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
AOFBREAK NTR1                                                                   
         MVC   P+39(20),=C'AGENCY OFFICE TOTAL:'                                
         EDIT  AOFDOLLO,(11,DDOLLARO),2                                         
         XC    AOFDOLLO,AOFDOLLO   CLEAR OFFICE DOLLARS                         
         EDIT  AOFDOLLN,(11,DDOLLARN),2                                         
         XC    AOFDOLLN,AOFDOLLN   CLEAR OFFICE DOLLARS                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
FNLBREAK NTR1                                                                   
         MVC   P+39(20),=C'       GRAND  TOTAL:'                                
         EDIT  FNLDOLLO,(11,DDOLLARO),2                                         
         XC    FNLDOLLO,FNLDOLLO   CLEAR OFFICE DOLLARS                         
         EDIT  FNLDOLLN,(11,DDOLLARN),2                                         
         XC    FNLDOLLN,FNLDOLLN   CLEAR OFFICE DOLLARS                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1                                                                   
         MVI   SORTREC,0                                                        
         MVC   SOFFICE,RCONKOFF    INSERT OFFICE                                
         CLI   QOPTION1,C'A'       AGENCY SEQUENCE REPORT?                      
         BNE   SGEN0010            NO                                           
         MVC   SOFFIC2,SOFFICE     MOVE OFFICE TO MINOR KEY                     
         XC    SOFFICE,SOFFICE     CLEAR OFFICE                                 
SGEN0010 EQU   *                                                                
         MVC   SAGENCY(6),RCONKAGY INSERT AGENCY+ AGYOFF                        
         MVC   SADVERT,RCONKADV    INSERT ADVERTISER                            
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    SGEN0020            NO                                           
         MVC   SPRODUCT(2),=C'C='                                               
         MVC   SPRODUCT+2(3),RCONPRD                                            
         B     SGEN0060                                                         
SGEN0020 EQU   *                                                                
         LA    RF,RCONELEM         RETRIEVE PRODUCT EXPANSION ELT               
SGEN0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),5             EXPANSION ELEMENT?                           
         BE    SGEN0050            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     SGEN0040            GO BACK FOR NEXT ELEMENT                     
SGEN0050 EQU   *                                                                
         MVC   SPRODUCT(20),2(RF)  INSERT PRODUCT EXPANSION                     
SGEN0060 EQU   *                                                                
         MVC   SCONTYPE,RCONTYPE   INSERT CONTRACT TYPE                         
         MVC   SSALPER,RCONSAL     INSERT SALESPERSON                           
         MVC   SFLITEST(6),RCONDATE                                             
*                                  INSERT FLIGHT START/END DATES                
         MVC   SOLDCON#,RCONKCON   INSERT CONTRACT NUMBER                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*        MVC   P+5(07),=C'SORTGEN' **TEST**                                     
*        MVC   P+15(65),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
         B     EXIT                                                             
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1                                                                   
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GESO0040            YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GESO0040            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(65),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GESO0040 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
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
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=65'                                    
         SPACE 2                                                                
*                                                                               
RGWORK   DCB   DDNAME=RGWORK,DSORG=PS,EODAD=RDON0340,LRECL=SRTRECLN,   X        
               BLKSIZE=16*SRTRECLN,MACRF=(GM,PM),RECFM=FB                       
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
**PAN#1  DC    CL21'085REREPTK02S05/01/02'                                      
         END                                                                    
