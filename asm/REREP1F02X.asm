*          DATA SET REREP1F02X AT LEVEL 115 AS OF 06/25/02                      
*PHASE RE1F02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
         TITLE 'REREP1F02 - ADVERTISER CATEGORY FIXER'                          
*********************************************************************           
*                                                                   *           
*        REREP1F02 --- ADVERTISER CATEGORY FIXER                    *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*   NOTE:  THIS PROGRAM MUST BE RUN IN THE WEEKEND RUN IN ONLY ONE  *           
*          PLACE:  AFTER THE LAST LOAD IS DONE!                     *           
*      1.  THE SWITCH PROGRAM MAY CHANGE ADVERTISERS, WHICH WILL    *           
*          RESULT IN CHANGED PRODUCT CODES.                         *           
*      2.  THE LOAD AFTER THE SWITCH WILL GENERATE NEW RIS KEYS.    *           
*      3.  THIS JOB WILL REALIGN CATEGORY CODES WITHIN CONTRACTS    *           
*          WITH THE CATEGORY CODES WITHIN THE ADVERTISER & PRODUCT  *           
*          RECORDS.                                                 *           
*      4.  THE RIS KEYS WERE BUILT BY THE LOAD OF THE FINAL SWITCH. *           
*      5.  KEYTYPE 2 CONTAINS THE CATEGORY CODE.                    *           
*      6.  MODIFICATIONS TO THIS PROGRAM ADJUST THE KEYTYPE 2 RIS   *           
*          KEY TO AGREE WITH THE NEW CATEGORY CODE ASSIGNED TO THE  *           
*          CONTRACT.                                                *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* AUG31/92 (BU ) --- INITIAL ENTRY - CLONED FROM ORIGINAL RE1B02    *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* DEC30/96 (BU ) --- ADD PRODUCT CATEGORY TO CONSIDERATION          *           
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* AUG31/99 (BU ) --- ENHANCE DISPLAY OUTPUT                         *           
*                                                                   *           
* SEP03/99 (BU ) --- REPLACE IF RADVCTG = SPACE AND TAKEOVER        *           
*                                                                   *           
* 15FEB01  (BU ) --- KATZ RADIO:  CLEAR CHANNEL SPLITOFF            *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   QUESTOR  =  PRINTSR   -   DISPLAY SORT RECORDS + VARIOUS        *           
*                                DATA COMPONENTS                    *           
*               PRINT     -   DISPLAYS VARIOUS DATA COMPONENTS      *           
*   QUESTOR+7=  #         -   PROCESSES FIRST N CONTRACTS ONLY      *           
*                                                                   *           
*   QUESTOR+8=  Y         -   PRINT 'CONPOST', ELSE SKIP            *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R9  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1F02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1F02,R7,R9,RR=RE                                           
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
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
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
*                                                                               
*   ALL PROCESSING IN THIS PROGRAM IS DONE IN THE INITIAL PASS                  
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
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BNE   INIT0005            NO                                           
         MVC   P+1(25),=C'TEST/SOFT RUN:  NO UPDATE'                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   PREC(2),=X'0000'    YES - PHYSICALLY PREVENT REWRITE             
INIT0005 EQU   *                                                                
         XC    PROCCTR,PROCCTR     INITIALIZE CONTRACT COUNTER                  
         XC    PRODCTR2,PRODCTR2   INITIALIZE CONTRACT COUNTER                  
         MVC   PX,SPACES           INITIALIZE ALTERNATE PRINT LINE              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE THE REP RECORD                      
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                                                             
         GOTO1 GETREP              RETRIEVE THE REP RECORD                      
         MVC   ALTREPFL,RCREPFL                                                 
*                                                                               
*   CLEAR CHANNEL/KATZ IS SATELLITED UNDER THE KRG MASTER FILE.                 
*        THIS RUN MUST BE FORCED TO THAT POWER CODE, TO CONSIDER                
*        CCR AS A SUBSIDIARY FOR PRODUCT CATEGORY UPDATING.                     
*                                                                               
         CLC   =C'NU',RCREPFL      CLEAR CHANNEL/KATZ RADIO?                    
         BNE   INIT0010            NO                                           
         MVC   RREPMAST,=C'K3'     YES - FORCE KRG POWER CODE                   
INIT0010 EQU   *                                                                
         CLC   =X'FFFF',RREPMAST   IS THIS A MASTER?                            
         BE    INIT0020            YES - USE RCREPFL VALUE                      
         OC    RREPMAST,RREPMAST   IS THIS A NON-MASTER?                        
         BZ    INIT0020            YES - USE RCREPFL VALUE                      
         CLC   =C'  ',RREPMAST     IS THIS A NON-MASTER?                        
         BE    INIT0020            YES - USE RCREPFL VALUE                      
         MVC   ALTREPFL,RREPMAST   NO  - USE THE MASTER REP                     
INIT0020 EQU   *                                                                
         LA    RF,RREPELEM         SET A(01 ELEMENT)                            
INIT0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    INIT0120            YES - NO PROFILE ELEMENT                     
         CLI   0(RF),4             PROFILE ELEMENT FOUND?                       
         BE    INIT0060            YES - LOOK FOR PROGRAM UNIT SET              
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     INIT0040                                                         
INIT0060 EQU   *                                                                
         ZIC   RE,2(RF)            SET # OF 10-BYTE PROGRAM UNITS               
         LA    RF,4(RF)            BUMP TO 1ST PROGRAM UNIT                     
INIT0080 EQU   *                                                                
         CLI   0(RF),RREPQLFM      PROGRAM UNIT = FILE PROG?                    
         BE    INIT0100            YES                                          
         LA    RF,10(RF)           NO  - BUMP TO NEXT UNIT                      
         BCT   RE,INIT0080         GO BACK FOR NEXT                             
         B     INIT0120            NO ELEMENT FOUND -                           
INIT0100 EQU   *                                                                
         LA    RF,2(RF)            POINT TO 1ST PROFILE BYTE                    
*                                     DIDN'T HAVE TO BE DONE:                   
*                                        NEATER, THO'                           
         TM    1(RF),X'08'         PRODUCT CATEGORY = ADVERTISER?               
         BO    INIT0120            YES - ORIGINAL FIXER PATH                    
         MVI   PRODCATG,1          NO  - PRODUCT CATEGORY MAY                   
*                                     BE DIFFERENT                              
         MVC   P+1(39),=C'PRODUCT CATEGORY CODES TO HAVE PRIORITY'              
         GOTO1 REPORT                                                           
         OPEN  (CATWORK,OUTPUT)    OPEN SCRATCH FILE OUTPUT                     
INIT0120 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           CYCLE THROUGH CONTRACT RECORDS               
         MVC   KEY+2(2),RCREPFL    INSERT REP CODE                              
*                                                                               
*   TEST FOR START:  TO START AT A SPECIFIC CONTRACT                            
****>>>> MVC   KEY+4(19),=C'R WLRSFDELINTDECHDR'                                
*   TEST FOR START                                                              
         GOTO1 HIGH                                                             
*        SR    R2,R2                                                            
INIT0140 EQU   *                                                                
         CLI   KEY,X'0C'           STILL CONTRACT?                              
         BNE   INIT0160            NO  - DONE EXTRACTING                        
         CLC   KEY+2(2),RCREPFL    SAME REP?                                    
         BNE   INIT0160            NO  - DONE EXTRACTING                        
         BAS   RE,OUTSORT          PUT OUT SORT RECORD                          
         GOTO1 SEQ                 READ NEXT KEY IN SEQUENCE                    
         CLI   QUESTOR+7,C'#'      COUNTER IN EFFECT?                           
         BNE   INIT0140            NO  - PROCESS ALL                            
         LA    R2,1(R2)                                                         
         C     R2,=F'100'          COUNTER REACHED?                             
         BE    INIT0160            YES                                          
         B     INIT0140            GO BACK TO PROCESS NEXT                      
INIT0160 EQU   *                                                                
         XC    KEYSAV2,KEYSAV2     RESET ADVERTISER KEY                         
         XC    KEY,KEY             RESET CONTRACT   KEY                         
         BAS   RE,POST             PROCESS CONTRACTS                            
         OC    PRODCTR,PRODCTR     ANY PRODUCT RECORDS?                         
         BZ    INIT0200            NO  -                                        
         BAS   RE,PRODPOST         YES - UPDATE PRODUCT RECORDS                 
INIT0200 EQU   *                                                                
         MVC   P+1(30),=C'** CATEGORY UPDATE COMPLETE **'                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'  CONTRACTS W/ADV CATS CHANGED:'                      
         EDIT  PROCCTR,(9,P+34)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'  CONTRACTS W/PRD CATS CHANGED:'                      
         EDIT  PRODCTR2,(9,P+34)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'  CONTRACTS W/PROD CATEGORIES :'                      
         EDIT  PRODCTR,(9,P+34)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'**                          **'                       
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   PUT OUT SORT RECORD FOR EACH CONTRACT'S ADVERTISER AND ITS                  
*     DISK ADDRESS                                                              
*                                                                               
OUTSORT  NTR1                                                                   
         XC    SORTREC,SORTREC                                                  
         MVC   SADV,KEY+19         INSERT ADVERTISER CODE                       
         MVC   SDSKADDR,KEY+28     INSERT DISK ADDRESS                          
         MVC   SCONNUM,KEY+23      INSERT CONTRACT NUMBER                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                                                               
         L     RF,SORTCTR          INCREMENT SORT COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
         CLC   =C'PRINTSR',QUESTOR                                              
         BNE   OUTS0040                                                         
         MVC   P+00(12),=C'SORT REC =  '                                        
         MVC   P+12(17),SORTREC                                                 
         EDIT  SORTCTR,(9,P+32)                                                 
         GOTO1 REPORT                                                           
*                                                                               
OUTS0040 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
         MVC   P+1(04),=C'POST'                                                 
         EDIT  SORTCTR,(9,P+10)                                                 
         GOTO1 REPORT                                                           
         XC    SORTCTR,SORTCTR     CLEAR SORT COUNTER                           
*                                                                               
POST0020 EQU   *                                                                
         BAS   R8,GETSORT          RETRIEVE SORT RECORD                         
         CLI   SADV,X'FF'          EOF REACHED?                                 
         BE    POST0240            YES - FINISHED                               
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
         SR    RE,RE               CLEAR FOR DIVIDE                             
         D     RE,=F'1000'                                                      
         LTR   RE,RE               ANY REMAINDER?                               
         BNZ   POST0040            YES - NOT EVEN 1000                          
         MVC   P+1(14),=C'PROCESSED/POST'                                       
         EDIT  SORTCTR,(9,P+20)                                                 
         GOTO1 REPORT                                                           
POST0040 EQU   *                                                                
         CLC   SADV,RADVKADV       SAME ADVERTISER?                             
         BE    POST0100            YES - PROCESS CONTRACT                       
         BL    POST0060            CONTRACT ADV CODE MISSED                     
         BAS   RE,NEWADVER         GET NEXT ADVERTISER                          
         B     POST0040            CHECK OUT NEXT ADVERTISER                    
POST0060 EQU   *                                                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   POST0080                                                         
         MVC   PX+5(18),=C'MISSED =      /   '                                  
         MVC   PX+15(04),SADV                                                   
         GOTO1 HEXOUT,DMCB,SDSKADDR,PX+24,4,=C'TOG'                             
         MVC   PX+35(09),=C'RADVKADV='                                          
         MVC   PX+45(4),RADVKADV                                                
         MVC   P(80),PX                                                         
         MVC   PX(80),SPACES                                                    
         GOTO1 REPORT                                                           
POST0080 EQU   *                                                                
         B     POST0020            GO BACK FOR NEXT SORT                        
POST0100 EQU   *                                                                
         XC    KEY,KEY             NOW RETURN THE CONTRACT RECORD               
         MVC   KEY+28(4),SDSKADDR  SET D/A OF CONTRACT RECORD                   
         BAS   RE,GETCON           READ CON REC VIA DISK ADDR                   
         LA    RF,RCONREC          SET A(RCONREC)                               
         ZICM  RE,RCONLEN,2        GET CONTRACT LENGTH                          
         AR    RF,RE                                                            
         XC    0(2,RF),0(RF)       INSERT TWO BYTES ZERO                        
*                                     AT CONTRACT END                           
*                                                                               
*  SPECIAL TEST                                                                 
*                                                                               
         CLC   =C'M090',RCONKADV                                                
         BE    TEST0020                                                         
*                                                                               
         CLC   =C'PRINT',QUESTOR                                                
         BNE   POST0120                                                         
         CLI   QUESTOR+8,C'Y'      PRINT CONPOST OUTPUTS?                       
         BNE   POST0120            NO                                           
TEST0020 EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P+5(18),=C'CONPOST=      /   '                                   
         MVC   P+15(04),RCONKADV                                                
         MVC   P+20(2),RCONCTGY                                                 
         MVC   P+24(3),RCONPRD                                                  
         OC    P+24(3),SPACES                                                   
         MVC   P+38(09),=C'ADVCTG = '                                           
         MVC   P+48(02),RADVCATG                                                
         MVC   P+52(09),=C'PRODCATG='                                           
         MVC   P+61(1),PRODCATG                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+27,4,=C'TOG'                              
         GOTO1 REPORT                                                           
POST0120 EQU   *                                                                
         CLI   PRODCATG,0          CHECK FOR PRODUCT CODE?                      
         BE    POST0160            NO  - NO CATEGORY PRIORITY                   
         CLC   RCONPRD,=C'   '     ANY PRODUCT CODE?                            
         BNH   POST0160            NO  -                                        
         MVC   SPROD,RCONPRD       YES - UPGRADE SORT RECORD WITH               
         MVC   SCAT,RCONCTGY          PRODUCT AND EXISTING CATEGORY             
         LA    R0,SORTREC                                                       
         PUT   CATWORK,(R0)        WRITE RECORD TO SCRATCH                      
*                                                                               
         CLC   =C'PRINT',QUESTOR                                                
         BNE   POST0140                                                         
         MVC   P+01(08),=C'SCRATCH='                                            
         MVC   P+12(09),SADV                                                    
         GOTO1 HEXOUT,DMCB,SCONNUM,P+24,4,=C'TOG'                               
         GOTO1 REPORT                                                           
POST0140 EQU   *                                                                
         L     RF,PRODCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PRODCTR                                                       
         B     POST0020            GO BACK FOR NEXT                             
POST0160 EQU   *                                                                
         BAS   RE,CHKTKO           TAKEOVER ORDER?                              
         BNZ   POST0170            YES - NO SPACE/BIN ZERO TESTS                
         CLC   =C'  ',RADVCATG     CATEGORY CODE = SPACE?                       
         BE    POST0220            YES - NO UPDATE                              
         CLC   =X'0000',RADVCATG   CATEGORY CODE = BINARY ZERO?                 
         BE    POST0220            YES - NO UPDATE                              
POST0170 EQU   *                                                                
*                                                                               
*  SPECIAL TEST                                                                 
*                                                                               
         CLC   =C'M090',RCONKADV                                                
         BNE   TEST0040                                                         
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P+5(18),=C'ADV:CTGY=     /   '                                   
         MVC   P+15(04),RCONKADV                                                
         MVC   P+20(2),RCONCTGY                                                 
         MVC   P+24(3),RCONPRD                                                  
         OC    P+24(3),SPACES                                                   
         MVC   P+38(09),=C'ADVCTG = '                                           
         MVC   P+48(02),RADVCATG                                                
         MVC   P+52(09),=C'PRODCATG='                                           
         MVC   P+61(1),PRODCATG                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+27,4,=C'TOG'                              
         GOTO1 REPORT                                                           
TEST0040 EQU   *                                                                
         CLC   RADVCATG,RCONCTGY   CATEGORY CODES SAME?                         
         BE    POST0220            YES - NO UPDATE                              
         MVC   OLDCTGY,RCONCTGY    SAVE OLD CATEGORY                            
         MVC   RCONCTGY,RADVCATG   NO  - UPDATE IT                              
*                                                                               
*  SPECIAL TEST                                                                 
*                                                                               
         CLC   =C'M090',RCONKADV                                                
         BNE   TEST0060                                                         
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P+5(18),=C'REWRITE:=     /   '                                   
         MVC   P+15(04),RCONKADV                                                
         MVC   P+20(2),RCONCTGY                                                 
         MVC   P+24(3),RCONPRD                                                  
         OC    P+24(3),SPACES                                                   
         MVC   P+38(09),=C'ADVCTG = '                                           
         MVC   P+48(02),RADVCATG                                                
         MVC   P+52(09),=C'PRODCATG='                                           
         MVC   P+61(1),PRODCATG                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+27,4,=C'TOG'                              
         GOTO1 REPORT                                                           
TEST0060 EQU   *                                                                
         BAS   RE,PUTCON           REWRITE RECORD                               
         L     RF,PROCCTR          INCREMENT CHANGED REC COUNTER                
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
POST0180 EQU   *                                                                
*                                                                               
*  SPECIAL TEST                                                                 
*                                                                               
         CLC   =C'M090',RCONKADV                                                
         BE    TEST0080                                                         
         CLC   =C'PRINT',QUESTOR                                                
         BNE   POST0220                                                         
TEST0080 EQU   *                                                                
         MVC   PX,SPACES           CLEAR PRINT LINE                             
         MVC   PX+05(18),=C'UPDTD  =  ****/** '                                 
         MVC   PX+15(04),RCONKADV                                               
         MVC   PX+20(2),RCONCTGY                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,PX+27,4,=C'TOG'                             
         EDIT  PROCCTR,(5,PX+45)                                                
**       CLI   PRNTFLAG,C'Y'       PRINT FLAG SET?                              
**       BE    POST0200            YES                                          
         B     POST0200                                                         
*                                                                               
*   PRINT INTERVAL COUNTER                                                      
*                                                                               
         LH    R1,=H'1000'         PRINT EVERY NTH RECORD                       
         SR    RE,RE               INITIALIZE FIRST OF PAIR                     
         L     RF,PROCCTR          CURRENT CONTRACT COUNT                       
         DR    RE,R1               DIVIDE COUNT BY INCREMENT                    
         LTR   RE,RE               TEST FOR ZERO REMAINDER                      
         BNZ   POST0220            NOT ZERO - DON'T PRINT                       
POST0200 EQU   *                                                                
         MVI   PRNTFLAG,C'N'       TURN PRINT FLAG OFF                          
         MVC   P(80),PX            MOVE SECOND PRINTLINE TO FIRST               
         GOTO1 REPORT              PRINT THE ENTRY                              
POST0220 EQU   *                                                                
         MVC   PX(80),SPACES                                                    
         B     POST0020            GO BACK FOR NEXT CONTRACT                    
POST0240 EQU   *                                                                
         CLI   PRODCATG,0          CHECK FOR PRODUCT CODE?                      
         BE    POST0280            NO  - NO CATEGORY PRIORITY                   
         CLOSE CATWORK                                                          
POST0280 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKTKO:  CHECK CONTRACT FOR PRESENCE OF X'2A' TAKEOVER ELEMENT.             
*        IF PRESENT, ORDER IS A TAKEOVER, AND EXISTING CATEGORY CODE            
*        OF ORDER IS TO BE REPLACED BY THAT IN ADVERTISER RECORD                
*        EVEN IF ADVERTISER CATEGORY CODE IS EMPTY.                             
*                                                                               
CHKTKO   NTR1                                                                   
         LA    R6,RCONELEM                                                      
CHKT0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    CHKT0100            YES - EXIT CC = ZERO                         
*                                     NO X'2A'                                  
         CLI   0(R6),X'2A'         TAKEOVER ELEMENT?                            
         BE    CHKT0120            YES - EXIT CC NOT ZERO                       
         BH    CHKT0100            NO  - HIGH: FINISHED                         
         ZIC   RE,1(R6)            NO  - LOW:                                   
         AR    R6,RE                                                            
         B     CHKT0020            GO BACK FOR NEXT                             
CHKT0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CHKT0140                                                         
CHKT0120 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CHKT0140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   NEWADVER: RETRIEVES NEXT ADVERTISER RECORD IN SEQUENCE.  BECAUSE            
*      THE CONTRACT RECORD IS READ VIA DISK ADDRESS AFTER - REPEAT,             
*      AFTER - THE ADVERTISER RECORD, THE CONTRACT RECORD IS                    
*      AVAILABLE FOR RE-WRITE WITH NO REPOSITIONING OF THE FILE.                
*                                                                               
NEWADVER NTR1                                                                   
         OC    KEYSAV2,KEYSAV2     1ST TIME?                                    
         BNZ   NADV0010            NO  - USE KEY                                
         MVI   KEYSAV2,X'08'       YES - SET 1ST KEY                            
         MVC   KEY,KEYSAV2         LOAD THE NEW KEY                             
         GOTO1 HIGH                START THE CHAIN                              
         B     NADV0030                                                         
NADV0010 EQU   *                                                                
         MVC   KEY,KEYSAV2         SET KEY FOR NEXT ADVERTISER RECORD           
NADV0020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT SEQUENTIAL                         
NADV0030 EQU   *                                                                
         CLI   KEY,X'08'           ADVERTISER KEY?                              
         BE    NADV0035            YES                                          
         MVC   RADVKADV,=X'FFFFFFFF' NO  - SET TO HIGH VALUE                    
         B     MODEEXIT            RETURN                                       
NADV0035 EQU   *                                                                
         CLC   ALTREPFL,KEY+25     CORRECT REP?                                 
         BNE   NADV0020            NO  - GET NEXT KEY                           
         BAS   RE,GETADV                                                        
         MVC   KEYSAV2,RADVREC     SAVE ADV KEY FOR SEQ READ                    
*                                                                               
*  SPECIAL TEST                                                                 
*                                                                               
         CLC   =C'M090',RCONKADV                                                
         BE    TEST0100                                                         
         CLC   =C'PRINT',QUESTOR                                                
         BNE   NADV0040                                                         
TEST0100 EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P+2(18),=C'ADVERT =      /   '                                   
         MVC   P+11(04),RADVKADV                                                
         MVC   P+16(2),RADVCATG                                                 
         MVC   P+22(20),RADVNAME                                                
         GOTO1 REPORT                                                           
         MVI   PRNTFLAG,C'Y'       SET PRINT FLAG                               
NADV0040 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  CLI   SADV,X'FF'          EOF REACHED?                                 
         BER   R8                  YES                                          
         MVI   SADV,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZR   R8                  TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        CLC   =C'PRINTSR',QUESTOR                                              
*        BNE   GETS0010                                                         
*        MVC   P+5(07),=C'GETSORT'                                              
*        MVC   P+15(08),SORTREC                                                 
*        GOTO1 REPORT                                                           
GETS0010 EQU   *                                                                
         BR    R8                                                               
         EJECT                                                                  
*   PROCESS SECONDARY SORT RECORDS                                              
         SPACE 3                                                                
PRODPOST NTR1                                                                   
*                                                                               
         MVC   P+1(09),=C'PRODPOST:'                                            
         EDIT  PRODCTR,(9,P+12)                                                 
         GOTO1 REPORT                                                           
         XC    SORTCTR,SORTCTR                                                  
*                                                                               
         XC    KEYSAV2,KEYSAV2     CLEAR ALTERNATE KEYSAVE AREA                 
         OPEN  (CATWORK,INPUT)     OPEN SECONDARY FILE                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                  RESET SORT FILE                              
PPOS0020 EQU   *                                                                
         GET   CATWORK,SORTREC                                                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     PPOS0020            GO BACK FOR NEXT RECORD                      
*                                                                               
PPOS0040 EQU   *                                                                
         CLOSE CATWORK             CLOSE SCRATCH FILE - NO MORE USE             
*                                                                               
         MVC   P+1(09),=C'CATCLOSED'                                            
         GOTO1 REPORT                                                           
*                                                                               
PPOS0060 EQU   *                                                                
         BAS   R8,GETSORT          RETRIEVE SORT RECORD                         
         CLI   SADV,X'FF'          EOF REACHED?                                 
         BE    PPOS0240            YES - FINISHED                               
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
         SR    RE,RE               CLEAR FOR DIVIDE                             
         D     RE,=F'100'                                                       
         LTR   RE,RE               ANY REMAINDER?                               
         BNZ   PPOS0080            YES - NOT EVEN 100                           
         MVC   P+1(14),=C'PROCESSED/PROD'                                       
         EDIT  SORTCTR,(9,P+20)                                                 
         GOTO1 REPORT                                                           
PPOS0080 EQU   *                                                                
         CLC   SADV(7),RPRDKADV    SAME ADVERTISER/PRODUCT?                     
         BE    PPOS0140            YES - PROCESS CONTRACT                       
         BL    PPOS0100            CONTRACT ADV CODE MISSED                     
         BAS   RE,NEWPROD          GET NEXT ADVERTISER/PRODUCT                  
         B     PPOS0080            CHECK OUT NEXT ADVERTISER/PRODUCT            
PPOS0100 EQU   *                                                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   PPOS0120                                                         
         MVC   PX+5(18),=C'MISSED =      /   '                                  
         MVC   PX+15(04),SADV                                                   
         GOTO1 HEXOUT,DMCB,SDSKADDR,PX+24,4,=C'TOG'                             
         MVC   PX+35(09),=C'RPRDKADV='                                          
         MVC   PX+45(4),RPRDKADV                                                
         MVC   P(80),PX                                                         
         MVC   PX(80),SPACES                                                    
         GOTO1 REPORT                                                           
PPOS0120 EQU   *                                                                
         B     PPOS0060            GO BACK FOR NEXT SORT                        
PPOS0140 EQU   *                                                                
         XC    KEY,KEY             NOW RETURN THE CONTRACT RECORD               
         MVC   KEY+28(4),SDSKADDR  SET D/A OF CONTRACT RECORD                   
         BAS   RE,GETCON           READ CON REC VIA DISK ADDR                   
         CLC   =C'PRINT',QUESTOR                                                
         BNE   PPOS0160                                                         
         MVC   PX+5(18),=C'CONPROD=      /   '                                  
         MVC   PX+15(04),RCONKADV                                               
         MVC   PX+20(2),RCONCTGY                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,PX+24,4,=C'TOG'                             
PPOS0160 EQU   *                                                                
         MVC   RCONCTGY,SCAT       INSERT CODE FROM SORTREC                     
         CLC   =C'  ',RPRDCATG     CATEGORY CODE = SPACE?                       
         BE    PPOS0220            YES - NO UPDATE                              
         CLC   =X'0000',RPRDCATG   CATEGORY CODE = BINARY ZERO?                 
         BE    PPOS0220            YES - NO UPDATE                              
         CLC   RPRDCATG,RCONCTGY   CATEGORY CODES SAME?                         
         BE    PPOS0220            YES - NO UPDATE                              
         MVC   OLDCTGY,RCONCTGY    SAVE OLD CATEGORY                            
         MVC   RCONCTGY,RPRDCATG   NO  - UPDATE IT                              
         BAS   RE,PUTCON           REWRITE RECORD                               
         L     RF,PRODCTR2         INCREMENT CHANGED REC COUNTER                
         LA    RF,1(RF)                                                         
         ST    RF,PRODCTR2                                                      
PPOS0180 EQU   *                                                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   PPOS0220                                                         
         MVC   PX+37(18),=C'UPDTD  =      /   '                                 
         MVC   PX+47(04),RCONKADV                                               
         MVC   PX+52(2),RCONCTGY                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,PX+56,4,=C'TOG'                             
         EDIT  PRODCTR2,(9,PX+68)                                               
         CLI   PRNTFLAG,C'Y'       PRINT FLAG SET?                              
         BE    PPOS0200            YES                                          
*                                                                               
*   PRINT INTERVAL COUNTER                                                      
*                                                                               
         LH    R1,=H'1000'         PRINT EVERY NTH RECORD                       
         SR    RE,RE               INITIALIZE FIRST OF PAIR                     
         L     RF,PROCCTR          CURRENT CONTRACT COUNT                       
         DR    RE,R1               DIVIDE COUNT BY INCREMENT                    
         LTR   RE,RE               TEST FOR ZERO REMAINDER                      
         BNZ   PPOS0220            NOT ZERO - DON'T PRINT                       
PPOS0200 EQU   *                                                                
         MVI   PRNTFLAG,C'N'       TURN PRINT FLAG OFF                          
         MVC   P(80),PX            MOVE SECOND PRINTLINE TO FIRST               
         GOTO1 REPORT              PRINT THE ENTRY                              
PPOS0220 EQU   *                                                                
         MVC   PX(80),SPACES                                                    
         B     PPOS0060            GO BACK FOR NEXT CONTRACT                    
PPOS0240 EQU   *                                                                
         CLI   PRODCATG,0          CHECK FOR PRODUCT CODE?                      
         BE    PPOS0280            NO  - NO CATEGORY PRIORITY                   
         CLOSE CATWORK                                                          
PPOS0280 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   NEWPROD: RETRIEVES NEXT ADVERT/PROD RECORD IN SEQUENCE.  BECAUSE            
*      THE CONTRACT RECORD IS READ VIA DISK ADDRESS AFTER - REPEAT,             
*      AFTER - THE ADVERT/PROD RECORD, THE CONTRACT RECORD IS                   
*      AVAILABLE FOR RE-WRITE WITH NO REPOSITIONING OF THE FILE.                
*                                                                               
NEWPROD  NTR1                                                                   
         OC    KEYSAV2,KEYSAV2     1ST TIME?                                    
         BNZ   NPRO0010            NO  - USE KEY                                
         MVI   KEYSAV2,X'09'       YES - SET 1ST KEY                            
         MVC   KEY,KEYSAV2         LOAD THE NEW KEY                             
         GOTO1 HIGH                START THE CHAIN                              
         B     NPRO0030                                                         
NPRO0010 EQU   *                                                                
         MVC   KEY,KEYSAV2         SET KEY FOR NEXT ADVER/PROD RECORD           
NPRO0020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT SEQUENTIAL                         
NPRO0030 EQU   *                                                                
         CLI   KEY,X'09'           ADVER/PROD KEY?                              
         BE    NPRO0035            YES                                          
         MVC   RPRDKADV,=X'FFFFFFFF'                                            
*                                  NO  - SET TO HIGH VALUE                      
         B     MODEEXIT            RETURN                                       
NPRO0035 EQU   *                                                                
         CLC   ALTREPFL,KEY+25     CORRECT REP?                                 
         BNE   NPRO0020            NO  - GET NEXT KEY                           
         BAS   RE,GETPROD                                                       
         MVC   KEYSAV2,RPRDREC     SAVE ADV/PROD KEY FOR SEQ READ               
         CLC   =C'PRINT',QUESTOR                                                
         BNE   NPRO0040                                                         
*                         2.4.6.8.0.2.4.6.8.0.2.4.6.8.0                         
         MVC   P+2(20),=C'ADVERT = XXXX/XXX/XX'                                 
         MVC   P+11(04),RPRDKADV                                                
         MVC   P+16(03),RPRDKPRD                                                
         MVC   P+20(2),RPRDCATG                                                 
         MVC   P+24(20),RPRDNAME                                                
         GOTO1 REPORT                                                           
         MVI   PRNTFLAG,C'Y'       SET PRINT FLAG                               
NPRO0040 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETPROD  LA    RF,RPRDREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
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
         BE    PUTF0020            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
PUTF0020 EQU   *                                                                
         BAS   RE,UPRISKEY         UPDATE RIS KEYS                              
         XIT1                                                                   
         SPACE 3                                                                
UPRISKEY NTR1                                                                   
*                                                                               
*   TEST                                                                        
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0020                                                         
         MVC   P+1(12),=C'KEY/KEYSAV2:'                                         
         MVC   P+13(27),KEY                                                     
         MVI   P+40,C'/'                                                        
         MVC   P+41(27),KEYSAV2                                                 
         GOTO1 REPORT                                                           
UPRI0020 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8D'           SET KEY TYPE                                 
         MVC   KEY+1(2),RCREPFL    INSERT REP CODE                              
**       MVC   KEY+3(5),RCONKSTA   NO STATION FOR 8D                            
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,KEY+8)                               
*                                  CONVERT START DATE TO COMPRESSED             
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,KEY+10)                            
         MVC   KEY+12(4),RCONKCON  INSERT CONTRACT NUMBER                       
         MVI   KEY+16,2            INSERT RECORD TYPE '2'                       
         MVC   KEY+17(3),RCONSAL   INSERT SALESPERSON CODE                      
         MVC   KEY+20(1),RCONTYPE  INSERT CONTRACT TYPE                         
         MVC   KEY+21(2),RCONKGRP  INSERT GROUP/SUBGROUP                        
         MVC   KEY+23(2),OLDCTGY   INSERT ORIGINAL CATEGORY CODE                
         MVC   KEY+25(2),RCONTEM   INSERT TEAM                                  
*                                  CONVERT END   DATE TO COMPRESSED             
         MVC   ORIGKEY,KEY         SAVE ORIGINAL KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY SHOULD BE ON FILE                        
         BE    UPRI0040            FOUND                                        
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0030                                                         
         MVC   P+1(22),=C'8D NF: MUST BE ON FILE'                               
         MVC   P+7(27),KEYSAVE                                                  
         GOTO1 REPORT                                                           
UPRI0030 EQU   *                                                                
         MVC   KEY(27),ORIGKEY     RESET KEY NOT FOUND                          
         B     UPRI0080            ADD NEW 8D KEY                               
UPRI0040 EQU   *                                                                
         OI    KEY+27,X'FE'        TURN ON DELETE BIT (UNIQUE)                  
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    UPRI0060            YES - DON'T REWRITE IT                       
         GOTO1 WRITE               REWRITE KEY                                  
UPRI0060 EQU   *                                                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0080                                                         
         MVC   P+1(09),=C'8D FOUND:'                                            
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
UPRI0080 EQU   *                                                                
*                                                                               
         NI    KEY+27,X'FF'-X'FE'  TURN OFF DELETE BIT                          
         MVC   KEY+23(2),RCONCTGY  INSERT NEW CATEGORY                          
         OI    DMINBTS,X'08'       READ FOR 'NEW' KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY ALREADY ON FILE?                         
         BNE   UPRI0100            NO  - ADD NEW KEY                            
         NI    KEY+27,X'FF'-X'FE'  TURN OFF DELETE BIT                          
         GOTO1 WRITE                                                            
         MVC   P+1(09),=C'8D REWRI:'                                            
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
         B     UPRI0120                                                         
UPRI0100 EQU   *                                                                
         GOTO1 ADD                 WRITE NEW KEY                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0120                                                         
         MVC   P+1(09),=C'8D ADDED:'                                            
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
UPRI0120 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'8E'           SET KEY TYPE                                 
         MVC   KEY+1(2),RCREPFL    INSERT REP CODE                              
         MVC   KEY+3(5),RCONKSTA   STATION FOR 8E                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,KEY+8)                               
*                                  CONVERT START DATE TO COMPRESSED             
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,KEY+10)                            
         MVC   KEY+12(4),RCONKCON  INSERT CONTRACT NUMBER                       
         MVI   KEY+16,2            INSERT RECORD TYPE '2'                       
         MVC   KEY+17(3),RCONSAL   INSERT SALESPERSON CODE                      
         MVC   KEY+20(1),RCONTYPE  INSERT CONTRACT TYPE                         
         MVC   KEY+21(2),RCONKGRP  INSERT GROUP/SUBGROUP                        
         MVC   KEY+23(2),OLDCTGY   INSERT ORIGINAL CATEGORY CODE                
         MVC   KEY+25(2),RCONTEM   INSERT TEAM                                  
*                                  CONVERT END   DATE TO COMPRESSED             
         MVC   ORIGKEY,KEY         SAVE ORIGINAL KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY SHOULD BE ON FILE                        
         BE    UPRI0140            FOUND                                        
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0130                                                         
         MVC   P+1(22),=C'8E NF: MUST BE ON FILE'                               
         MVC   P+23(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
UPRI0130 EQU   *                                                                
         MVC   KEY(27),ORIGKEY     RESET KEY NOT FOUND                          
         B     UPRI0180            ADD NEW 8E KEY                               
UPRI0140 EQU   *                                                                
         OI    KEY+27,X'FE'        TURN ON DELETE BIT (UNIQUE)                  
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    UPRI0160            YES - DON'T REWRITE IT                       
         GOTO1 WRITE               REWRITE KEY                                  
UPRI0160 EQU   *                                                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0180                                                         
         MVC   P+1(09),=C'8E FOUND:'                                            
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
UPRI0180 EQU   *                                                                
         NI    KEY+27,X'FF'-X'FE'  TURN OFF DELETE BIT                          
         MVC   KEY+23(2),RCONCTGY  INSERT NEW CATEGORY                          
         OI    DMINBTS,X'08'       READ FOR 'NEW' KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY ALREADY ON FILE?                         
         BNE   UPRI0190            NO  - ADD NEW KEY                            
         NI    KEY+27,X'FF'-X'FE'  TURN OFF DELETE BIT                          
         GOTO1 WRITE                                                            
         MVC   P+1(09),=C'83 REWRI:'                                            
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
         B     UPRI0200                                                         
UPRI0190 EQU   *                                                                
         GOTO1 ADD                 WRITE NEW KEY                                
         CLC   =C'PRINT',QUESTOR                                                
         BNE   UPRI0200                                                         
         MVC   P+1(09),=C'8E ADDED:'                                            
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
UPRI0200 EQU   *                                                                
         OC    KEYSAV2,KEYSAV2     ANY RESTART LOCATION?                        
         BZ    UPRI0900            NO                                           
         MVC   KEY,KEYSAV2                                                      
         GOTO1 HIGH                                                             
UPRI0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
         DC    CL8'**SORT**'                                                    
SORTREC  DS    0CL17                                                            
SADV     DS    CL4                 ADVERTISER CODE                              
SPROD    DS    CL3                 PRODUCT    CODE                              
SCAT     DS    CL2                 CATEGORY CODE                                
SDSKADDR DS    CL4                 CATEGORY CODE                                
SCONNUM  DS    CL4                 CONTRACT NUMBER                              
*                                                                               
PROCCTR  DS    F                   CONTRACTS PROCESSED CTR                      
PRODCTR  DS    F                   PRODUCT RECORD COUNTER                       
PRODCTR2 DS    F                   PRODUCT CATS CHANGED COUNTER                 
SORTCTR  DS    F                   SORT    RECORD COUNTER                       
PRNTFLAG DS    CL1                                                              
PRODCATG DS    XL1                 REP PREFERS PRODUCT CATEGORY                 
*                                  0  =  USE ADVERTISER CATEGORY                
*                                  1  =  USE PRODUCT    CATEGORY                
ALTREPFL DS    CL2                                                              
OLDCTGY  DS    CL2                 ORIGINAL CATEGORY CODE                       
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
ORIGKEY  DS    CL27                                                             
PX       DS    CL80                ALTERNATE PRINT SETUP AREA                   
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,09,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=17'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
*                                                                               
CATWORK  DCB   DDNAME=CATWORK,DSORG=PS,EODAD=PPOS0040,LRECL=17,        X        
               BLKSIZE=4352,MACRF=(GM,PM),RECFM=FB                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115REREP1F02X06/25/02'                                      
         END                                                                    
