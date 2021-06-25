*          DATA SET REREP1C02  AT LEVEL 043 AS OF 05/01/02                      
*PHASE RE1C02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP1C02 - BUDGET ALLOCATION SPREADER'                         
*********************************************************************           
*                                                                   *           
*        REREP1C02 --- REPPAK BUDGET WORKSHEET/PRELIM/FINALS        *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* SEP05/90 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* NOV28/90 (BU ) --- INCORPORATION OF JOINED/LEFT LOGIC AND APPLI-  *           
*                    CATION OF COMPANY PERCENTS, WHERE APPROPRIATE, *           
*                    AS WELL AS A RESTART MECHANISM                 *           
*                                                                   *           
* MAR08/91 (BU ) --- CHANGE RESTART MECHANISM BY CALCULATING THE    *           
*                    STARTING ALLOCATION $ FROM THE ALLOCATIONS OF  *           
*                    EACH CONTRACT TYPE IN THE REQUEST              *           
*                                                                   *           
* NOV07/91 (BU ) --- ADJUST FOR NEW MONTABLE SETUP FOR VALUENEW     *           
*                                                                   *           
* NOV11/91 (BU ) --- INCORPORATE CHANGES REQUESTED BY INTEREP:      *           
*                    PERCENTAGE CALC BASIS, TOTALLING, ETC          *           
*                                                                   *           
* DEC06/91 (BU ) --- BUILD IN NEW TOTALS                            *           
*                                                                   *           
* DEC16/91 (BU ) --- ADD 'NO BUDGET W/CONTRACT $' ENTRIES TO EXCEPT-*           
*                    ION LIST                                       *           
*                                                                   *           
* FEB27/92 (BU ) --- INSERT DATE ALLOCATED (MMDD BINARY) INTO ALLOC *           
*                    ELEMENTS.  IF ADDED, SET X'80' BIT IN MM.      *           
*                    ALSO FIX ERROR WHERE WRAPPED JOBS WERE PASSING *           
*                    CARD2 OPTIONS TO LATER RUNS.                   *           
*                                                                   *           
* MAR27/92 (BU ) --- UPGRADE FOR NEW VALU2NEW.                      *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* JUN24/97 (BU ) --- SKIP SPECIAL COMPANY C*MP BUDGETS              *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R8  =  THIRD  BASE REGISTER                                  *           
*********************************************************************           
*                                                                               
RE1C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1C02,R7,R8,RR=RE                                           
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
         GOTO1 (RF),DMCB,(RC)      GO TO THE ROUTINE                            
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         B     MAINBAD             NON-ZERO IS ERROR                            
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
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(GRUPFRST),AL3(GRPINIT)   GROUP FIRST                         
         DC    AL1(STAFRST),AL3(STAINIT)    STATION FIRST                       
         DC    AL1(OFFFRST),AL3(OFFINIT)    OFFICE FIRST                        
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(STALAST),AL3(STADONE)    STATION BREAK                       
         DC    AL1(OFFLAST),AL3(OFFDONE)    OFFICE  BREAK                       
         DC    AL1(GRUPLAST),AL3(GRPDONE)   GROUP   BREAK                       
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         L     RF,=A(SAVEREGS)                                                  
         STM   R2,RC,0(RF)         SAVE REGS 2 -> C                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   CONFLAG,C'N'        SET CONTRACT FLAG TO NO                      
         L     R3,=A(ALTHDR)       STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC2,SORTREC2                                                
         LA    RF,ACCTBL           ZERO OUT ACCUMULATOR TABLE                   
         LA    R3,LACCOFF          SET L(ACCUM TABLE)                           
         XCEFL 0(RF),(R3)                                                       
         LA    RF,TOTTABLE         ZERO OUT TOTALS TABLE                        
         LA    R3,LTOTTOT          SET L(TOTALS TABLE)                          
         XCEFL 0(RF),(R3)                                                       
         LA    RE,NJSTATS          ZERO OUT NOT-JOINED STATIONS                 
         LA    RF,LNJSTATS         SIZE OF TABLE                                
         XCEF                                                                   
*                                                                               
*   THE NOT-JOINED/LEFT TABLE IS STARTED 5 BYTES BEFORE THE FIRST               
*        ENTRY (IN THE PRECEDING TABLE).  THIS IS TO TAKE CARE OF               
*        THE 'FIRST-TIME' PATH.                                                 
*                                                                               
         LA    R3,NJSTATS-8        STORE A(NOT-JOINED STAT TABLE)               
         ST    R3,ANJSTATS                                                      
*                                                                               
*   INITIALIZE AREA TO BUILD BUDGET CONTRACT TYPE ELEMENTS, IF NEEDED           
*                                                                               
         XC    WORKX(150),WORKX                                                 
         MVC   WORKX(2),=X'026E'   SET ELCODE, LENGTH                           
*                                                                               
*   INITIALIZE AREA TO BUILD RESTART ALLOCATION ELEMENT, IF NEEDED              
*                                                                               
         XC    WORKY(50),WORKY                                                  
         MVC   WORKY(2),=X'0330'   SET ELCODE, LENGTH                           
*                                                                               
*   PICK UP 2ND REQUEST CARD                                                    
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED TWO CARDS                               
         BE    IN002                                                            
         MVC   P(L'CARDMISS),CARDMISS                                           
         B     BADEXIT                                                          
IN002    EQU   *                                                                
         L     R4,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R4)                                                     
         DROP  R5                                                               
*                                                                               
*   SET UP SPREAD CONTRACT TYPES, AND PERCENTAGES                               
*                                                                               
IN003    EQU   *                                                                
         MVC   SPREDCT,=X'40004000400040004000'                                 
*                                  INITIALIZE TO FIVE OCCURRENCES OF:           
*                                    BYTE 1  = CONTRACT TYPE                    
*                                    BYTE 2  = PERCENT: BINARY                  
*                                                                               
         LA    R2,SPREDCT          A(SPREAD PERCENTAGE ARRAY)                   
         LA    R3,5                NUMBER OF ENTRIES                            
         LA    R4,CARD2+12         A(1ST ENTRY, CARD2)                          
*                                                                               
*  CHECK CARD LAYOUT AS A FUNCTION OF REQUESTOR                                 
*                                                                               
IN004    EQU   *                                                                
         CLI   0(R4),C' '          CONTRACT TYPE ENTERED?                       
         BE    IN008               NO  - FINISHED                               
         MVC   0(1,R2),0(R4)       STORE CONTRACT TYPE                          
         OC    1(3,R4),=X'F0F0F0'  FILL LEADING SPACE, IF ANY                   
         PACK  CONTOTS(8),1(3,R4)  PACK PERCENTAGE                              
         CVB   R5,CONTOTS          CONVERT IT TO BINARY                         
         STC   R5,1(R2)            SAVE PERCENT                                 
*                                                                               
*  PERCENT CAN'T EXCEED 100.  STORE ONLY 1 BYTE                                 
*                                                                               
         LA    R2,2(R2)            NEXT A(CONTRACT TYPE)                        
         LA    R4,4(R4)            NEXT CARD ENTRY                              
         BCT   R3,IN004            PROCESS NEXT ENTRY                           
IN008    EQU   *                                                                
*                                                                               
*   SET ALIGNMENT: BUDGET DATE RANGE VS BILLING DATE RANGE                      
*      BUDGET MAY BE JAN-DEC, BILLING EXTRACT MAY BE OCT-SEP                    
*      MONTHLY BUCKETS IN SORT RECORDS WILL BE ALIGNED SO THAT                  
*      BILLING IS ALIGNED WITH BUDGET POSITIONS                                 
*                                                                               
         MVC   SETDT(4),CBUDFRM    SET UP DISPLAY BUDGET START                  
         GOTO1 DATCON,DMCB,(0,SETDT),(6,DBUDFRM),0                              
         MVC   SETDT(4),CBUDTO     SET UP DISPLAY BUDGET END                    
         GOTO1 DATCON,DMCB,(0,SETDT),(6,DBUDTO),0                               
         MVC   SETDT(4),QSTART     ESTABLISH BILLING START DATE                 
         GOTO1 DATCON,DMCB,(0,SETDT),(3,BILSTDTE),0                             
         MVC   SETDT(4),CBUDFRM    ESTABLISH BUDGET START DATE                  
         GOTO1 DATCON,DMCB,(0,SETDT),(3,BUDSTDTE),0                             
         ZIC   R2,BILSTDTE+1       MONTH OF BILLING START DATE                  
         ZIC   R3,BUDSTDTE+1       MONTH OF BUDGET  START DATE                  
*                                                                               
*   SETTING 'BUKSTART':  INITIALIZED TO ZERO, 'BUKSTART' IS THE                 
*        ALIGNMENT VALUE SO THAT THE $ BUCKETS ARE POSITIONALLY                 
*        CONSISTENT WITH THE BUDGET DATE RANGE.                                 
*                                                                               
         CR    R2,R3                                                            
         BE    IN012               BUKSTART INITIALIZED TO ZERO                 
         BH    IN010               BUDGET < BILLING                             
         AH    R2,=H'12'           SET BUDGET > BILLING                         
IN010    EQU   *                                                                
         SR    R2,R3               SUBTRACT BUD FROM BIL MONTH                  
         STC   R2,BUKSTART         SAVE START OFFSET                            
IN012    EQU   *                                                                
         BAS   RE,SETRECON         ESTABLISH 4-5 WEEK MATRIX                    
         XC    CONTOTS,CONTOTS                                                  
*                                                                               
*   GET TODAY'S DATE FOR DATE ALLOCATED.                                        
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,ALLOCDTE)                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   ESTABLISH 4-5 WEEK/MONTH MATRIX FOR PRIOR/CURRENT YEARS                     
*        IN ADDITION TO ESTABLISHING THE MATRIX, THE BILLING YEAR               
*        ARRAY IS ALIGNED POSITIONALLY WITH THE BUDGET YEAR ARRAY.              
*        THAT IS, IF THE BILLING YEAR IS OCT-SEP, AND THE BUDGET YEAR           
*        IS JAN-DEC, THE BILLING YEAR ARRAY WILL BE SET JAN-DEC,                
*        WRAPPING-AROUND THE YEAR BREAK                                         
*                                                                               
SETRECON NTR1                                                                   
         PACK  YF(2),QSTART(2)        TAKE YEAR-FROM                            
         PACK  MF(2),QSTART+2(2)      TAKE MONTH-FROM                           
         PACK  YT(2),QEND(2)          TAKE YEAR-TO                              
         PACK  MT(2),QEND+2(2)        TAKE MONTH-TO                             
         LA    R3,BILLARR             A(BILLING ARRAY)                          
         LA    R4,BILLARR             A(BILLING ARRAY) FOR RESET                
*                                                                               
*   INSERT ALIGNMENT OFFSET FOR BILLING VS BUDGET                               
*                                                                               
         ZIC   R2,BUKSTART                                                      
         AR    R3,R2               ADD OFFSET                                   
         LA    R2,ENDBILL          SET END-OF-ARRAY ADDRESS                     
         MVI   SETJOIN,C'Y'        SET BILLING START-END FOR JOINED             
         BAS   RE,SETREC2                                                       
*                                                                               
         PACK  YF(2),CBUDFRM(2)    TAKE YEAR-FROM                               
         PACK  MF(2),CBUDFRM+2(2)  TAKE MONTH-FROM                              
         PACK  YT(2),CBUDTO(2)     TAKE YEAR-TO                                 
         PACK  MT(2),CBUDTO+2(2)   TAKE MONTH-TO                                
         LA    R3,BUDGARR          A(BUDGET ARRAY)                              
         LA    R4,BUDGARR          A(BUDGET ARRAY) FOR RESET                    
         LA    R2,ENDBUDG          SET END-OF-ARRAY ADDRESS                     
         MVI   SETJOIN,C'N'        DON'T RESET JOINED DATES                     
         BAS   RE,SETREC2                                                       
*                                                                               
         B     MODEEXIT               RETURN                                    
         SPACE 5                                                                
*                                                                               
*   USE DATES TO CALL GETBROAD, LOAD FLAGS TO ARRAYS                            
*       R2   = END OF ARRAY ADDRESS FOR WRAP-AROUND                             
*       R3   = STARTING ADDRESS IN ARRAY                                        
*       R4   = START OF ARRAY FOR RESET                                         
*                                                                               
SETREC2  NTR1                                                                   
SR002    EQU   *                                                                
         CLI   SETJOIN,C'Y'        CHECK DO-IT FLAG                             
         BNE   SR004               NOT SET - DON'T DO-IT                        
         UNPK  DTWK2(2),YF(2)      ESTABLISH 'FROM-DATE'                        
         UNPK  DTWK2+2(2),MF(2)                                                 
         MVC   DTWK2+4(2),=C'15'   SET JOIN CUTOFF TO 15TH                      
         OC    DTWK2(4),=X'F0F0F0F0'    ENSURE NUMERICS                         
         GOTO1 DATCON,DMCB,(0,DTWK2),(3,JOINSTDT),0                             
         UNPK  DTWK2(2),YT(2)      ESTABLISH 'TO-DATE'                          
         UNPK  DTWK2+2(2),MT(2)                                                 
         MVC   DTWK2+4(2),=C'16'   SET JOIN CUTOFF TO 15TH                      
         OC    DTWK2(4),=X'F0F0F0F0'    ENSURE NUMERICS                         
         GOTO1 DATCON,DMCB,(0,DTWK2),(3,JOINENDT),0                             
SR004    EQU   *                                                                
         MVC   DTWK2+4(2),=C'01'   RESET WORK AREA TO DAY=01                    
         UNPK  DTWK2(2),YF(2)      RE-ESTABLISH 'FROM-DATE'                     
         UNPK  DTWK2+2(2),MF(2)                                                 
         OC    DTWK2(4),=X'F0F0F0F0'    ENSURE NUMERICS                         
         GOTO1 =V(GETBROAD),DMCB,(0,DTWK2),CONTOTS,0,0                          
         MVI   0(R3),C'4'          PRE-LOAD 4-WEEK INDICATOR                    
         CLI   DMCB,X'04'          FOUR-WEEK MONTH?                             
         BE    SR006               YES -                                        
         MVI   0(R3),C'5'          NO  - LOAD INDICATOR                         
SR006    EQU   *                                                                
*                                                                               
*    THE FOLLOWING DATE RTN HAS BEEN REPLACED BY DATE ROUTINE AT                
*       SR002.  THIS ROUTINE BASES CUTOFF DATES ON BROADCAST MONTH.             
*       THE CODE REMAINS IN THE EVENT SUCH A CHANGE IS DESIRED.                 
*       IN WHICH CASE, THE ROUTINEAT SR002 SHOULD BE INACTIVATED,               
*       AND THIS REACTIVATED.                                                   
*                                                                               
*    IF 'SETJOIN=Y' THE CUTOFF DATES FOR DATE-JOINED/DATE-LEFT                  
*    MUST BE SET.  THE FIRST BROADCAST-MONTH START DATE RETURNED                
*    BY 'GETBROAD' HAS 15 DAYS ADDED TO IT, IS THEN CONVERTED TO                
*    THREE-BYTE YMD FORMAT, AND STORED AS 'JOINSTDT'.  EACH OF THE              
*    BROADCAST-MONTH END DATES RETURNED BY 'GETBROAD' IN TURN HAS               
*    15 DAYS SUBTRACTED FROM IT, IS THEN CONVERTED TO THREE-BYTE                
*    YMD FORMAT, AND STORED AS 'JOINENDT'.  THE FINAL CONVERTED                 
*    END-DATE IS THE BILLING PERIOD END DATE.                                   
*                                                                               
*        CLI   SETJOIN,C'Y'        CHECK DO-IT FLAG                             
*        BNE   SR006B              NOT SET - DON'T DO-IT                        
*        OC    JOINSTDT,JOINSTDT   ANY DATE JOINED?                             
*        BNZ   SR006A              YES - DON'T OVERLAY                          
*        LH    R6,=H'15'           FIFTEEN DAYS FROM START                      
*        ST    R6,DMCB+8           STORE IN THIRD FULL WORD                     
*        GOTO1 =V(ADDAY),DMCB,CONTOTS,CONTOTS                                   
*        GOTO1 DATCON,DMCB,(0,CONTOTS),(3,JOINSTDT),0                           
SR006A   EQU   *                                                                
*        LH    R6,=H'-15'          FIFTEEN DAYS BEFORE END                      
*        ST    R6,DMCB+8           STORE IN THIRD FULL WORD                     
*        GOTO1 =V(ADDAY),DMCB,CONTOTS+6,CONTOTS+6                               
*        GOTO1 DATCON,DMCB,(0,CONTOTS+6),(3,JOINENDT),0                         
SR006B   EQU   *                                                                
         LA    R3,1(R3)            BUMP TABLE                                   
         CR    R3,R2               WRAP-AROUND POINT?                           
         BNE   SR008               NOT REACHED                                  
         LR    R3,R4               RESET TO BEGINNING OF ARRAY                  
SR008    EQU   *                                                                
         CP    YF(2),YT            CHECK FOR END OF TABLE                       
         BNE   SR010                                                            
         CP    MF(2),MT            CHECK MONTH FOR END                          
         BE    SR099               END REACHED                                  
SR010    EQU   *                                                                
         CP    MF(2),=P'12'        LAST MONTH OF YEAR?                          
         BNE   SR014               NO                                           
         ZAP   MF(2),=P'01'        RESET MONTH TO 1                             
         CP    YF(2),=P'99'        YEAR = 99?                                   
         BNE   SR012               NO                                           
         ZAP   YF(2),=P'00'        YES - SET TO 00, DON'T INCREMENT             
         B     SR004                                                            
SR012    EQU   *                                                                
         AP    YF(2),=P'01'        NO  - INCREMENT YEAR BY 1                    
         B     SR004                                                            
SR014    AP    MF(2),=P'01'        INCREMENT MONTH BY 1                         
         B     SR004                                                            
SR099    EQU   *                                                                
         B     MODEEXIT            RETURN                                       
         EJECT                                                                  
*   GRPINIT: FIRST GROUP MODE SETTING                                           
*                                                                               
GRPINIT  NTR1                                                                   
         XC    SORTREC,SORTREC              SET UP BASIC KEY                    
         MVC   SORTREC+SGROUP(2),RCONKGRP   INSERT GROUP                        
         MVC   SORTREC+SSTAT1(5),RCONKSTA   INSERT STATION                      
         MVC   SORTREC+SOFF1(2),RCONKOFF    INSERT OFFICE                       
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
*   STAINIT: FIRST STATION MODE SETTING                                         
*                                                                               
STAINIT  NTR1                                                                   
         MVC   SORTREC+SGROUP(2),RCONKGRP    INSERT GROUP                       
         MVC   SORTREC+SSTAT1(5),RCONKSTA    INSERT STATION                     
         MVC   SORTREC+SOFF1(2),RCONKOFF     INSERT OFFICE                      
         MVC   SORTREC+SMKTNM(20),RSTAMKT    INSERT MARKET NAME                 
         MVC   SORTREC+SOFFNM(20),ROFFNAME   INSERT OFFICE NAME                 
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFINIT: FIRST OFFICE                                                       
*                                                                               
OFFINIT  NTR1                                                                   
         MVC   SORTREC+SGROUP(2),RCONKGRP    INSERT GROUP                       
         MVC   SORTREC+SSTAT1(5),RCONKSTA    INSERT STATION                     
         MVC   SORTREC+SOFF1(2),RCONKOFF     INSERT OFFICE                      
         MVC   SORTREC+SOFFNM(20),ROFFNAME   INSERT OFFICE NAME                 
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
*                                                                               
*              NOW LOOP THROUGH THE MONTH TABLE                                 
*                                                                               
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
         L     R4,ANEWMON          A(NEW MONTH TABLE)                           
         LA    R3,TMONTBL          A(ACCUMULATORS)                              
         LA    R1,L'TMONTBL(R3)    CALC END OF TABLE FOR WRAP                   
         ZIC   R5,BUKSTART         ESTABLISH OFFSET FOR ALIGNMENT               
         SLA   R5,2                MULTIPLY BY 4 FOR OFFSET                     
         AR    R3,R5               ADD OFFSET TO START                          
*                                                                               
POST02   EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST12              FOUND - BEGIN TO PULL FIGURES                
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST02              GO BACK FOR NEXT                             
*                                                                               
POST12   EQU   *                                                                
         CLI   0(R4),0                                                          
         BE    POST20                                                           
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END   DATE                
         BH    POST20              TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROL                           
*                                                                               
         TM    FLAG6(R4),X'01'          ANY INVOICED $ IN BUCKET?               
         BZ    POST14                   NO  - TAKE ORDERED $                    
         MVC   CONTOTS(4),CUASATIN(R6)  YES = INVOICED THIS YEAR                
         B     POST18                                                           
*                                                                               
POST14   EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)    NO  = ORDERED THIS YEAR                 
*                                                                               
POST18   EQU   *                                                                
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD CURRENT BEST $                           
         LTR   R0,R0               TEST FOR NON-ZERO DOLLARS                    
         BZ    POST18A             ZERO DOLLARS IN BUCKET                       
         MVI   CONFLAG,C'Y'        SET CONTRACT FLAG TO YES                     
POST18A  EQU   *                                                                
         A     R0,0(R3)            TO ACCUMULATOR CURR BEST $                   
         ST    R0,0(R3)            STORE IT BACK                                
*                                                                               
         LA    R3,4(R3)            BUMP A(TMONTBL ACCUM)                        
         CR    R3,R1               END OF TABLE FOR WRAP-AROUND?                
         BNE   POST19              NO                                           
         LA    R3,TMONTBL          RELOAD START OF TABLE                        
POST19   EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST12              SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
POST20   EQU   *                                                                
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
MEXIT    EQU   *                                                                
         XIT1                                                                   
BADEXIT  EQU   *                                                                
         LA    R0,1                SET CC NOT = ZERO                            
         B     MEXIT               RETURN                                       
         EJECT                                                                  
*   GRPDONE:  LAST GROUP MODE SETTING                                           
*                                                                               
GRPDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    GDEXIT              YES                                          
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
GDEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   STADONE:  LAST STATION MODE SETTING                                         
*                                                                               
STADONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    SDEXIT              YES                                          
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
SDEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFDONE:  LAST OFFICE MODE SETTING                                          
*                                                                               
OFFDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    ODEXIT              YES                                          
*                                                                               
         GOTO1 =A(SORTGEN),DMCB,(RC)                                            
ODEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1                                                                   
         CLI   CONFLAG,C'Y'        CONFLAG SET?                                 
         BNE   SG099               NO  - DON'T PUT OUT RECORD ON BREAK          
         MVI   CONFLAG,C'N'        RESET FLAG TO NO                             
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         CLC   KEY+20(5),SAVESTA   STATION PREVIOUSLY SEEN?                     
         BE    SG014               YES - SKIP REREADING IT                      
         MVC   SAVESTA(5),SORTREC+SSTAT1        SAVE STATION                    
         XC    KEY,KEY             ESTABLISH STATION KEY                        
         MVI   KEY,X'02'                                                        
         MVC   KEY+20,RCREPFL                                                   
         MVC   KEY+22(5),SAVESTA   INSERT STATION                               
         GOTO1 HIGH1                                                            
         CLC   KEY+20(2),RCREPFL   RECORD FOUND?                                
         BNE   SGDUMP              NOT FOUND - DUMP IT OUT                      
         CLC   KEY+22(5),SAVESTA                                                
         BE    SG008                                                            
SGDUMP   DC    H'0'                DUMP IT OUT                                  
SG008    EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 =A(LINKFILE),DMCB,(RC),RSTAREC,GETREC                            
         PRINT NOGEN                                                            
         CLC   QUESTOR(5),=C'$GONE' **TEST**                                    
         BNE   GONE0102            **TEST**                                     
         CLC   SAVESTA(4),=C'WBZ ' **TEST**                                     
         BNE   GONE0102            **TEST**                                     
         MVI   STAACTIV,C'Y'       **TEST**                                     
         MVI   STAGON,C'Y'         **TEST**                                     
         B     SG014               **TEST**                                     
GONE0102 EQU   *                                                                
         MVI   STAGON,C'N'         SET 'STATION GONE' FLAG TO 'NO'              
         OC    RSTAEND,RSTAEND     ANY END DATE ENTERED?                        
         BZ    SG010               NO  - ACTIVE                                 
         MVI   STAGON,C'Y'         SET 'STATION GONE' FLAG TO 'YES'             
SG010    EQU   *                                                                
*                                                                               
*   JOINED AND LEFT DATES ARE CHECKED AGAINST THE BILLING RANGE DATES.          
*   IF A JOINED OR LEFT DATE INDICATES THAT LESS THAN 12 MONTHS - 15            
*   DAYS BILLING IS AVAILABLE, THE STATION IS CONSIDERED INACTIVE, AND          
*   NO FIGURES ARE ACCUMULATED FOR IT.                                          
*                                                                               
         CLC   QUESTOR(5),=C'$GONE' **TEST**                                    
         BNE   GONE0104            **TEST**                                     
         CLC   SAVESTA(4),=C'WMAQ' **TEST**                                     
         BNE   GONE0104            **TEST**                                     
         MVI   STAACTIV,C'N'       **TEST**                                     
         MVI   STAGON,C'N'         **TEST**                                     
         B     SG098               **TEST**                                     
GONE0104 EQU   *                                                                
         MVI   STAACTIV,C'Y'       SET STATION ACTIVE FLAG ON                   
         CLC   RSTASTRT,JOINSTDT   STARTED AFTER JOIN DATE: INACTIVE            
         BH    SG012               HIGH = INACTIVE                              
         OC    RSTAEND,RSTAEND     ANY END DATE ENTERED?                        
         BZ    SG014               NO  - ACTIVE                                 
         CLC   RSTAEND,JOINENDT    LEFT BEFORE END DATE: INACTIVE               
         BH    SG014               HIGH = ACTIVE                                
SG012    EQU   *                                                                
         MVI   STAACTIV,C'N'       INACTIVE: SET FLAG OFF                       
SG014    EQU   *                                                                
         CLI   STAACTIV,C'N'       STATION INACTIVE?                            
         BE    SG098                                                            
         GOTO1 =A(OFFTOT),DMCB,(RC)             ROLL FIGS INTO OFFICES          
         MVI   SORTREC,C'1'                     SET STATION MAJOR TYPE          
         MVI   SORTREC+STYP2,C'1'               SET SUB-TYPE                    
         MVC   SORTREC+SCURR(48),TMONTBL        INSERT CURRENT BEST $           
         MVC   SORTREC+SSTALEFT(1),STAGON        SET 'STATN GONE' FLAG          
         CLC   QUESTOR(8),=C'**TEST**'                                          
         BNE   SG018                                                            
         MVC   P+1(12),=C'1ST SORTREC:'                                         
         MVC   P+20(20),SORTREC                                                 
         MVC   P+44(12),WORKX+96   **TEST**                                     
         GOTO1 REPORT                                                           
SG018    EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   RESETREC,SORTREC                 SAVE FOR RESETTING              
         MVI   SORTREC,C'2'                     SET OFFICE MAJOR TYPE           
         MVC   SORTREC+SGROUP(2),SPACES         NO GRP FOR OFF ORDER            
         MVC   SWAPSTA(5),SORTREC+SSTAT1        SWITCH STATION/OFFICE           
         MVC   SORTREC+SOFF2(2),SORTREC+SOFF1   MOVE OFFICE MAJOR               
         MVC   SORTREC+SSTAT2(5),SWAPSTA        MOVE STATION MINOR              
         CLC   QUESTOR(8),=C'**TEST**'                                          
         BNE   SG022                                                            
         MVC   P+1(12),=C'2ND SORTREC:'                                         
         MVC   P+20(20),SORTREC                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C' - - - - - -'                                         
         GOTO1 REPORT                                                           
SG022    EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   SORTREC,RESETREC           RESET TYPE 1 SORT RECORD              
         AP    SORTCTR,=P'2'              ADD TO SORTED RECORDS                 
SG098    EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESTORE CONTRACT KEY                         
         GOTO1 READ1               REESTABLISH CONTRACT RECORD                  
SG099    EQU   *                                                                
         XC    TMONTBL,TMONTBL     RESET TABLE IN ALL CASES                     
         XIT1                                                                   
         EJECT                                                                  
READ1    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY1                                                         
*                                                                               
HIGH1    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY1                                                         
*                                                                               
DIRCTRY1 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         ORG   RE1C02+4096                                                      
*                                                                               
*  WORK AREA IS ORG'D TO RE1C02+4096 TO BE COVERED BY SECOND                    
*   AND THIRD BASE REGISTERS                                                    
*                                                                               
CARDMISS DC    C'SECOND REQUEST CARD MISSING'                                   
*                                                                               
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
         SPACE 3                                                                
WEEKARR  DS    0XL24               WEEKS/MONTH ARRAY                            
BILLARR  DS    XL12'00'                                                         
ENDBILL  DS    0CL4                END OF ARRAY ADDRESS                         
BUDGARR  DS    XL12'00'                                                         
ENDBUDG  DS    0CL4                END OF ARRAY ADDRESS                         
         SPACE 3                                                                
DATEWORK DS    0XL8                                                             
YF       DC    PL2'0'              YEAR-FROM                                    
MF       DC    PL2'0'              MONTH-FROM                                   
YT       DC    PL2'0'              YEAR-TO                                      
MT       DC    PL2'0'              MONTH-TO                                     
DTWK2    DC    CL6'    01'         GETBROAD CALL AREA                           
         SPACE 3                                                                
ASTART   DS    F                                                                
CONFLAG  DS    CL1                                                              
STAACTIV DC    CL1'N'                                                           
STAGON   DC    CL1'N'              STATION GONE FLAG                            
NEWELEM  DC    CL1'N'              INSERT NEW ELEMENT FLAG                      
MONPCTS  DC    CL1'Y'              MONTHLY %S EXIST FLAG                        
JOINFLAG DC    XL1'00'                                                          
SAVCONTP DS    CL1                 CONTRACT TYPE SAVE AREA                      
STARTMO  DS    XL1                 FISCAL YEAR START MONTH                      
SAVESTDT DC    XL3'0'              STATION JOINED DATE                          
ALLOCDTE DS    XL3                 ALLOCATION DATE                              
BILSTDTE DS    XL3                 BUDGET PERIOD BEGIN BILLING DATE             
BUDSTDTE DS    XL3                 BILLING PERIOD START DATE                    
DBUDFRM  DS    CL6                 DISPLAY BUDGET START DATE                    
DBUDTO   DS    CL6                 DISPLAY BUDGET END DATE                      
BUKSTART DC    XL1'00'             BUCKET OFFSET: BUDGET VS BILLING             
PERCENTS DC    12H'00'             PERCENTS BY MONTH                            
SETDT    DC    CL6'    01'         BUDGET PERIOD START DATE AREA                
SPREDCT  DC    XL10'40004000400040004000'  CON TYPES/PERCENTAGES                
JOINSTDT DC    XL3'0'              DATE JOINED CUTOFF                           
JOINENDT DC    XL3'0'              DATE LEFT   CUTOFF                           
SETJOIN  DS    CL1                                                              
*                                                                               
SORTREC  DS    0CL125                                                           
STYP     DS    CL1                                                              
         DS    CL9                                                              
SSUBTYP  DS    CL1                                                              
SCURDOL  DS    12CL4               CURRT YR BEST DOLLAR BY MONTH                
SALLDOL  DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAME DS    CL20                MARKET NAME                                  
SOFFNAME DS    CL20                OFFICE NAME                                  
SNOJOIN  DS    CL1                 LATE JOIN FLAG                               
SSTAGON  DS    CL1                 STATION GONE FLAG                            
         DS    CL20                SPARE                                        
*                                                                               
*  KEY BUILDING EQUATES:  DISPLACEMENTS INTO SORTREC                            
*                                                                               
SGROUP   EQU   1                                                                
SSTAT1   EQU   3                   STATION MAJOR                                
SOFF1    EQU   8                   OFFICE MINOR                                 
SOFF2    EQU   3                   OFFICE MAJOR                                 
SSTAT2   EQU   5                   STATION MINOR                                
STYP2    EQU   10                  SUBTYPE FOR BUDGET RECORDS                   
SCURR    EQU   11                                                               
SALL$    EQU   59                  ALLOCATED DOLLARS DISPLACEMENT               
SMKTNM   EQU   63                                                               
SOFFNM   EQU   83                                                               
SNOJN    EQU   103                                                              
SSTALEFT EQU   104                 STATION LEFT                                 
*                                                                               
SORTREC2 DS    0CL125              SAVED SORTKEY                                
SRECTYP2 DS    CL1                                                              
         DS    CL9                                                              
SSUBTYP2 DS    CL1                                                              
SCURDOL2 DS    12CL4               CURRT YR BEST DOLLAR BY MONTH                
SALLDOL2 DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAM2 DS    CL20                                                             
SOFFNAM2 DS    CL20                                                             
SNOJOIN2 DS    CL1                 JOINED LATE FLAG                             
SSTAGON2 DS    CL1                 STATION GONE                                 
         DS    CL20                SPARE                                        
*                                                                               
RESETREC DS    CL125                                                            
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
SAVESTA  DC    CL5'     '          SAVED STATION CALLS                          
SAVEGRUP DC    CL2'  '             SAVED GROUP CODE                             
SAVEALL$ DC    XL4'0'              SAVED ALLOCATION DOLLARS                     
SAVEMKT  DS    CL20                SAVE MARKET NAME                             
SAVEOFF  DS    CL20                SAVE OFFICE NAME                             
*                                                                               
PROCCTR  DC    PL4'0'              CONTRACTS PROCESSED CTR                      
SORTCTR  DC    PL4'0'              RECORDS RELEASED TO SORT                     
RETCTR   DC    PL4'0'              RECORDS RETURNED FROM SORT                   
BUDCTR   DC    PL4'0'              BUDGET SORT RECS OUTPUT                      
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
SWAPSTA  DS    CL5                 STATION/OFFICE SWAP AREA                     
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
ACTIVE   DS    CL1                                                              
ELCODE   DS    CL1                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
WORKX    DS    CL150               ELEMENT BUILD AREA                           
WORKY    DS    CL50                ELEMENT BUILD AREA                           
SORTCARD DC    CL80'SORT FIELDS=(1,11,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=125'                                   
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
*                                                                               
         DC    C'**CARD2**'                                                     
CARD2    DS    0CL80               2ND REQUEST CARD                             
CBUDFRM  DS    CL6                 'BUDGET-FROM' DATE                           
CBUDTO   DS    CL6                 'BUDGET-TO' DATE                             
CBUDCTYP DS    5CL4                CONTRACT TYPE/PERCENTAGE                     
         DS    CL48                                                             
CARD2L   EQU   *-CARD2                                                          
*                                                                               
         DS    0F                                                               
HUNDRED  DC    F'100'                                                           
HUND25   DC    H'125'                                                           
EIGHTY   DC    H'80'                                                            
FIFTY    DC    H'50'                                                            
ADJUST   DS    H                                                                
*                                                                               
         DS    0F                                                               
ALLOC$   DS    XL4                                                              
         DS    0D                                                               
CONTOTS  DS    0CL16                                                            
         DC    4F'0'                                                            
TOTYEAR  DS    XL4                 YEAR'S TOTAL DOLLARS                         
TMONTBL  DS    0CL48                                                            
         DC    12F'00'             BUCKET ACCUMULATOR                           
         DS    0F                  ALIGNMENT (JIC)                              
NJSTATS  DS    250CL8              250 8-BYTE ENTRIES                           
*                                  BYTES 1-5    - STATION CALLS                 
*                                  BYTES 6-7    - OFFICE                        
*                                  BYTE  8      - CONTRACT TYPE                 
*      BYTES 6-7 ARE ONLY USED IF STATION/OFFICE IS                             
*      OUT OF BALANCE.  OTHERWISE, FIELD IS BINARY ZERO                         
*      BYTE 8 IS USED TO INDICATE IF STATION IF NOT-JOINED/LEFT,                
*      NO LONGER REPPED, OR BOTH.                                               
*                                                                               
*                                                                               
LNJSTATS EQU   *-NJSTATS                                                        
ENJSTATS DC    CL5'ZZZZZ'                                                       
ANJSTATS DS    F                   A(NEXT AVAILABLE ENTRY)                      
*                                                                               
*   TOTALS TABLES:                                                              
*      11 SETS OF BUCKETS:  5 CONTRACT TYPES MAX AT BREAK  LEVEL +              
*                           5 CONTRACT TYPES MAX AT REPORT LEVEL +              
*                           1 GRAND TOTAL SET                                   
*      EACH SET OF BUCKETS CONSISTS OF:                                         
*         BYTES   1 - 4    =   CONTROL                                          
*           BYTE  1        =   CONTRACT TYPE                                    
*           BYTES 2 - 4    =   UNUSED AT THIS TIME                              
*           BYTES 5 - 8    =   GROSS ALLOCATION DOLLARS                         
*         BYTES   9 - 56   =   12 X 4 BYTES MONTHLY BUCKETS                     
*                                                                               
         DC    CL12'**TOTTABLE**'                                               
         DS    0F                  ALIGNMENT (JIC)                              
TOTTABLE DS    14F                                                              
         ORG   TOTTABLE                                                         
TOTCONTP DS    CL1                 CONTRACT TYPE                                
         DS    CL3                 UNUSED AT THIS TIME                          
TOTALLOC DS    F                   GROSS ALLOCATION DOLLARS                     
TOTMON1  DS    F                   FIRST MONTH'S DATA                           
         DS    11F                 NEXT 11 MONTH'S DATA                         
LTOTTABL EQU   *-TOTTABLE          L(SINGLE ENTRY)                              
         DS    (LTOTTABL*10)X      NEXT TEN SETS OF BUCKETS                     
LTOTTOT  EQU   *-TOTTABLE          L(ENTIRE TABLE)                              
         DC    XL4'FFFFFFFF'       DELIMITER FOR TABLE                          
*                                                                               
*  12 MONTHLY BUCKETS -                                                         
*                                                                               
*  ALLOCATION SPREADER ACCUMULATORS:  USED TO ADD UP                            
*     TOTALS BY OFFICE, AS WELL AS OVERALL TOTALS.  IF A STATION                
*     WAS NOT JOINED AT THE START OF THE BILLING CYCLE, THE COMPANY             
*     TOTALS FOR THE MONTH WILL BE USED AS THE ALLOCATION SPREAD.               
*     USED AS THE ALLOCATION SPREAD.  IN ALL OTHER CASES, THE OFFICE            
*     FIGURES WILL BE USED AS THE ALLOCATION SPREAD.                            
*                                                                               
*  THIRTEEN 4-BYTE FIELDS WILL BE USED PER OFFICE.  A MAX OF 50 OFFICES         
*     IS PROVIDED FOR.                                                          
*        FIELD 1     =   BYTES 1-2  - OFFICE CODE                               
*                        BYTES 3-4  - NOT USED AT THIS TIME                     
*        FIELD 2-13  =   ACCUMULATORS MONTH 1 - 12                              
*                                                                               
*    FIRST ENTRY WILL BE COMPANY TOTALS                                         
*    NEXT 50 ENTRIES WILL BE OFFICE TOTALS                                      
*                                                                               
         DC    CL8'**ATBL**'                                                    
         DS    0F                  ALIGNMENT (JIC)                              
ACCTBL   DS    13F                                                              
         ORG ACCTBL                                                             
ACCOFF   DS    CL2                 OFFICE CODE (X'00' FOR COMPANY)              
         DS    CL2                 UNUSED TWO BYTES                             
ACCMON1  DS    F                   FIRST MONTH'S DATA                           
         DS    11F                 NEXT 11 MONTH'S DATA                         
LACCTBL  EQU   *-ACCTBL            L(SINGLE ENTRY)                              
         DS    (LACCTBL*30)X       THIRTY OFFICE ENTRIES                        
*                                                                               
*  INCREASE THE MULTIPLICATION FACTOR ABOVE IF MORE OFFICES ARE NEEDED          
*                                                                               
LACCOFF  EQU   *-ACCOFF                                                         
         DC    XL4'FFFFFFFF'       DELIMITER FOR TABLE                          
         ORG                                                                    
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*             SORT FILE                                                         
*             COMPARE SORTED DATA KEYS VS BUDGET RECORDS ON FILE                
*             PRODUCE REPORT                                                    
*                                                                               
*                                                                               
         DS    0H                                                               
RPTDONE  NMOD1 0,*RPTDON*                                                       
         L     RC,0(R1)            RESET A(WORK SPACE)                          
         XC    SAVESTA,SAVESTA     INIT STATION SAVE AREA                       
         XC    KEY,KEY             GET REP RECORD FOR FISCAL MONTH              
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGH                                                             
         GOTO1 =A(LINKFILE),DMCB,(RC),RREPREC,GETREC                            
         MVC   STARTMO,RREPFMON    TAKE OUT START MONTH                         
*                                                                               
         XC    KEY,KEY             ESTABLISH FIRST BUDGET KEY                   
         MVI   KEY,X'13'                                                        
         MVC   KEY+16(2),RCREPFL                                                
         MVC   KEY+18(2),QRGPROG   LOAD START YEAR                              
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RPDO0004                                                         
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RPDO0004                                                         
         MVC   KEY+20(5),QSTATION  YES - LOAD INTO KEY                          
*                                                                               
RPDO0004 GOTO1 HIGH                                                             
*                                                                               
RPDO0008 CLI   KEY,X'13'           SAME CODE?                                   
         BNE   RPDO0099            NO  - DONE EXTRACTING                        
         CLC   KEY+16(2),RCREPFL   SAME REP?                                    
         BNE   RPDO0099            NO  - DONE EXTRACTING                        
         CLC   KEY+18(2),QRGPROG   SAME YEAR?                                   
         BNE   RPDO0099            NO  - DONE EXTRACTING                        
         CLC   =C'C*MP',KEY+20     SPECIAL COMPANY BUDGET?                      
         BE    RPDO0056            YES - SKIP IT                                
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RPDO0012                                                         
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RPDO0012                                                         
         CLC   KEY+20(5),QSTATION  SAME STATION?                                
         BNE   RPDO0099            NO  - DONE EXTRACTING                        
*                                                                               
RPDO0012 EQU   *                                                                
         OC    QOFFICE,QOFFICE     ANY OFFICE ENTERED?                          
         BZ    RPDO0016                                                         
         CLC   QOFFICE(2),SPACES   DITTO LAST COMMENT                           
         BE    RPDO0016                                                         
         CLC   KEY+25(2),QOFFICE   SAME OFFICE?                                 
         BNE   RPDO0056            NO  - SKIP THIS RECORD                       
RPDO0016 EQU   *                                                                
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         CLC   KEY+20(5),SAVESTA   STATION PREVIOUSLY SEEN?                     
         BE    RPDO0028            YES - SKIP REREADING IT                      
         MVC   SAVESTA(5),KEY+20   SAVE STATION                                 
*                                                                               
*   RETRIEVE STATION RECORD FOR STATION NAME                                    
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION KEY                        
         MVI   KEY,X'02'                                                        
         MVC   KEY+20,RCREPFL                                                   
         MVC   KEY+22(5),SAVESTA   INSERT STATION                               
         GOTO1 HIGH                                                             
         CLC   KEY+20(2),RCREPFL   RECORD FOUND                                 
         BNE   RPDODUMP            NOT FOUND - DUMP IT OUT                      
         CLC   KEY+22(5),SAVESTA                                                
         BNE   RPDODUMP                                                         
         PRINT GEN                                                              
         GOTO1 =A(LINKFILE),DMCB,(RC),RSTAREC,GETREC                            
         PRINT NOGEN                                                            
         MVC   SAVEGRUP,RSTAGRUP   SAVE GROUP FROM STATION RECORD               
         MVC   SAVEMKT,RSTAMKT     SAVE MARKET NAME                             
         MVC   SAVESTDT,RSTASTRT   SAVE DATE JOINED                             
         CLC   QUESTOR(5),=C'$GONE' **TEST**                                    
         BNE   GONE0002            **TEST**                                     
         CLC   SAVESTA(4),=C'WBZ ' **TEST**                                     
         BNE   GONE0002            **TEST**                                     
         MVI   STAACTIV,C'Y'       **TEST**                                     
         MVI   STAGON,C'Y'         **TEST**                                     
         B     RPDO0028            **TEST**                                     
GONE0002 EQU   *                                                                
         MVI   STAGON,C'N'         SET 'STATION GONE' TO 'NO'                   
         OC    RSTAEND,RSTAEND     ANY END DATE ENTERED?                        
         BZ    RPDO0020            NO  -                                        
         MVI   STAGON,C'Y'         YES - SET 'STATION GONE' TO 'YES'            
RPDO0020 EQU   *                                                                
         CLC   QUESTOR(5),=C'$GONE' **TEST**                                    
         BNE   GONE0004            **TEST**                                     
         CLC   SAVESTA(4),=C'WMAQ' **TEST**                                     
         BNE   GONE0004            **TEST**                                     
         MVI   STAACTIV,C'N'       **TEST**                                     
         MVI   STAGON,C'N'         **TEST**                                     
         B     RPDO0028            **TEST**                                     
GONE0004 EQU   *                                                                
         MVI   STAACTIV,C'Y'       SET STATION ACTIVE FLAG ON                   
*                                                                               
*   JOINED AND LEFT DATES ARE CHECKED AGAINST THE BILLING RANGE DATES.          
*   IF A JOINED OR LEFT DATE INDICATES THAT LESS THAN 12 MONTHS - 15            
*   DAYS BILLING IS AVAILABLE, THE STATION IS CONSIDERED INACTIVE, AND          
*   NO FIGURES ARE DRAWN.                                                       
*                                                                               
         CLC   RSTASTRT,JOINSTDT   STARTED AFTER JOIN DATE: INACTIVE            
         BH    RPDO0024            HIGH = INACTIVE                              
         OC    RSTAEND,RSTAEND     ANY END DATE ENTERED?                        
         BZ    RPDO0028            NO  - ACTIVE                                 
         CLC   RSTAEND,JOINENDT    LEFT BEFORE END DATE: INACTIVE               
         BH    RPDO0028            HIGH = ACTIVE                                
RPDO0024 EQU   *                                                                
         MVI   STAACTIV,C'N'       YES - SET FLAG OFF                           
RPDO0028 EQU   *                                                                
*                                                                               
         CLC   QUESTOR(6),=C'$NOBUD'   **TEST**                                 
         BNE   BUDTST99                **TEST**                                 
         CLC   SAVESTA(4),=C'WBZ '     **TEST**                                 
         BE    RPDO0052                **TEST**                                 
BUDTST99 EQU   *                                                                
*                                                                               
         CLC   QGROUP(1),SPACES    ANY GROUP FILTER?                            
         BE    RPDO0032            NO  - SKIP TEST                              
         CLC   QGROUP(1),RSTAGRUP  YES - BUDGET FOR GROUP?                      
         BNE   RPDO0052            NO  - NO SORT OUTPUT                         
RPDO0032 EQU   *                                                                
         CLC   QSBGROUP(1),SPACES  ANY SUBGROUP FILTER?                         
         BE    RPDO0036            NO  - SKIP TEST                              
         CLC   QSBGROUP(1),RSTAGRUP+1 YES - BUDGET FOR SUBGROUP?                
         BNE   RPDO0052            NO  - NO SORT OUTPUT                         
*                                                                               
*  RETRIEVE OFFICE RECORD FOR OFFICE NAME: OFFICE IS MINOR, MUST BE             
*        READ FOR EACH BUDGET RECORD PROCESSED                                  
*                                                                               
RPDO0036 EQU   *                                                                
         XC    KEY,KEY                   ESTABLISH OFFICE KEY                   
         CLC   KEYSAV2+25(2),=X'0000'    ANY OFFICE ENTERED?                    
         BNE   RPDO0040                                                         
         MVC   SAVEOFF(20),SPACES                                               
         MVC   SAVEOFF(09),=C'CORPORATE '                                       
         B     RPDO0044                                                         
RPDO0040 EQU   *                                                                
         MVI   KEY,X'04'                                                        
         MVC   KEY+23,RCREPFL                                                   
         MVC   KEY+25(2),KEYSAV2+25       INSERT OFFICE FROM BUD REC            
         GOTO1 HIGH                                                             
         CLC   KEY+23(2),RCREPFL          RECORD FOUND                          
         BNE   RPDODUMP                   NOT FOUND - DUMP IT OUT               
         CLC   KEY+25(2),KEYSAV2+25                                             
         BNE   RPDODUMP                                                         
         GOTO1 =A(LINKFILE),DMCB,(RC),ROFFREC,GETREC                            
         MVC   SAVEOFF,ROFFNAME           SAVE OFFICE NAME                      
*                                                                               
RPDO0044 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         MVI   SORTREC,C'1'                                                     
         MVC   SORTREC+SGROUP(2),SAVEGRUP                                       
         MVC   SORTREC+SSTAT1(5),SAVESTA                                        
         MVC   SORTREC+SOFF1(2),KEYSAV2+25    OFFICE FROM BGT REC               
         MVC   SORTREC+SMKTNM(20),SAVEMKT                                       
         MVC   SORTREC+SOFFNM(20),SAVEOFF                                       
         MVI   SORTREC+STYP2,C'0'             SETS SUBTYP TO X'F0'              
         MVC   SORTREC+SSTALEFT(1),STAGON     SET 'STATION LEFT' FLAG           
         CLI   STAACTIV,C'Y'                  STATION ACTIVE?                   
         BE    RPDO0048                       YES, ACTIVE.                      
         MVI   SNOJOIN,C'1'                   SET 'NOT JOINED' FLAG             
RPDO0048 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'2'                                                     
         MVC   SORTREC+SGROUP(2),SPACES       NO GRP FOR OFF ORDER              
         MVC   SORTREC+SSTAT2(5),SAVESTA      SWAP KEYS FOR TYPE 2              
         MVC   SORTREC+SOFF2(2),KEYSAV2+25                                      
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    BUDCTR,=P'2'                   ADD BGT RECS GEN'D                
RPDO0052 EQU   *                                                                
         MVC   KEY(27),KEYSAV2                RESTORE BUDGET KEY                
         GOTO1 READ                                                             
RPDO0056 EQU   *                                                                
         GOTO1 SEQ                 REESTABLISH SEQUENTIAL ACCESSING             
         B     RPDO0008            PROCESS THIS READ                            
*                                                                               
RPDODUMP DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
RPDO0099 EQU   *                                                                
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
*   THE SORTED FILE WILL CONTAIN UP TO 2 RECORDS FOR EACH                       
*     GROUP/STATION/OFFICE:  RULES:                                             
*        1.  RECORD 1: BUDGET RECORD, SUBTYPE OF 0.                             
*        2.  RECORD 2: CONTRACT $,    SUBTYPE OF 1.                             
*        3.  IF BOTH RECORDS PRESENT, ALLOCATION WILL BE SPREAD.                
*                                                                               
*  NOTE:  ALL SPREADS ARE NOW DONE FROM OFFICE FIGURES!!                        
*                                                                               
*        4.  BUDGET ALONE: IF 'NOT JOINED' (SEE BELOW FOR DEFINITION)           
*               PERCENTS WILL BE DERIVED FROM COMPANY FIGURES, ELSE             
*               SKIPPED                                                         
*        5.  CONTRACT $ ALONE:  WILL BE IGNORED.                                
*        6.  'NOT JOINED' STATIONS WILL BE FLAGGED                              
*        7.  'NOT JOINED' DEFINITION:  A JOINED OR LEFT DATE FOR THE            
*               STATION RESULTS IN LESS THAN 11.5 MONTHS BILLING.               
*                                                                               
         XC    SORTREC2,SORTREC2                                                
         MVI   SUBPRG,0            YES - SET INCREMENT                          
         CLI   QOPTION1,C'O'       OFFICE REPORT ONLY?                          
         BNE   REDA0100            NO  -                                        
         MVC   P+34(LNOFFSTN),NOFFSTN                                           
         GOTO1 REPORT                                                           
         B     REDA0100                                                         
*                                                                               
NOFFSTN  DC    C'**NO OFFICE WITHIN STATION REPORT '                            
         DC    C'WILL BE PRODUCED BY THIS RUN**'                                
LNOFFSTN EQU   *-NOFFSTN                                                        
*                                                                               
REDA0100 EQU   *                                                                
         GOTO1 =A(GETSORT),DMCB,(RC)                                            
         CLI   STYP,X'FF'          EOF                                          
         BE    REDA0144            YES                                          
         CLI   SSUBTYP,C'0'        CONTRACT OR BUDGET?                          
         BNE   REDA0104            CONTRACT - SET NOTHING                       
         MVI   SETJOIN,C'Y'        SET JOINED TO YES                            
         CLI   SNOJOIN,C'1'        1 = NOT JOINED OR LEFT                       
         BNE   REDA0104            STILL WITH US                                
         MVI   SETJOIN,C'N'        SET JOINED TO NO                             
REDA0104 EQU   *                                                                
         OC    SORTREC2,SORTREC2                                                
         BZ    REDA0128            FIRST TIME IF ALL ZERO                       
         CLC   STYP,SRECTYP2       SAME RECORD TYPE?                            
         BE    REDA0108            YES                                          
         MVC   RESETREC,SORTREC         SWAP RECORDS FOR HEADINGS               
         MVC   SORTREC,SORTREC2                                                 
         GOTO1 =A(DUMPTOTS),DMCB,(RC)   NO  - DUMP TOTALS                       
         MVC   SORTREC,SPACES                 NO INFO FOR HDGS                  
         GOTO1 =A(GRNDTOTS),DMCB,(RC)         DUMP GRAND TOTALS                 
         MVC   SORTREC,RESETREC         RESET CORRECT RECORD                    
         MVI   FORCEHED,C'Y'            FORCE HEADING BREAK                     
         B     REDA0128                                                         
*                                                                               
REDA0108 EQU   *                                                                
         CLC   SORTREC+SGROUP(2),SORTREC2+SGROUP                                
         BE    REDA0116                                                         
         CLI   STYP,C'1'                STATION/OFFICE REPORT?                  
         BNE   REDA0112                 NO  - OFFICE/STATION REPORT             
         GOTO1 =A(DUMPTOTS),DMCB,(RC)   YES - DUMP TOTALS                       
REDA0112 EQU   *                                                                
         MVI   FORCEHED,C'Y'                                                    
         B     REDA0124                                                         
*                                                                               
REDA0116 EQU   *                                                                
         CLI   STYP,C'1'                                                        
         BNE   REDA0120                                                         
         CLC   SORTREC+SSTAT1(5),SORTREC2+SSTAT1                                
         BE    REDA0124                                                         
         GOTO1 =A(DUMPTOTS),DMCB,(RC)                                           
         MVI   FORCEHED,C'Y'                                                    
         B     REDA0124                                                         
*                                                                               
REDA0120 EQU   *                                                                
         CLC   SORTREC+SOFF2(2),SORTREC2+SOFF2                                  
         BE    REDA0124                                                         
         GOTO1 =A(DUMPTOTS),DMCB,(RC)                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
REDA0124 CLC   SORTREC+1(9),SORTREC2+1  SAME GROUP/STATION/OFFICE?              
         BNE   REDA0128                 NO                                      
*                                                                               
*   YES - IT IS A CONTRACT $ RECORD.                                            
*                                                                               
*                                                                               
*  FOLLOWING INSTRUCTION CALCULATES SPREAD FROM STATION BILLING.                
*    THIS IS CHANGED TO CALCULATE SPREAD FROM OFFICE BILLING                    
*                                                                               
*****>   LA    R1,SCURDOL               A(MONTHLY BILL$) FROM RECORD            
*                                                                               
         BAS   RE,FINDOFFC         FIND OFFICE BILLING                          
*                                  USE RETURNED ADDRESS                         
*                                                                               
         L     R3,DUB              SET RETURNED ADDRESS                         
         PRINT GEN                                                              
         GOTO1 =A(ALLSPRED),DMCB,(RC),(R3)     SPREAD                           
         PRINT NOGEN                                                            
         B     REDA0140                                                         
*                                                                               
*   REDA128:  A BREAK IN GRP/STA/OFF ENCOUNTERED, MEANING THAT PRIOR            
*        ENTRY SHOULD BE WRAPPED UP, IF NEED BE                                 
*                                                                               
REDA0128 EQU   *                                                                
         CLI   SSUBTYP,C'0'        CONTRACT OR BUDGET?                          
         BNE   REDA0132            CONTRACT:  NO BUDGET FOR IT                  
         BAS   RE,COMMPRNT         LOAD COMMON FIELDS                           
         ZIC   R3,SUBPRG           COMPUTE REPORT HEADING #                     
         ZIC   R6,RCSUBPRG                                                      
         AR    R3,R6                                                            
         STC   R3,RCSUBPRG         RESET HEADER CONTROL                         
         CLI   SETJOIN,C'N'        BUDGET: NOT JOINED OR LEFT?                  
         BNE   REDA0140            C'N' = NOT JOINED OR LEFT                    
         BAS   RE,FINDOFFC         FIND OFFICE FIGURES FOR SPREAD               
         L     R3,DUB              SET RETURNED ADDRESS                         
         GOTO1 =A(ALLSPRED),DMCB,(RC),(R3)  SPREAD OFFICE FIGURES               
         B     REDA0140                                                         
REDA0132 EQU   *                                                                
         CLI   STYP,C'1'           STATION/OFFICE REPORT?                       
         BNE   REDA0136            NO  - ONLY ENTER IN THAT REPORT              
         GOTO1 =A(NOBUDDOL),DMCB,(RC) ADD TO EXCEPTION LIST                     
REDA0136 EQU   *                                                                
         MVC   P,SPACES            SPACE PRINT-LINE TO CLEAR                    
         MVI   FORCEHED,C'N'       TURN OFF PAGEBREAK                           
         B     REDA0100            SKIP SAVING KEY                              
*                                                                               
REDA0140 MVC   SORTREC2,SORTREC    SAVE PROCESSED KEY                           
         B     REDA0100                                                         
*                                                                               
REDA0144 EQU   *                                                                
         GOTO1 =A(DUMPTOTS),DMCB,(RC)                                           
         MVC   SORTREC,SPACES                 NO INFO FOR HDGS                  
         GOTO1 =A(GRNDTOTS),DMCB,(RC)         DUMP GRAND TOTALS                 
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   RCSUBPRG,X'6'       SET NEW HEADER CONTROL                       
         BAS   RE,JOINLEFT         PRINT NOT-JOINED LIST                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DISPLAY ENTRIES IN NOT JOINED/LEFT STATIONS TABLE                           
*        BECAUSE LOADING TABLE STARTS WITH AN ADDRESS ONE                       
*        ENTRY BEFORE THE ACTUAL FIRST POSITION, AN EMPTY                       
*        TABLE REFLECTS THIS SITUATION                                          
*                                                                               
JOINLEFT NTR1                                                                   
         LA    R5,NJSTATS          ANY ENTRIES IN TABLE?                        
         L     R6,ANJSTATS                                                      
         CR    R5,R6               NJSTATS > ANJSTATS = NO ENTRIES              
         BNH   JOLE0004                                                         
         MVC   P+1(41),=C'NO STATIONS/OUT-OF-BALANCE TO BE REPORTED'            
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         B     JOLE0024                                                         
JOLE0004 EQU   *                                                                
         CR    R5,R6               CHECK FOR LAST TABLE ENTRY                   
         BH    JOLE0024                                                         
         MVC   P+20(5),0(R5)       MOVE TABLE ENTRY TO PRINT                    
         CLC   5(2,R5),=X'0000'    ANY OFFICE ENTERED?                          
         BNE   JOLE0012            YES - OUT-OF-BALANCE                         
         TM    7(R5),X'01'         STATION NOT JOINED/LEFT?                     
         BNO   JOLE0008            NO                                           
*                                                                               
*   SET MESSAGE INITIALLY TO 'NEW STATION' - WILL BE OVERLAID IF                
*     LEAVE DATE OF STATION HAS BEEN ENCOUNTERED.                               
*                                                                               
         MVC   P+45(LNOJOIN),NOJOIN  MOVE MESSAGE                               
JOLE0008 EQU   *                                                                
         TM    7(R5),X'02'         STATION NO LONGER REPPED?                    
         BNO   JOLE0020            NO                                           
         MVC   P+45(LNOTREP),NOTREP  MOVE MESSAGE                               
         B     JOLE0020                                                         
JOLE0012 EQU   *                                                                
         MVC   P+27(2),5(R5)       YES - MOVE IT TO PRINT                       
         TM    7(R5),X'04'         NO BUDGET ALLOC FLAG SET?                    
         BNO   JOLE0016            NO                                           
         MVC   P+45(LNOBUD$),NOBUD$ YES - SET MESSAGE                           
         B     JOLE0020                                                         
JOLE0016 EQU   *                                                                
         MVC   P+31(1),7(R5)       MOVE CONTRACT TYPE ALSO                      
         MVC   P+45(LNOBAL),NOBAL  MOVE MESSAGE                                 
JOLE0020 EQU   *                                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R5,L'NJSTATS(R5)    BUMP TO NEXT ENTRY                           
         B     JOLE0004                                                         
*                                                                               
JOLE0024 EQU   *                                                                
         MVC   P+53(25),=C'****  END OF REPORT  ****'                           
         GOTO1 REPORT                                                           
         B     ALSP0099                                                         
NOJOIN   DC    C'NEW STATION                     '                              
LNOJOIN  EQU   *-NOJOIN                                                         
NOTREP   DC    C'STATION NO LONGER REPPED        '                              
LNOTREP  EQU   *-NOTREP                                                         
NOBAL    DC    C'CONTRACT TYPE OUT OF BALANCE        '                          
LNOBAL   EQU   *-NOBAL                                                          
NOBUD$   DC    C'NO BUDGET ALLOCATION ENTERED        '                          
LNOBUD$  EQU   *-NOBUD$                                                         
         DS    0D                                                               
         EJECT                                                                  
*                                                                               
*   ESTABLISH COMMON PRINT FIELDS HERE                                          
*                                                                               
COMMPRNT NTR1                                                                   
         CLI   STYP,C'1'                 STATION/OFFICE REPORT?                 
         BNE   COPR0004                  NO                                     
         MVI   RCSUBPRG,0                SET SUBPRG TO STATION                  
         MVC   P+09(2),SORTREC+SOFF1     YES - SHOW OFFICE                      
         MVC   P+13(15),SORTREC+SOFFNM   SHOW OFFICE NAME                       
         B     COPR0008                                                         
COPR0004 MVC   P+09(5),SORTREC+SSTAT2    NO - SHOW STATION                      
         MVC   P+16(15),SORTREC+SMKTNM   SHOW STATION MARKET                    
         MVI   RCSUBPRG,3                SET SUBPRG TO OFFICE                   
COPR0008 EQU   *                                                                
         MVI   SPACING,2                 ENTIRE REPORT IS DOUBLE SPACED         
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*  ROUTINE FINDS OFFICE IN TABLE, SETS DUB TO THAT ADDRESS TO                   
*    CALCULATE SPREAD FROM OFFICE, RATHER THAN STATION, FIGURES                 
*                                                                               
FINDOFFC NTR1                                                                   
         LA    R1,ACCOFF           A(FIRST TABLE ENTRY - COMPANY)               
         CLI   STYP,C'2'           OFFICE/STATION REPORT?                       
         BE    FIOF0012            YES - DON'T BOTHER LOOKING UP                
FIOF0004 EQU   *                                                                
         LA    R1,LACCTBL(R1)      POINT TO NEXT ENTRY                          
         CLC   =X'0000',0(R1)      EMPTY ENTRY?                                 
         BE    FIOF0008            YES - FORCE COMPANY FIGURES                  
         CLC   =X'FFFF',0(R1)      END OF TABLE?                                
         BE    FIOF0008            YES - FORCE COMPANY FIGURES                  
         CLC   0(2,R1),SORTREC+SOFF1       COMPARE OFFICE CODES                 
         BE    FIOF0012                                                         
         B     FIOF0004            GO BACK FOR NEXT                             
FIOF0008 EQU   *                                                                
         LA    R1,ACCOFF           SET A(COMPANY BUCKETS)                       
FIOF0012 EQU   *                                                                
         LA    R1,4(R1)            SKIP OVER CODE BYTES                         
         ST    R1,DUB              SET RETURN VALUE                             
         XIT1                                                                   
         EJECT                                                                  
READ     MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY                                                          
*                                                                               
ADD      MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY                                                          
*                                                                               
WRT      MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0H                                                               
ALLSPRED NMOD1 0,*ALLSP*                                                        
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         L     R1,4(R1)            RELOAD R1 W/A(ACCUMULATOR)                   
         CLC   0(3,R1),=C'STA'     CALL TO READ STATION RECORD?                 
         BNE   ALSP0004            NO  -                                        
*                                                                               
*   NEED TO ACCESS THE DATAMANAGER FACILITY IN THIS NMOD TO DO I/O              
*                                                                               
         GOTO1 =A(LINKFILE),DMCB,(RC),RSTAREC,GETREC                            
         B     ALSP0099            EXIT                                         
ALSP0004 EQU   *                                                                
         CLI   STYP,C'2'           NO CALCS FOR OFFICE/STATION REPORT           
         BE    ALSP0100                                                         
         BAS   RE,YEARTOTL         CALCULATE STATION/OFFICE TOTAL $             
         BAS   RE,PERCCALC         CALCULATE PERCENTAGES                        
         BAS   RE,GETBUDGT         RETRIEVE BUDGET RECORD                       
         BAS   RE,APPDOLRS         ALLOCATE BUDGET BY CONTRACT TYPE             
ALSP0099 EQU   *                                                                
         LTR   R0,R0               PROBABLY USELESS CONDITION SETTING           
         XIT1                                                                   
*                                                                               
*   ALL CALCULATIONS WERE DONE DURING THE CARD-TYPE 1 PROCESSING.               
*        ONCE CARD-TYPE 2 IS ENCOUNTERED, IT IS NECESSARY TO RETRIEVE           
*        THE BUDGET RECORDS, AND 'FINESSE' THEM INTO THE PRINT LOGIC            
*                                                                               
ALSP0100 EQU   *                                                                
         BAS   RE,GETBUDGT         RETRIEVE BUDGET RECORD                       
         BAS   RE,OFFSTREP         FIND/PRINT CONTRACT TYPE                     
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*  PRINT-OUT OF OFFICE/STATION REPORT                                           
*                                                                               
OFFSTREP NTR1                                                                   
         LA    R1,SPREDCT          A(1ST CONTRACT TYPE REQUESTED)               
         MVI   ELCODE,X'02'                                                     
         LA    R5,4                COUNT NEXT FOUR CONTRACT TYPES               
OFST0004 EQU   *                                                                
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL                                                         
         BE    OFST0008            FOUND A CONTRACT TYPE ELEMENT                
         MVC   P+45(LNCTF),NCTF    NO CON TYPE FOR THIS BUDGET                  
         GOTO1 REPORT                                                           
         B     OFST0020            EXIT                                         
*                                                                               
OFST0008 EQU   *                                                                
         CLC   DCONTYP(1,R6),0(R1) CONTRACT TYPE FOUND?                         
         BE    OFST0012            FOUND - USE ITS ALLOC $                      
         BAS   RE,NEXTEL           NOT FOUND - FIND NEXT                        
         BE    OFST0008                                                         
         MVC   P+33(1),0(R1)       DISPLAY CONTRACT TYPE SOUGHT                 
         MVC   P+40(3),=C'***'     FLAG AS 'NOT FOUND'                          
         GOTO1 REPORT              PRINT LINE                                   
         B     OFST0016                                                         
*                                                                               
OFST0012 EQU   *                                                                
         MVC   P+33(1),0(R1)       MOVE CONTRACT TYPE TO PRINTLINE              
         MVC   SAVCONTP,0(R1)      FOR TOTALLING ROUTINE                        
         L     R3,DALLOC$(R6)      LOAD ALLOCATION $                            
         MVC   CONTOTS(4),DALLOC$(R6) FOR TOTALLING ROUTINE                     
         BAS   RE,BUDELPRT         PRINT OUT ALLOCATION                         
*                                                                               
OFST0016 EQU   *                                                                
         LA    R1,2(R1)            NEXT CONTRACT TYPE                           
         CLI   0(R1),C' '          ANY ENTRY?                                   
         BE    OFST0020            NO  - END PROCESSING                         
         BCT   R5,OFST0004         DO EACH OF THE FIELDS                        
OFST0020 EQU   *                   END OF PRINTOUT OF BUDGET RECD               
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   PRINT OUT THE ELEMENT IN THE BUDGET RECORD.  'FINESSE' THE                  
*        PRINT ROUTINE                                                          
*                                                                               
BUDELPRT NTR1                                                                   
         ST    R6,ASTART           A(START OF ELEMENT)                          
         SR    R2,R2                                                            
         LA    R9,P+38                                                          
         EDIT  (R3),(10,(R9)),COMMAS=YES  PRINT ALLOCATION $                    
         ZIC   R5,STARTMO          CALCULATE FISCAL START                       
         BCTR  R5,0                ZERO RELATIVE                                
         MH    R5,=H'4'            SIZE OF FIELD                                
         LA    R6,2(R6)            A(BUDGET MONTHS IN ELEMENT)                  
         LA    R6,0(R5,R6)         R6=A(STARTING MONTH)                         
         LA    R5,12               DO 12 MONTHS' PRINTOUT                       
         LA    R9,P+50                                                          
         PRINT GEN                                                              
         GOTO1 TOTACCUM,DMCB,(R6)  ACCUMULATE TOTALS BY CON TYPE                
         PRINT NOGEN                                                            
BUEL0004 EQU   *                                                                
         L     R3,0(R6)            AMOUNT IN BUDGET MONTH                       
         AR    R2,R3               ACCUMULATE MONTHLY FIGURES                   
         EDIT  (R3),(10,(R9)),COMMAS=YES  PRINT ALLOCATION $                    
         LA    R9,13(R9)           BUMP PRINT SPACING                           
         CH    R5,=H'7'            SIX ENTRIES ON LINE?                         
         BNE   BUEL0008            NO                                           
         MVI   SPACING,1           SINGLE SPACE AFTER LINE 1                    
         GOTO1 REPORT              PRINT OUT LINE                               
         LA    R9,P+50             RESET FIRST PRINT POSITION                   
BUEL0008 EQU   *                                                                
         LA    R6,4(R6)            BUMP A(BUCKET)                               
         BCT   R5,BUEL0004         DO EACH BUCKET                               
         MVI   SPACING,2           DOUBLE SPACE AFTER LINE 2                    
         GOTO1 REPORT                                                           
         L     R6,ASTART           RESET START OF ELEMENT                       
         L     R3,DALLOC$(R6)      TAKE GROSS ALLOCATION $                      
         CR    R3,R2               GROSS $ VS MONTHLY TOTAL                     
         BE    ALSP0099            = NO PROBLEM                                 
         MVI   SPACING,2                                                        
         MVC   P+45(LNOBAL),NOBAL  NOT = DISPLAY MESSAGE                        
         GOTO1 REPORT                                                           
         B     ALSP0099                                                         
         EJECT                                                                  
YEARTOTL NTR1                                                                   
*                                                                               
*   ACCUMULATE YEAR'S TOTAL FOR STATION/OFFICE BY ADDING MONTHS                 
*                                                                               
         LA    R5,12               LOOP CONTROL                                 
         SR    R3,R3                                                            
YETO0004 EQU   *                                                                
         L     R2,0(R1)            GET MONTHLY AMOUNT                           
         AR    R3,R2               ACCUMULATE FOR YEAR                          
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         BCT   R5,YETO0004         DO 12 MONTHS                                 
         ST    R3,TOTYEAR          SAVE TOTAL DOLLARS                           
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   CALCULATE PERCENTAGES FOR MONTHS BASED ON MONTHLY VALUES AND                
*        YEAR'S TOTAL.  CALCULATE TO TWO DECIMAL PLACES                         
*        R1 =  A(MONTHLY BILLING $) OR COMPANY FIGURES IN TABLE                 
*                                                                               
PERCCALC NTR1                                                                   
         MVI   MONPCTS,C'Y'        SET PERCENT FLAG = YES                       
         LA    R5,12               LOOP CONTROL                                 
         LA    R6,PERCENTS         A(PERCENTS BY MONTH)                         
         SR    R9,R9               INITIALIZE COUNTER                           
PECA0004 EQU   *                                                                
         SR    R2,R2                                                            
         L     R3,0(R1)            LOAD DOLLARS TO REGISTER                     
         TM    0(R1),X'80'         IS BUCKET NEGATIVE?                          
         BNO   PECA0008            NO                                           
         SR    R3,R3               TREAT NEGATIVE AS ZERO                       
PECA0008 EQU   *                                                                
         LH    R4,=H'10000'                                                     
         MR    R2,R4               MULTIPLY $ BY 10000                          
         SLDA  R2,1                MULT BY 2                                    
*                                                                               
*   DON'T PERMIT DIVIDE BY ZERO IF NOTHING IN TOTYEAR:  SINGLE                  
*     STATION REQUEST, STATION NOT JOINED/LEFT: NO BILLING                      
*                                                                               
         OC    TOTYEAR,TOTYEAR     ANYTHING THERE?                              
         BZ    PECA0012            NO                                           
         D     R2,TOTYEAR          MONTH*10000/YEAR TOTAL                       
PECA0012 EQU   *                                                                
         LTR   R3,R3               ANSWER NEGATIVE?                             
         BM    *+8                 YES - DON'T ADD 1                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                DIVIDES RESULT BY 2                          
         LTR   R3,R3               TEST R3 AGAIN                                
         BZ    PECA0016            NO VALUE - NO ADD                            
         LA    R9,1(R9)            ADD TO NON-ZERO COUNTER                      
PECA0016 EQU   *                                                                
         STH   R3,0(R6)            SAVE PERCENT TO 2 DEC'L                      
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         LA    R6,2(R6)            BUMP TO NEXT PERCENT                         
         BCT   R5,PECA0004         GO BACK FOR NEXT                             
*                                                                               
*   NOW IT IS NECESSARY TO ADJUST PERCENTS CALCULATED BY 4 WEEK VS              
*       5 WEEK BILLING.  PARALLEL ARRAYS FOR BILLING PERIOD VS BUDGET           
*       PERIOD CONTAIN # OF WEEKS IN BROADCAST MONTH.  ARRAYS ARE               
*       COMPARED AND, WHERE # OF WEEKS IS UNEQUAL, THE PERCENT IS               
*       ADJUSTED EITHER UP OR DOWN                                              
*                                                                               
         LA    R1,BILLARR          A(BILLING ARRAY)                             
         LA    R2,BUDGARR          A(BUDGET ARRAY)                              
         LA    R5,12               COUNTER                                      
         LA    R3,PERCENTS         A(PERCENT ARRAY)                             
PECA0020 EQU   *                                                                
         CLC   0(1,R1),0(R2)       LOOK FOR 4 WK VS 5 WK                        
         BE    PECA0024            EQUAL - NO ADJUSTMENT                        
         BAS   RE,ADJUSTMT         NOT EQUAL:  ADJUST                           
PECA0024 EQU   *                                                                
         LA    R1,1(R1)            BUMP A(BILLING ARRAY)                        
         LA    R2,1(R2)            BUMP A(BUDGET ARRAY)                         
         LA    R3,2(R3)            BUMP A(PERCENT ARRAY) BY HALFWORD            
         BCT   R5,PECA0020         DO EACH ENTRY                                
*                                                                               
*  NOW NEED TO CHECK TOTAL OF PERCENT BUCKETS VS 100.00.  DIFFERENCE            
*        MUST BE SPREAD OVER THE NON-ZERO BUCKETS, OF WHICH R9 HAS              
*        THE COUNT.                                                             
*                                                                               
         BAS   RE,DF250            ADD UP BUCKETS                               
         LH    R5,=H'10000'        CHECK AGAINST 100.00                         
*                                                                               
*  THE DIFFERENCE MAY BE POSITIVE OR NEGATIVE.  RATHER THAN TRYING              
*        TO ESTABLISH A COMPARISON OF A NEGATIVE DIFFERENCE VS A                
*        POSITIVE NUMBER OF STATIONS, THE DIFFERENCE WILL BE                    
*        SPREAD ACROSS ACTIVE MONTHS, EVEN IF THE CALCULATION                   
*        PRODUCES A ZERO.  THE FINAL REMAINDER WILL BE ADDED TO/                
*        SUBTRACTED FROM THE FIRST ACTIVE MONTH.                                
*                                                                               
         SR    R5,R6               CALC DIFF, POS OR NEGATIVE                   
         LTR   R5,R5               TEST FOR NEGATIVE                            
         BM    PECA0028            NEGATIVE - SET R4 ALL X'FF'                  
         SR    R4,R4               POSITIVE - SET R4 TO ZERO                    
         B     PECA0032                                                         
PECA0028 EQU   *                                                                
         L     R4,=F'-1'           SET R4 TO X'FF'                              
PECA0032 EQU   *                                                                
         LTR   R9,R9               DON'T PERMIT DIV BY ZERO                     
         BNZ   PECA0036                                                         
         MVI   MONPCTS,C'N'        SET PERCENT FLAG = NO                        
         B     PECA0064            EXIT                                         
PECA0036 EQU   *                                                                
         DR    R4,R9               DIVIDE DIFF BY # NON-ZERO BUCKETS            
         LA    R1,PERCENTS         A(PERCENT ARRAY)                             
         LA    R4,12               COUNTER                                      
PECA0040 EQU   *                                                                
         LH    R0,0(R1)            LOAD PERCENT                                 
         LTR   R0,R0               ANY VALUE?                                   
         BZ    PECA0052            NO                                           
         LTR   R5,R5               DIFF POS OR NEG?                             
         BNM   PECA0044            DIFF NOT NEGATIVE                            
         LPR   R3,R5               GET POS ABSOLUTE OF DIFF                     
         CR    R0,R3               IS PERCENT .GE. DIFFERENCE?                  
         BNL   PECA0044            YES - USE IT                                 
         SR    R0,R0               NO  - SET IT TO ZERO                         
         B     PECA0048                                                         
PECA0044 EQU   *                                                                
         AR    R0,R5               YES - ADD DIFFERENCE                         
PECA0048 EQU   *                                                                
         STH   R0,0(R1)            RESTORE VALUE                                
PECA0052 EQU   *                                                                
         LA    R1,2(R1)            BUMP TO NEXT PERCENT                         
         BCT   R4,PECA0040         DO EACH BUCKET                               
         BAS   RE,DF250            ADD UP BUCKETS                               
         LH    R5,=H'10000'        CHECK AGAINST 100.00                         
         SR    R5,R6               CALCULATE DIFFERENCE                         
         BZ    PECA0064            NO DIFFERENCE - EXIT                         
*                                                                               
*   REMAINING DIFF WILL BE ADDED INTO THE FIRST NON-ZERO BUCKET                 
*                                                                               
         LA    R1,PERCENTS         A(PERCENT ARRAY)                             
         LA    R4,12               COUNTER                                      
PECA0056 EQU   *                                                                
         LH    R0,0(R1)            LOAD PERCENT                                 
         LTR   R0,R0               ANY VALUE?                                   
         BZ    PECA0060            NO                                           
         AR    R0,R5               YES - ADD DIFFERENCE                         
         STH   R0,0(R1)            RESTORE VALUE                                
         B     PECA0064                                                         
PECA0060 EQU   *                                                                
         LA    R1,2(R1)            BUMP TO NEXT PERCENT                         
         BCT   R4,PECA0056         DO EACH BUCKET                               
PECA0064 EQU   *                                                                
         B     ALSP0099            RETURN                                       
         EJECT                                                                  
*                                                                               
*  ADJUSTMENT:  IF BUDGET=4,BILLING=5, PERCENT*.80                              
*               IF BUDGET=5,BILLING=4, PERCENT*1.25                             
*        R1  =  BILLING ARRAY                                                   
*        R2  =  BUDGET ARRAY                                                    
*        R3  =  PERCENT  (TO TWO DECIMAL PLACES)                                
*                                                                               
ADJUSTMT NTR1                                                                   
         MVC   ADJUST(2),HUND25    PRESET BILLING < BUDGET                      
         CLC   0(1,R1),0(R2)       RETEST 4 WK VS 5 WK                          
         BL    ADJU0004            BILLING < BUDGET                             
         MVC   ADJUST(2),EIGHTY    RESET BILLING > BUDGET                       
ADJU0004 EQU   *                                                                
         SR    R4,R4               FIRST OF REG PAIR                            
         LH    R5,0(R3)            GET PERCENT                                  
         MH    R5,ADJUST           MULTIPLY BY ADJUSTMENT FACTOR                
         AH    R5,FIFTY            HALF-ADD FOR ROUNDING                        
         D     R4,HUNDRED          PERCENT*125+50/100                           
         STH   R5,0(R3)            LOAD AMOUNT BACK                             
         B     ALSP0099            RETURN                                       
         SPACE 5                                                                
*                                                                               
*   BUCKET ACCUMULATOR RETURN VALUE IN R6 - NO NTR1 DONE HERE                   
*                                                                               
DF250    EQU   *                                                                
         LA    R1,PERCENTS         A(PERCENT ARRAY)                             
         LA    R5,12               COUNTER                                      
         SR    R6,R6                                                            
DF251    EQU   *                                                                
         AH    R6,0(R1)            ACCUMULATE BUCKET                            
         LA    R1,2(R1)            BUMP A(BUCKET)                               
         BCT   R5,DF251            GET ALL BUCKETS                              
         BR    RE                  RETURN                                       
         EJECT                                                                  
*                                                                               
*   RETRIEVE BUDGET RECORD FOR UPDATING                                         
*                                                                               
GETBUDGT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'13'                                                        
         MVC   KEY+16,RCREPFL                                                   
         MVC   KEY+18(2),QRGPROG         LOAD BUDGET YEAR                       
         CLI   STYP,C'1'                 STATION/OFFICE REPORT?                 
         BNE   GEBU0004                  NO                                     
         MVC   KEY+20(5),SORTREC+SSTAT1                                         
         MVC   KEY+25(2),SORTREC+SOFF1                                          
         B     GEBU0008                                                         
GEBU0004 EQU   *                                                                
         MVC   KEY+20(5),SORTREC+SSTAT2                                         
         MVC   KEY+25(2),SORTREC+SOFF2                                          
GEBU0008 EQU   *                                                                
         GOTO1 HIGH2                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    GEBU0012                                                         
         DC    H'0'                NOT FOUND - ABORT                            
GEBU0012 EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 =A(LINKFILE),DMCB,(RC),RBUDREC,GETREC                            
         PRINT NOGEN                                                            
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   ROUTINE APPORTIONS DOLLARS IN THIS MANNER:                                  
*     1. ALLOCATION DOLLARS ARE TAKEN FROM FIRST BUDGET CONTRACT                
*            TYPE ON REQUEST                                                    
*     2. EACH CONTRACT TYPE OF THE REQUEST IS PROCESSED INDIVIDUALLY            
*     3. EACH CONTRACT TYPE'S PERCENT OF THE ALLOCATION DOLLARS IS              
*            CALCULATED                                                         
*     4. THE PERCENT OF THE ALLOCATION DOLLARS IS FURTHER APPORTIONED           
*            BY THE MONTHLY PERCENTS DERIVED FROM ACTUAL BILLING                
*     5. IF A CONTRACT TYPE DOES NOT EXIST IN THE BUDGET RECORD, IT             
*            IS ADDED                                                           
*                                                                               
DALLOC$  EQU   RBUD$TOT-RBUDELEM                                                
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
DALLFLG  EQU   RBUDTAG-RBUDELEM                                                 
DBUDTOT  EQU   RBUDSTOT-RBUDELEM                                                
DALLDATE EQU   RBUDADTE-RBUDELE2                                                
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
APPDOLRS NTR1                                                                   
         SR    R3,R3               INITIALIZE ACCUMULATOR                       
         LA    R1,SPREDCT          A(1ST CONTRACT TYPE REQUESTED)               
         MVI   ELCODE,X'02'                                                     
APDO0004 EQU   *                                                                
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL                                                         
         BNE   APDO0016            CHECK FOR TOTAL ALLOCATION                   
*                                                                               
APDO0008 EQU   *                                                                
         CLC   DCONTYP(1,R6),0(R1) CONTRACT TYPE FOUND?                         
         BE    APDO0012            FOUND - USE ITS ALLOC $                      
         BAS   RE,NEXTEL           NOT FOUND - FIND NEXT                        
         BNE   APDO0016            CHECK FOR TOTAL ALLOCATION                   
         B     APDO0008                                                         
*                                                                               
APDO0012 EQU   *                                                                
         A     R3,DALLOC$(R6)      ACCUMULATE ALLOCATION $                      
         LA    R1,2(R1)            SEEK NEXT CON TYPE REQUESTED                 
         CLI   0(R1),C' '          ANY ENTRY?                                   
         BNE   APDO0004            YES - GO BACK FOR NEXT                       
*                                                                               
APDO0016 EQU   *                                                                
         LTR   R3,R3               ANY DOLLARS?                                 
         BZ    APDO0048            NO DOLLARS FOR ALLOCATION                    
         LA    R1,SPREDCT          RESET TO 1ST CONTRACT TYPE                   
         MVC   P+33(1),0(R1)       MOVE CONTRACT TYPE TO PRINTLINE              
         ST    R3,ALLOC$           SAVE ALLOCATION $                            
         CLI   QOPTION3,C'Y'       RESTART?                                     
         BNE   APDO0020            NO                                           
         BAS   RE,FINDRSTT         YES - LOOK FOR RESTART $ BASED               
*                                  ON FIRST CONTRACT TYPE ENTERED               
         B     APDO0024                                                         
APDO0020 EQU   *                                                                
         BAS   RE,RESTARTS         SET CONTYP, ALLOC$ FOR RESTART               
*                                  BASED ON 1ST CONTRACT TYPE ENTERED           
*                                  AND TOTAL ACCUM'D $ FOR ALL TYPES            
APDO0024 EQU   *                                                                
         LA    R5,5                ALLOCATE UP TO 5 CONTRACT TYPES              
*                                                                               
APDO0028 EQU   *                                                                
         CLI   0(R1),C' '          ANY ENTRY?                                   
         BE    APDO0052            NO  - REWRITE RECORD                         
         LA    R6,RBUDREC          SET BACK TO RECORD START                     
         BAS   RE,GETEL            GET FIRST ELEMENT                            
*                                                                               
APDO0032 EQU   *                                                                
         MVI   NEWELEM,C'N'        'INSERT NEW ELEMENT' SET TO NO               
         CLC   DCONTYP(1,R6),0(R1) CONTRACT TYPE FOUND?                         
         BE    APDO0036            FOUND                                        
         BAS   RE,NEXTEL           NOT FOUND - FIND NEXT                        
         BNE   APDO0040            NO ELEMENT FOR ALLOC: CREATE IT              
         B     APDO0032                                                         
*                                                                               
APDO0036 EQU   *                                                                
         MVC   P+33(1),0(R1)       MOVE CONTRACT TYPE TO PRINTLINE              
         BAS   RE,SPREDBUD         SPREAD BUDGET OVER MONTHS                    
         MVC   DALLDATE(2,R6),ALLOCDTE+1                                        
*                                  INSERT DATE ALLOCATED INTO ELEMENT           
         LA    R1,2(R1)            BUMP TO NEXT CONTRACT TYPE                   
         BCT   R5,APDO0028         GO BACK FOR NEXT CARD FIELD                  
         B     APDO0052            REWRITE RECORD ON COMPLETION                 
*                                                                               
APDO0040 EQU   *                                                                
         LA    R6,WORKX            INDICATE WORK AREA                           
         MVC   DCONTYP(1,R6),0(R1) LOAD CONTRACT TYPE                           
         MVC   P+33(1),0(R1)       MOVE CONTRACT TYPE TO PRINTLINE              
         MVI   NEWELEM,C'Y'        'INSERT NEW ELEMENT' SET TO YES              
         MVC   DALLDATE(2,R6),ALLOCDTE+1                                        
*                                  INSERT DATE ALLOCATED INTO ELEMENT           
         OI    DALLDATE(R6),X'80' TURN ON HIGH-ORDER BIT                        
         BAS   RE,SPREDBUD         SPREAD BUDGET OVER MONTHS                    
         LA    R1,2(R1)            BUMP TO NEXT CONTRACT TYPE                   
         BCT   R5,APDO0028         GO BACK FOR NEXT CARD FIELD                  
         B     APDO0052            REWRITE RECORD ON COMPLETION                 
*                                                                               
APDO0044 EQU   *                                                                
         CLI   QOPTION1,C'O'       OFFICE ONLY RUN?                             
         BE    APDO0056            YES - SKIP OUTPUT                            
         MVC   P+45(LNCTF),NCTF    INSERT MESSAGE                               
         MVC   P+63(1),0(R1)       INSERT CONTRACT TYPE                         
         GOTO1 REPORT              PRINT LINE                                   
         B     APDO0056            END OF ROUTINE                               
*                                                                               
NCTF     DC    C'NO CONTRACT TYPE " '                                           
         DC    C'" FOUND FOR THIS STATION/OFFICE: NO SPREAD DONE'               
LNCTF    EQU   *-NCTF                                                           
         DS    0H                                                               
*                                                                               
APDO0048 EQU   *                                                                
         CLI   QOPTION1,C'O'       OFFICE ONLY RUN?                             
         BE    APDO0056            YES - SKIP OUTPUT                            
         MVC   P+45(LNA$F),NA$F    INSERT MESSAGE                               
         GOTO1 REPORT              PRINT LINE                                   
         B     APDO0056            END OF ROUTINE                               
*                                                                               
NA$F     DC    C'NO ALLOCATION DOLLARS'                                         
         DC    C' FOUND FOR THIS STATION/OFFICE: NO SPREAD DONE'                
LNA$F    EQU   *-NA$F                                                           
         DS    0H                                                               
*                                                                               
APDO0052 EQU   *                                                                
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    APDO0056            YES - NO REWRITE OF RECORD                   
         CLI   QOPTION1,C'X'       SPECIAL TEST RUN                             
         BE    APDO0056            YES - NO REWRITE OF RECORD                   
         LA    RF,RBUDREC          REWRITE BUDGET RECORD                        
         ST    RF,AIOAREA                                                       
         GOTO1 =A(LINKFILE),DMCB,(RC),RBUDREC,PUTREC                            
APDO0056 EQU   *                                                                
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   ACTUAL BUDGET SPREADER                                                      
*                                                                               
*        ALLOC$ =  ALLOCATION DOLLARS (GROSS FIGURE)                            
*        R1     =  A(CONTRACT TYPE/PERCENTAGE FOR CONTRACT TYPE)                
*                                                                               
SPREDBUD NTR1                                                                   
         SR    R2,R2               SET UP FOR ALLOCATION CALC                   
         L     R3,ALLOC$           PLACE ALLOCATION IN REG                      
         ZIC   R4,1(R1)            CONTRACT TYPE PERCENTAGE                     
         MR    R2,R4               ALLOC$ X PERCENTAGE                          
         LH    R4,=H'50'           HALF ADD FOR ROUNDING                        
         AR    R4,R3                                                            
         LH    R4,=H'100'          DECIMAL ALIGN                                
         DR    R2,R4                                                            
*                                                                               
*    AT THIS POINT, R3 HAS THE GROSS ALLOCATION DOLLARS FOR                     
*        THIS CONTRACT TYPE.  IT MUST NOW BE SPREAD MONTH BY MONTH.             
*                                                                               
         BAS   RE,SPBU0004         SWING OUT AND SPREAD BY MONTH                
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   SPREAD THIS CONTRACT TYPE'S PERCENT OF GROSS ALLOCATION                     
*        BY MONTH                                                               
*        R2,R3    =  $                                                          
*        R6       =  A(ELEMENT BEING UPDATED)                                   
*                                                                               
SPBU0004 NTR1                                                                   
         MVC   SAVCONTP(1),P+33    SAVE CONTRACT TYPE FROM PRINT                
         LA    R9,P+38                                                          
         ST    R3,CONTOTS          SAVE INCOMING VALUE                          
         EDIT  (R3),(10,(R9)),COMMAS=YES PRINT ALLOCATION $                     
         MVC   DALLOC$(4,R6),CONTOTS      INSERT INTO RECORD                    
         CLI   MONPCTS,C'N'        ANY PERCENTS?                                
         BE    SPBU0008            NO  - DON'T LOAD ALLOCATIONS                 
         MVC   DBUDTOT(4,R6),CONTOTS      INSERT INTO RECORD                    
         MVC   CONTOTS+8(4),CONTOTS       SAVE FOR ADJUSTMENT                   
         MVI   DALLFLG(R6),C'A'    SET SPREADER TAG = ALLOC                     
SPBU0008 EQU   *                                                                
         LA    R1,PERCENTS         A(PERCENTS BY MONTH)                         
         LA    R6,2(R6)            A(BUDGET MONTHS IN ELEMENT)                  
         ZIC   R5,STARTMO          CALCULATE FISCAL START                       
         BCTR  R5,0                ZERO RELATIVE                                
         MH    R5,=H'4'            SIZE OF FIELD                                
         LA    R6,0(R5,R6)         R6=A(STARTING MONTH)                         
         ST    R6,ASTART           SAVE STARTING ADDRESS                        
         SR    R0,R0               RESET ACCUMULATOR                            
         LA    R5,12               DO 12 MONTHS' SPREAD                         
         LA    R9,P+50             PRINT OUTPUT CONTROL                         
SPBU0012 EQU   *                                                                
         SR    R2,R2                                                            
         L     R3,CONTOTS          RELOAD ALLOCATION AMOUNT                     
SPBU0016 EQU   *                                                                
         ZICM  R4,0(R1),2          PERCENT FROM ARRAY                           
         MR    R2,R4               ALLOCATION X PERCENTAGE                      
         LH    R4,=H'5000'         HALF ADD FOR ROUNDING                        
         AR    R3,R4                                                            
         LH    R4,=H'10000'        DECIMAL ALIGN                                
         DR    R2,R4                                                            
         AR    R0,R3               ACCUMULATE FOR TOTALS                        
         ST    R3,CONTOTS+4        MOVE TO STORAGE                              
         MVC   0(4,R6),CONTOTS+4   MOVE TO RECORD                               
         LA    R1,2(R1)            BUMP A(PERCENT FROM ARRAY)                   
         LA    R6,4(R6)            BUMP A(ELEMENT BUCKET)                       
         BCT   R5,SPBU0012         DO EACH BUCKET                               
         BAS   RE,OBAL0008         ADJUST, PRINT RESULTS                        
         L     R5,ASTART           DO THE FUNNY SHIFT                           
         BAS   RE,SHIFTMO                                                       
         CLI   NEWELEM,C'Y'        ELEMENT NEED TO BE INSERTED?                 
         BNE   SPBU0020            NO  - RETURN                                 
         PRINT GEN                                                              
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,WORKX,        X        
               =C'ADD=CODE'                                                     
         PRINT NOGEN                                                            
         ZIC   R3,RBUDCNTR         INCREMENT ELEMENT COUNTER                    
         LA    R3,1(R3)                                                         
         STC   R3,RBUDCNTR         STORE IT BACK                                
SPBU0020 EQU   *                                                                
         L     R6,ASTART           RESET STARTING ADDRESS                       
         GOTO1 TOTACCUM,DMCB,(R6)  ACCUMULATE TOTALS BY CON TYPE                
         SR    R2,R2                                                            
         LA    R5,12                                                            
SPBU0024 EQU   *                                                                
         A     R2,0(R6)            TOTAL THE BUCKETS                            
         LA    R6,4(R6)            BUMP A(BUCKET)                               
         BCT   R5,SPBU0024         GET THEM ALL                                 
*                                                                               
*   RESET FLAG AND REINITIALIZE WORK AREA ON ALL PASSES                         
*                                                                               
         MVI   NEWELEM,C'N'        TURN OFF FLAG                                
         XC    WORKX+2(108),WORKX+2                                             
         L     R5,CONTOTS          RELOAD ALLOCATION AMOUNT                     
         CR    R2,R5               BUCKETS = ALLOC $?                           
         BE    ALSP0099            YES - EXIT                                   
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BE    SPBU0028            YES - SKIP PRINT OUT                         
         MVI   SPACING,2           NO  - PUT OUT MSG                            
         MVC   P+45(LNOBAL),NOBAL                                               
         GOTO1 REPORT                                                           
SPBU0028 EQU   *                                                                
         BAS   RE,OUTOFBAL         ADD TO OUT-OF-BALANCE                        
         B     ALSP0099                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*  FILLS OUT THE 24-MONTH FISCAL YEAR SETUP IN THE BUDGET RECORD                
*                                                                               
*                                                                               
SHIFTMO  EQU   *                                                                
         ZIC   R4,STARTMO          SET UP BCT                                   
         SH    R4,=H'13'                                                        
         LCR   R4,R4                                                            
         SPACE 1                                                                
SHIFT5   MVC   0(4,R6),0(R5)                                                    
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,SHIFT5                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*   ACCUMULATE TOTALS BY CONTRACT TYPE AT DETAIL LEVEL.                         
*                                                                               
TOTACCUM NTR1                                                                   
         L     R6,0(R1)            RESET STARTING ADDRESS OF BUCKETS            
         LA    R1,TOTTABLE         A(TOTALS TABLE)                              
         LA    R2,5                LOOP: 1ST 5 BCKTS = DETAIL LEVEL             
TOTA0002 EQU   *                                                                
         CLI   0(R1),X'00'         EMPTY ENTRY?                                 
         BE    TOTA0006            YES - ADD THE TYPE                           
         CLC   SAVCONTP,TOTCONTP-TOTTABLE(R1)   TYPE FOUND IN TABLE?            
         BE    TOTA0010            YES                                          
         LA    R1,LTOTTABL(R1)     BUMP TO NEXT SET OF BUCKETS                  
         BCT   R2,TOTA0002         GO BACK FOR NEXT                             
         B     TOTA0099            TABLE FULL (???)                             
TOTA0006 EQU   *                                                                
         MVC   TOTCONTP-TOTTABLE(1,R1),SAVCONTP   INSERT CON TYPE               
TOTA0010 EQU   *                                                                
         LA    R3,TOTMON1-TOTTABLE(R1) A(FIRST MONTHLY BUCKET)                  
         LA    R2,12               SET LOOP COUNT                               
TOTA0014 EQU   *                                                                
         L     R4,0(R3)            LOAD TOTAL FOR MONTH                         
         A     R4,0(R6)            ADD BUCKET AMOUNT                            
         ST    R4,0(R3)            PUT IT BACK                                  
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
         BCT   R2,TOTA0014         GO BACK FOR NEXT                             
*                                                                               
         L     R4,TOTALLOC-TOTTABLE(R1)    ADD ALLOC $                          
         L     R2,CONTOTS                                                       
         AR    R4,R2               ACCUMULATE ALLOC $                           
         ST    R4,TOTALLOC-TOTTABLE(R1)    PUT IT BACK                          
TOTA0099 EQU   *                                                                
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*   ADD OUT-OF-BALANCE ENTRIES TO EXCEPTION TABLE                               
*                                                                               
OUTOFBAL NTR1                                                                   
         L     R5,ANJSTATS         A(LAST SPACE USED)                           
         LA    R5,L'NJSTATS(R5)    BUMP TO A(NEXT AVAILABLE SPACE)              
         CLC   0(5,R5),=C'ZZZZZ'   DELIMITER REACHED?                           
         BE    OBAL0004            YES - NO MORE ROOM (TOUGH)                   
         MVC   0(7,R5),SORTREC+SSTAT1    SAVE STATION AND OFFICE                
         MVC   7(1,R5),SAVCONTP    SAVE CONTRACT TYPE                           
         ST    R5,ANJSTATS         SAVE ADDRESS                                 
OBAL0004 EQU   *                                                                
         B     ALSP0099                                                         
         EJECT                                                                  
OBAL0008 NTR1                                                                   
         L     R1,CONTOTS+8        RELOAD ALLOCATION $                          
         SR    R1,R0               DIFFERENCE: SUM VS PARTS                     
         LA    R5,12               NUMBER OF BUCKETS                            
         L     R6,ASTART           TAKE STARTING BUCKET                         
         LA    R9,P+50                                                          
OBAL0012 EQU   *                                                                
         L     R3,0(R6)            AMOUNT IN BUCKET                             
         LTR   R3,R3               ANY VALUE?                                   
         BZ    OBAL0016            NO  - SKIP ADJUSTMENT TEST                   
         AR    R3,R1               ADD ADJUSTMENT, NEG OR POS                   
         SR    R1,R1               BLANK OUT: ONLY DO ONCE                      
         ST    R3,0(R6)            PUT IT BACK                                  
OBAL0016 EQU   *                                                                
         EDIT  (R3),(10,(R9)),COMMAS=YES PRINT ALLOCATION $                     
         LA    R9,13(R9)           BUMP PRINT SPACING                           
         CH    R5,=H'7'            SIX ENTRIES ON LINE?                         
         BNE   OBAL0028            NO                                           
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BNE   OBAL0020            NO                                           
         MVC   P,SPACES            SPACE PRINTLINE                              
         B     OBAL0024                                                         
OBAL0020 EQU   *                                                                
         MVI   SPACING,1           SINGLE SPACE AFTER LINE 1                    
         GOTO1 REPORT              PRINT OUT LINE                               
OBAL0024 EQU   *                                                                
         LA    R9,P+50             RESET FIRST PRINT POSITION                   
OBAL0028 EQU   *                                                                
         LA    R6,4(R6)            BUMP A(BUCKET)                               
         BCT   R5,OBAL0012         DO EACH BUCKET                               
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BNE   OBAL0032            NO                                           
         MVC   P,SPACES            SPACE PRINTLINE                              
         B     ALSP0099                                                         
OBAL0032 EQU   *                                                                
         MVI   SPACING,2           DOUBLE SPACE AFTER LINE 2                    
         GOTO1 REPORT              PRINT OUT LINE                               
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*  SAVE ORIGINAL CONTRACT TYPE AND ITS ALLOCATION FOR RESTART                   
*                                                                               
FRSTRTYP EQU   RBUDRTYP-RBUDELE3                                                
LRTYPBUK EQU   5                                                                
*                                                                               
RESTARTS NTR1                                                                   
         MVI   ELCODE,X'03'        RETRIEVE RESTART ELT FROM REC                
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL            LOOK FOR X'03' ELT                           
         BNE   RSTT0016            NOT FOUND - INSERT IT                        
         LA    R6,FRSTRTYP(R6)     FOUND - SCAN FOR TYPE                        
         LA    R5,9                LOOP CONTROL                                 
RSTT0004 EQU   *                                                                
         CLI   0(R6),X'00'         TYPE EMPTY (END)?                            
         BE    RSTT0008            YES - INSERT NEW                             
         CLC   0(1,R6),SPREDCT     SAME AS 1ST CON TYPE?                        
         BE    RSTT0012            YES - INSERT DOLLARS                         
         LA    R6,LRTYPBUK(R6)     NO  - BUMP TO NEXT BUCKET                    
         BCT   R5,RSTT0004         DO EACH                                      
         DC    H'0'                ALL FULL? IMPOSSIBLE!                        
RSTT0008 EQU   *                                                                
         MVC   0(1,R6),SPREDCT     INSERT 1ST CONTRACT TYPE                     
RSTT0012 EQU   *                                                                
         MVC   1(4,R6),ALLOC$      INSERT ALLOCATION $                          
         B     RSTT0020            EXIT                                         
RSTT0016 EQU   *                   ADD NEW ELEMENT                              
         MVC   WORKY+2(1),SPREDCT  LOAD 1ST CONTYPE                             
         MVC   WORKY+3(4),ALLOC$   INSERT ALLOCATION                            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,WORKY                  
         XC    WORKY+2(46),WORKY+2                                              
RSTT0020 EQU   *                                                                
         MVI   ELCODE,X'02'        RESET ELT CODE FOR GETEL                     
         B     ALSP0099                                                         
         EJECT                                                                  
*                                                                               
*  LOOK FOR RESTART ELEMENT.  IF NOT PRESENT, OR NO ALLOCATION $                
*    FOR FIRST CONTRACT TYPE, USE ALLOC $ ALREADY DETERMINED.                   
*                                                                               
FINDRSTT NTR1                                                                   
         MVI   ELCODE,X'03'        RETRIEVE RESTART ELT FROM REC                
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL            LOOK FOR X'03' ELT                           
         BNE   FIRS0012            NOT FOUND - FINISHED                         
         LA    R6,FRSTRTYP(R6)     FOUND - SCAN FOR TYPE                        
         LA    R5,9                LOOP CONTROL                                 
FIRS0004 EQU   *                                                                
         CLI   0(R6),X'00'         TYPE EMPTY (END)?                            
         BE    FIRS0012            YES - FINISHED                               
         CLC   0(1,R6),SPREDCT     SAME AS 1ST CON TYPE?                        
         BE    FIRS0008            YES - USE ALLOCATION DOLLARS                 
         LA    R6,LRTYPBUK(R6)     NO  - BUMP TO NEXT BUCKET                    
         BCT   R5,FIRS0004         DO EACH                                      
         B     FIRS0012            NOT FOUND - FINISHED                         
FIRS0008 EQU   *                                                                
         MVC   ALLOC$(4),1(R6)     RETRIEVE ALLOCATION $                        
         B     FIRS0012            EXIT                                         
FIRS0012 EQU   *                                                                
         MVI   ELCODE,X'02'        RESET ELT CODE FOR GETEL                     
         B     ALSP0099                                                         
         EJECT                                                                  
READ2    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY2                                                         
*                                                                               
SEQ2     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY2                                                         
*                                                                               
ADD2     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY2                                                         
*                                                                               
WRT2     MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY2                                                         
*                                                                               
HIGH2    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY2                                                         
*                                                                               
DIRCTRY2 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
LINKFILE NMOD1 0,**LINK**                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         L     RF,4(R1)            RELOAD A(RECORD TO BE LOADED)                
         ST    RF,AIOAREA                                                       
         L     RF,8(R1)            LOAD A(COMMAND TO EXECUTE)                   
         MVC   COMMAND(8),0(RF)    LOAD COMMAND                                 
         BAS   RE,FILELINK                                                      
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
         B     LINKEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    LINKEXIT                                                         
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
LINKEXIT EQU   *                                                                
         XIT1                                                                   
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
*                                                                               
FILELINK NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ACCUMULATE COMPANY FIGURES AND INDIVIDUAL OFFICE FIGURES FOR                
*        LATER USE BY NON-JOINED STATION CALCULATIONS                           
*                                                                               
         DS    0H                                                               
OFFTOT   NMOD1 0,*OFFTOT*                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
*                                                                               
*   ACCUMULATE FIGURES FOR COMPANY                                              
*                                                                               
         LA    R1,ACCMON1                                                       
         LA    R2,TMONTBL          A(STATION/OFFICE ACCUMULATOR)                
         LA    R5,12               LOOP CONTROL                                 
OT002    EQU   *                                                                
         L     R3,0(R2)            EXTRACT STA/OFF AMOUNT                       
         A     R3,0(R1)            ADD IN PRIOR OFFICE AMOUNT                   
         ST    R3,0(R1)            RESTORE OFFICE AMOUNT                        
         LA    R2,4(R2)            NEXT BUCKET                                  
         LA    R1,4(R1)            NEXT BUCKET                                  
         BCT   R5,OT002            GET NEXT BUCKET                              
*                                                                               
* ACCUMULATE FIGURES BY OFFICE - 1ST PASS SKIPS COMPANY FIGURES                 
*                                                                               
         LA    R1,ACCOFF           FIND (OR ADD) OFFICE ENTRY)                  
OT003    EQU   *                                                                
         LA    R1,LACCTBL(R1)      SKIP TO NEXT BUCKETS                         
         CLC   =X'0000',0(R1)      ANY ENTRY IN TABLE?                          
         BE    OT006               NO  - ADD OFFICE TO TABLE                    
         CLC   =X'FFFF',0(R1)      TABLE FULL?                                  
         BE    OT095               YES - ABORT(?)                               
         CLC   0(2,R1),SORTREC+SOFF1   OFFICE IN TABLE?                         
         BE    OT009               YES - ADD $ TO ENTRY                         
         B     OT003               NO  - CHECK NEXT ENTRY                       
OT006    EQU   *                                                                
         MVC   0(2,R1),SORTREC+SOFF1   INSERT OFFICE INTO TABLE                 
OT009    EQU   *                                                                
         LA    R1,4(R1)            SKIP OFFICE CODE/SPARE                       
         LA    R2,TMONTBL          A(STATION/OFFICE ACCUMULATOR)                
         LA    R5,12               LOOP CONTROL                                 
OT012    EQU   *                                                                
         L     R3,0(R2)            EXTRACT STA/OFF AMOUNT                       
         A     R3,0(R1)            ADD IN PRIOR OFFICE AMOUNT                   
         ST    R3,0(R1)            RESTORE OFFICE AMOUNT                        
         LA    R2,4(R2)            NEXT BUCKET                                  
         LA    R1,4(R1)            NEXT BUCKET                                  
         BCT   R5,OT012            GET NEXT BUCKET                              
NMODXIT  XIT1                      RETURN TO CALLER                             
*                                                                               
OT095    EQU   *                                                                
         DC    H'0'                ABORT - TOO MANY OFFICES                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
         DS    0H                                                               
GETSORT  NMOD1 0,*GETSORT*                                                      
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
GS010    EQU   *                                                                
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GS025               YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6               TEST RETURN ZERO=EOF                         
         BZ    GS022               YES -                                        
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+35(10),SORTREC+1  **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
*                                                                               
* IF THIS RECORD IS A BUDGET, AND PREVIOUS RECORD WAS A BUDGET,                 
*     THE PREVIOUS RECORD MUST BE PRINTED BEFORE THE NEW ONE                    
*     CAN BE PROCESSED.  THE FIELDS ARE SWAPPED SO HEADER CORRECTLY             
*     DISPLAYS VALUES.                                                          
*                                                                               
         CLI   QOPTION1,C'T'       TEST RUN?                                    
         BE    GS016               YES - TREAT AS 'STATION ONLY'                
         CLI   QOPTION1,C'X'       SPECIAL NON-REQUEST TEST RUN?                
         BE    GS020               YES - TREAT AS 'STATION + OFFICE'            
         CLI   QOPTION1,C'B'       STATION + OFFICE?                            
         BE    GS020               YES                                          
         CLI   QOPTION1,C'S'       STATION ONLY?                                
         BNE   GS018               NO  - PROCESS OFFICE RECS ONLY               
GS016    EQU   *                                                                
         CLI   STYP,C'1'           YES - STATION RECORD?                        
         BE    GS020               YES                                          
         B     GS010               RETURN EOF FOUND                             
GS018    CLI   QOPTION1,C'N'       OFFICE REPORT/NO UPDATE?                     
         BNE   GS020               NO  - OFFICE ONLY - DO ALL RECS              
         CLI   STYP,C'1'           STATION RECORD?                              
         BE    GS010               YES - SKIP IT                                
GS020    EQU   *                                                                
         CLC   SORTREC+1(9),SORTREC2+1     SAME RECORD GROUP?                   
         BE    GS025               YES                                          
GS022    EQU   *                                                                
         CLI   SSUBTYP2,C'0'       PREVIOUS RECORD BUDGET?                      
         BNE   GS025               NO  -                                        
         CLI   SNOJOIN2,C'1'       PREVIOUS NOT JOINED OR LEFT?                 
         BE    GS025               YES - ALREADY PRINTED                        
         MVC   RESETREC,SORTREC    IN CASE THIS HAPPENS AT A PAGE               
         MVC   SORTREC,SORTREC2    BREAK, VALUES FOR BREAK MUST BE              
*                                  RESET FOR PRINTING                           
         BAS   RE,GS100            LOCATE OFFICE FIGURES IN TABLE               
         L     R3,DUB              SET RETURNED ADDRESS                         
         GOTO1 =A(ALLSPRED),DMCB,(RC),(R3)  NO CONTRACT DOLLARS:                
         MVC   SORTREC,RESETREC    RESTORE ORIGINAL RECORD                      
         MVI   SSUBTYP2,C'9'       FLAG PREVIOUS RECORD PRINTED                 
GS025    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  ROUTINE FINDS OFFICE IN TABLE, SETS DUB TO THAT ADDRESS TO                   
*    CALCULATE SPREAD FROM OFFICE, RATHER THAN STATION, FIGURES                 
*                                                                               
GS100    NTR1                                                                   
         LA    R1,ACCOFF           A(FIRST TABLE ENTRY - COMPANY)               
         CLI   STYP,C'2'           OFFICE/STATION REPORT?                       
         BE    GS120               YES - DON'T BOTHER LOOKING UP                
GS104    EQU   *                                                                
         LA    R1,LACCTBL(R1)      POINT TO NEXT ENTRY                          
         CLC   =X'0000',0(R1)      EMPTY ENTRY?                                 
         BE    GS108               YES - FORCE COMPANY FIGURES                  
         CLC   =X'FFFF',0(R1)      END OF TABLE?                                
         BE    GS108               YES - FORCE COMPANY FIGURES                  
         CLC   0(2,R1),SORTREC+SOFF1       COMPARE OFFICE CODES                 
         BE    GS120                                                            
         B     GS104               GO BACK FOR NEXT                             
GS108    EQU   *                                                                
         LA    R1,ACCOFF           SET A(COMPANY BUCKETS)                       
GS120    EQU   *                                                                
         LA    R1,4(R1)            SKIP OVER CODE BYTES                         
         ST    R1,DUB              SET RETURN VALUE                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADD ENTRY TO EXCEPTION TABLE, SET FLAG BIT FOR STATIONS/OFFICES             
*     WHICH HAVE CONTRACT DOLLARS, BUT HAVE NO BUDGET ALLOCATION                
*     DOLLARS ENTERED.                                                          
*                                                                               
         DS    0H                                                               
NOBUDDOL NMOD1 0,*NOBUD$*                                                       
         L     RC,0(R1)                                                         
         L     R5,ANJSTATS         A(LAST SPACE USED)                           
         LA    R5,L'NJSTATS(R5)    BUMP TO NEXT AVAILABLE SPACE                 
         CLC   0(5,R5),=C'ZZZZZ'   DELIMITER REACHED?                           
         BE    NOBU0099            YES - NO MORE ROOM (TOUGH)                   
         MVC   0(7,R5),SORTREC+SSTAT1   SAVE STATION AND OFFICE                 
         OI    7(R5),X'04'         SET NO BUD ALLOC FLAG BIT                    
         ST    R5,ANJSTATS         SAVE ADDRESS                                 
NOBU0099 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DUMP THE TOTALS BY CONTRACT TYPE:  DETAILS ARE IN FIRST FIVE                
*      ENTRIES OF TABLE                                                         
*                                                                               
         DS    0H                                                               
DUMPTOTS NMOD1 0,*DMPTOT*                                                       
         L     RC,0(R1)                                                         
         LA    R1,TOTTABLE         A(TOTAL TABLE)                               
         LA    R6,5                MAXIMUM NUMBER OF CONTRACT TYPES             
DUTO0002 EQU   *                                                                
         CLI   TOTCONTP-TOTTABLE(R1),X'00'    ANY CONTRACT TYPE ENTRY?          
         BE    DUTO0008                       NO  - GRAND TOTALS?               
         MVC   P+10(10),=C'--TOTAL-- '                                          
         MVC   P+33(1),TOTCONTP-TOTTABLE(R1)  INSERT CONTRACT TYPE              
         BAS   RE,DUMPLINE         DUMP THIS TOTAL LINE                         
         BAS   RE,REPTTOTL         ACCUMULATE REPORT TOTALS                     
         CH    R6,=H'5'            1ST SET OF TOTALS?                           
         BE    DUTO0004            YES - DON'T GRAND TOTAL                      
         BAS   RE,DUMPGRND         NO  - ACCUM GRAND TOTAL                      
DUTO0004 EQU   *                                                                
         LA    R1,LTOTTABL(R1)     BUMP A(TOTAL TABLE)                          
         BCT   R6,DUTO0002         GO BACK FOR NEXT                             
DUTO0008 EQU   *                                                                
         CH    R6,=H'4'            ONLY ONE CONTRACT TYPE?                      
         BNL   DUTO0098            YES - DON'T PRINT GRAND TOTAL                
         MVC   P+10(12),=C'GRAND TOTAL '                                        
         LA    R1,TOTTABLE         SET TO 1ST BUCKETS                           
         BAS   RE,DUMPLINE         PRINT GRAND TOTAL                            
DUTO0098 EQU   *                                                                
         LA    RF,TOTTABLE         ZERO OUT 1ST 5 ENTRIES IN TABLE              
         LA    R3,LTOTTABL*5       SET L(TOTALS TABLE)                          
         XCEFL 0(RF),(R3)                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PRINT OUT THE TOTALS BUCKETS ADDRESSED BY 'R1'.  THE DESCRIPTIVE            
*       INFORMATION HAS BEEN INSERTED INTO THE PRINTLINE ALREADY.               
*                                                                               
DUMPLINE NTR1                                                                   
         LA    R9,P+38                    SET A(ALLOCATION $)                   
         L     R3,TOTALLOC-TOTTABLE(R1)   SET ALLOCATION $                      
         EDIT  (R3),(10,(R9)),COMMAS=YES                                        
         LA    R5,12               BUCKET COUNTER                               
         LA    R9,P+50             PRINT POSITION                               
         LA    R1,TOTMON1-TOTTABLE(R1)    SET TO A(BUCKETS)                     
DULI0002 EQU   *                                                                
         L     R3,0(R1)            LOAD BUCKET AMOUNT                           
         EDIT  (R3),(10,(R9)),COMMAS=YES                                        
         LA    R9,13(R9)           INCREMENT A(PRINT POSITION)                  
         CH    R5,=H'7'            SIX ENTRIES ON LINE?                         
         BNE   DULI0010            NO                                           
         LA    R9,P+50             YES - RESET 1ST PRINT POSITION               
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BNE   DULI0008            NO                                           
         MVC   P,SPACES            YES - SPACE OUT PRINTLINE                    
         B     DULI0010                                                         
DULI0008 EQU   *                                                                
         MVI   SPACING,1           SINGLE SPACE AFTER LINE 1                    
         GOTO1 REPORT                                                           
DULI0010 EQU   *                                                                
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         BCT   R5,DULI0002         GO BACK FOR NEXT                             
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BE    DULI0012            YES - SKIP PRINTING                          
         MVI   SPACING,2           DOUBLE SPACE AFTER 2ND LINE                  
         GOTO1 REPORT              NO  - PRINT LINE                             
DULI0012 EQU   *                                                                
         MVC   P,SPACES            SPACE OUT PRINT LINE                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DUMPGRND ACCUMULATES THE 2ND THROUGH 5TH CONTRACT TYPES INTO                
*     THE FIRST TYPE ACCUMULATORS                                               
*     R1   =   A(BUCKETS TO BE ACCUMULATED)                                     
*                                                                               
DUMPGRND NTR1                                                                   
         LA    R2,TOTTABLE         A(TOTAL TABLE 1ST ENTRIES)                   
         LA    R5,13               NUMBER OF BUCKETS (ALLOC$ + 12)              
         LA    R1,TOTALLOC-TOTTABLE(R1) SKIP MISCELLANEOUS                      
         LA    R2,TOTALLOC-TOTTABLE(R2) SKIP MISCELLANEOUS                      
DUGR0002 EQU   *                                                                
         L     R3,0(R1)            GRAND TOTAL BUCKET                           
         A     R3,0(R2)            ADD IN DETAIL BUCKET                         
         ST    R3,0(R2)            STORE IT BACK                                
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         BCT   R5,DUGR0002                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ACCUMULATE GRAND TOTALS FOR REPORT                                          
*                                                                               
REPTTOTL NTR1                                                                   
         LA    R5,5                MAX NUMBER OF CONTRACT TYPES                 
         LA    R2,TOTTABLE         A(1ST ENTRY IN TABLE)                        
         LA    R2,LTOTTABL*5(R2)   SKIP 1ST 5 ENTRIES                           
REPL0002 EQU   *                                                                
         CLI   TOTCONTP-TOTTABLE(R2),X'00' ANY CONTRACT TYPE?                   
         BE    REPL0006            NO  - INSERT IT AND ADD #S                   
         CLC   TOTCONTP-TOTTABLE(1,R2),TOTCONTP-TOTTABLE(R1)                    
         BE    REPL0008            SAME CON TYPE - ADD #S                       
         LA    R2,LTOTTABL(R2)     BUMP TO NEXT BUCKETS                         
         BCT   R5,REPL0002         GO BACK FOR NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN                             
REPL0006 EQU   *                                                                
         MVC   TOTCONTP-TOTTABLE(1,R2),TOTCONTP-TOTTABLE(R1)                    
REPL0008 EQU   *                                                                
         LA    R2,4(R2)            SKIP TYPE/FLAGS                              
         LA    R1,4(R1)            SKIP TYPE/FLAGS                              
         LA    R5,13               LOOP COUNTER                                 
REPL0010 EQU   *                                                                
         L     R0,0(R1)            DETAIL BUCKET                                
         A     R0,0(R2)            TOTAL BUCKET                                 
         ST    R0,0(R2)            SAVE TOTAL                                   
         LA    R1,4(R1)            NEXT BUCKET                                  
         LA    R2,4(R2)            NEXT BUCKET                                  
         BCT   R5,REPL0010         GO BACK FOR NEXT                             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DUMP THE GRAND TOTALS BY CONTRACT TYPE:  GRAND TOTALS ARE IN                
*      THE SECOND 5 ENTRIES OF THE TABLE                                        
*                                                                               
         DS    0H                                                               
GRNDTOTS NMOD1 0,*GRDTOT*                                                       
         L     RC,0(R1)                                                         
         MVI   FORCEHED,C'Y'       FORCE HEADING FOR GRAND TOTALS               
         LA    R1,TOTTABLE         A(TOTAL TABLE)                               
         LA    R1,LTOTTABL*5(R1)   A(GRAND TOTAL ENTRIES)                       
         LA    R6,5                MAXIMUM NUMBER OF CONTRACT TYPES             
GRTO0002 EQU   *                                                                
         CLI   TOTCONTP-TOTTABLE(R1),X'00'    ANY CONTRACT TYPE ENTRY?          
         BE    GRTO0008                       NO  - GRAND TOTALS?               
         MVC   P+10(16),=C'--GRAND TOTAL-- '                                    
         MVC   P+33(1),TOTCONTP-TOTTABLE(R1)  INSERT CONTRACT TYPE              
         BAS   RE,GRNDLINE         DUMP THIS TOTAL LINE                         
         CH    R6,=H'5'            1ST SET OF TOTALS?                           
         BE    GRTO0004            YES - DON'T GRAND TOTAL                      
         BAS   RE,GRNDGRND         NO  - ACCUM GRAND TOTAL                      
GRTO0004 EQU   *                                                                
         LA    R1,LTOTTABL(R1)     BUMP A(TOTAL TABLE)                          
         BCT   R6,GRTO0002         GO BACK FOR NEXT                             
GRTO0008 EQU   *                                                                
         CH    R6,=H'4'            ONLY ONE CONTRACT TYPE?                      
         BE    GRTO0098            YES - DON'T PRINT GRAND TOTAL                
         MVC   P+10(14),=C'REPORT  TOTAL '                                      
         LA    R1,TOTTABLE         SET TO 1ST OF 5 GRAND TOTAL BKTS             
         LA    R1,LTOTTABL*5(R1)                                                
         BAS   RE,GRNDLINE         PRINT GRAND TOTAL                            
GRTO0098 EQU   *                                                                
         LA    RF,TOTTABLE         ZERO OUT 2ND 5 ENTRIES IN TABLE              
         LA    RF,LTOTTABL*5(RF)                                                
         LA    R3,LTOTTABL*5       SET L(TOTALS TABLE)                          
         XCEFL 0(RF),(R3)                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PRINT OUT THE TOTALS BUCKETS ADDRESSED BY 'R1'.  THE DESCRIPTIVE            
*       INFORMATION HAS BEEN INSERTED INTO THE PRINTLINE ALREADY.               
*                                                                               
GRNDLINE NTR1                                                                   
         LA    R9,P+38                    SET A(ALLOCATION $)                   
         L     R3,TOTALLOC-TOTTABLE(R1)   SET ALLOCATION $                      
         EDIT  (R3),(10,(R9)),COMMAS=YES                                        
         LA    R5,12               BUCKET COUNTER                               
         LA    R9,P+50             PRINT POSITION                               
         LA    R1,TOTMON1-TOTTABLE(R1)    SET TO A(BUCKETS)                     
GRLI0002 EQU   *                                                                
         L     R3,0(R1)            LOAD BUCKET AMOUNT                           
         EDIT  (R3),(10,(R9)),COMMAS=YES                                        
         LA    R9,13(R9)           INCREMENT A(PRINT POSITION)                  
         CH    R5,=H'7'            SIX ENTRIES ON LINE?                         
         BNE   GRLI0010            NO                                           
         LA    R9,P+50             YES - RESET 1ST PRINT POSITION               
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BNE   GRLI0008            NO                                           
         MVC   P,SPACES            YES - SPACE OUT PRINTLINE                    
         B     GRLI0010                                                         
GRLI0008 EQU   *                                                                
         MVI   SPACING,1           SINGLE SPACE AFTER LINE 1                    
         GOTO1 REPORT                                                           
GRLI0010 EQU   *                                                                
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         BCT   R5,GRLI0002         GO BACK FOR NEXT                             
         CLI   QOPTION1,C'O'       OFFICE RUN ONLY?                             
         BE    GRLI0012            YES - SKIP PRINTING                          
         MVI   SPACING,2           DOUBLE SPACE AFTER 2ND LINE                  
         GOTO1 REPORT              NO  - PRINT LINE                             
GRLI0012 EQU   *                                                                
         MVC   P,SPACES            SPACE OUT PRINT LINE                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GRNDGRND ACCUMULATES THE 2ND THROUGH 5TH CONTRACT TYPES INTO                
*     THE FIRST TYPE ACCUMULATORS                                               
*     R1   =   A(BUCKETS TO BE ACCUMULATED)                                     
*                                                                               
GRNDGRND NTR1                                                                   
         LA    R2,TOTTABLE         A(TOTAL TABLE 1ST ENTRIES)                   
         LA    R2,LTOTTABL*5(R2)                                                
         LA    R5,13               NUMBER OF BUCKETS (ALLOC$ + 12)              
         LA    R1,TOTALLOC-TOTTABLE(R1) SKIP MISCELLANEOUS                      
         LA    R2,TOTALLOC-TOTTABLE(R2) SKIP MISCELLANEOUS                      
GRGR0002 EQU   *                                                                
         L     R3,0(R1)            GRAND TOTAL BUCKET                           
         A     R3,0(R2)            ADD IN DETAIL BUCKET                         
         ST    R3,0(R2)            STORE IT BACK                                
         LA    R1,4(R1)            BUMP TO NEXT BUCKET                          
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         BCT   R5,GRGR0002                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
ALTHDR   NMOD1 0,*ALTHDR*                                                       
         LA    R1,SAVEREGS-ALTHDR  RESTORE REGISTERS                            
         AR    R1,RF                                                            
         LM    R2,RA,0(R1)         LOAD R2 -> RA                                
         L     RC,40(R1)           LOAD RC:  RB IS SET BY NMOD                  
*                                                                               
         CLI   QRGPROG,C'9'        BUDGET YEAR IN '90'S?                        
         BE    AH006               YES - IGNORE PRIOR TO 90'S                   
         MVC   HEAD1+47(2),=C'20'  NO  - MOVE IN NEXT CENTURY                   
         B     AH008                                                            
AH006    EQU   *                                                                
         MVC   HEAD1+49(2),=C'19'                                               
AH008    EQU   *                                                                
         MVC   HEAD1+51(2),QRGPROG LOAD BUDGET YEAR                             
         CLI   RCSUBPRG,X'6'       NON-JOINED STATIONS LIST?                    
         BE    AH036               YES - SKIP DETAIL LOAD                       
         CLI   STYP,C'1'           TYPE 1 OR TYPE 2 REC?                        
         BNE   AH016               TYPE 2                                       
         MVC   HEAD4+11(2),SORTREC+SGROUP                                       
         MVC   HEAD5+15(5),SORTREC+SSTAT1                                       
         MVC   HEAD5+24(20),SORTREC+SMKTNM                                      
         CLI   SETJOIN,C'N'            'N' = NOT JOINED/LEFT                    
         BNE   AH012                                                            
         MVC   HEAD5+50(28),=C'**STATION NO LONGER REPPED**'                    
         PRINT GEN                                                              
         GOTO1 AH090,DMCB,1            INSERT INTO NOT-JOINED TABLE             
         PRINT NOGEN                                                            
         CLI   SORTREC+SSTALEFT,C'N'   STATION 'NOT GONE'?                      
         BNE   AH010                   STATION HAS LEFT                         
         MVC   HEAD5+50(28),=C'*** NEW STATION ***         '                    
         B     AH020                                                            
AH010    EQU   *                                                                
         GOTO1 AH090,DMCB,2            INSERT INTO TABLE                        
         B     AH020                                                            
AH012    EQU   *                                                                
         CLI   SORTREC+SSTALEFT,C'N'   STATION 'NOT GONE'?                      
         BE    AH020               YES - STILL REPPED                           
         MVC   HEAD5+47(28),=C'**STATION NO LONGER REPPED**'                    
         GOTO1 AH090,DMCB,2        INSERT INTO TABLE                            
         B     AH020                                                            
AH016    EQU   *                                                                
         MVC   HEAD5+15(2),SORTREC+SOFF2                                        
         MVC   HEAD5+24(20),SORTREC+SOFFNM                                      
AH020    EQU   *                                                                
         LA    R1,MONCON           A(MONTH HEADING TABLE)                       
         ZIC   R2,STARTMO          FISCAL START MONTH                           
         BCTR  R2,0                ZERO RELATIVE                                
         MH    R2,=H'3'            SIZE OF FIELD                                
         AR    R1,R2               OFFSET INTO TABLE                            
         LA    R2,MCEND            A(END OF TABLE)                              
         LA    R5,12               NUMBER OF ENTRIES                            
         LA    R3,HEAD7+57         A(FIRST ENTRY,FIRST LINE)                    
AH024    EQU   *                                                                
         MVC   0(3,R3),0(R1)                                                    
         LA    R1,3(R1)            BUMP A(HEADING)                              
         CR    R1,R2               END OF TABLE REACHED?                        
         BNE   AH028               NO                                           
         LA    R1,MONCON           RESET START OF TABLE                         
AH028    EQU   *                                                                
         LA    R3,13(R3)           BUMP A(HEADING ON LINE)                      
         CH    R5,=H'7'            FIRST LINE FULL?                             
         BNE   AH032               NO                                           
         LA    R3,HEAD8+57         YES - RESET TO NEXT LINE                     
AH032    EQU   *                                                                
         BCT   R5,AH024            GO BACK FOR NEXT                             
AH036    EQU   *                                                                
         MVC   HEAD2+53(6),=C'BUDGET'                                           
         MVC   HEAD2+60(6),DBUDFRM                                              
         MVI   HEAD2+67,C'-'                                                    
         MVC   HEAD2+69(6),DBUDTO                                               
         CLI   QCONTYPE,C' '       ANY CONTRACT TYPE ENTERED?                   
         BE    AH040               NO  - FINISHED                               
         MVC   HEAD3+99(14),=C'CONTRACT TYPE:'                                  
         MVC   HEAD3+114(1),QCONTYPE                                            
AH040    XIT1                                                                   
*                                                                               
*  TABLE STATION CALLS FOR NOT JOINED/LEFT STATIONS FOR RECAP                   
*                                                                               
AH090    NTR1                                                                   
         L     R5,ANJSTATS         A(LAST SPACE USED)                           
         CLC   0(5,R5),SORTREC+SSTAT1    ALREADY IN TABLE?                      
         BE    AH099               YES - JUST SET INDICATOR                     
         LA    R5,L'NJSTATS(R5)    BUMP TO A(NEXT AVAILABLE SPACE)              
         CLC   0(5,R5),=C'ZZZZZ'   DELIMITER REACHED?                           
         BE    AH040               YES - NO MORE ROOM (TOUGH)                   
         MVC   0(5,R5),SORTREC+SSTAT1 SAVE STATION CALLS                        
         ST    R5,ANJSTATS         SAVE IT                                      
*                                                                               
*  FLAG PASSED IN AS 1ST PARAM INDICATES NOT JOINED/LEFT, NO LONGER             
*    REPPED.  THE 8TH BYTE IN ENTRY IS OR'D.  BIT 7 (VALUE 1)                   
*    IS NOT JOINED/LEFT.  BIT 6 (VALUE 2) IS NO LONGER REPPED.                  
*                                                                               
AH099    EQU   *                                                                
         OC    7(1,R5),DMCB+3      SET 'JOINED/LEFT-NO LNGR REPD'               
         B     AH040                                                            
         SPACE 5                                                                
         EJECT                                                                  
MONCON   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
MCEND    DS    0H                                                               
SAVEREGS DS    11F                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043REREP1C02 05/01/02'                                      
         END                                                                    
