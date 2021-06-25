*          DATA SET REREP1L02  AT LEVEL 110 AS OF 05/01/02                      
*PHASE RE1L02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'REREP1B02 - BUDGET/BILLING REVENUE PROJECTIONS'                 
*********************************************************************           
*                                                                   *           
*        REREP1B02 --- REPPAK BUDGET/BILLING REVENUE PROJECTIONS    *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JAN28/92 (BU ) --- INITIAL ENTRY - CLONED FROM A COMBINATION OF   *           
*                    ORIGINAL RE1A02 + RE1B02                       *           
*                                                                   *           
* FEB12/92 (BU ) --- PERMIT SELECTION OF MONTH-BY-MONTH BUDGET $    *           
*                                                                   *           
* MAR30/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                  *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* AUG27/92 (BU ) --- FIX PROJECTED REVENUE TOTALS BUG.              *           
*                                                                   *           
* JAN25/93 (BU ) --- CHANGE POST TO BEST-DOLLAR FIGURE.  NMOD A     *           
*                    FEW ROUTINES FOR ADDRESSABILITY.               *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL - REGENALL1              *           
*                                                                   *           
* JAN12/96 (BU ) --- IMPLEMENT TVB REGION FILTER FOR STATIONS       *           
*                                                                   *           
* JAN20/97 (DBU) --- DATA BROKEN UP INTO MONTHLY REPORTS            *           
*                                                                   *           
* MAY23/97 (BU ) --- UPGRADE FOR YR 2000                            *           
*                    NOTE:  THIS MAY BE INCOMPLETE.  QSTART IS USED *           
*                    TO CALCULATE A FIELD (BUDYEAR), WHOSE USE IS   *           
*                    UNCLEAR, AND, AT THIS TIME, THERE IS NO DATA   *           
*                    TO SET UP A TEST OF IT.                        *           
*                                                                   *           
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
* DISPLAY OPTIONS:                                                  *           
*   REQUESTOR = $$XTRACT  WILL SHOW CONTRACT #S, DATES, AND $$      *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R9  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1L02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1L02,R7,R9,RR=RE                                           
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
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   UNDRSCOR,X'6D'      SET FIELD TO UNDERSCORE                      
         MVI   CONFLAG,C'N'        SET CONTRACT FLAG TO NO                      
         MVC   QCONTYP2,QCONTYPE   ESTABLISH INCLUDE/EXCLUDE TYPE               
         OC    QCONTYP2(1),=X'40'  SET UPPER CASE BIT                           
         MVC   UNDRSCOR+1(L'UNDRSCOR),UNDRSCOR                                  
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC2,SORTREC2                                                
*                                                                               
*        LOAD TODAY'S DATE FOR COMPARISON TO                                    
*        EFFECTIVE DATE IN COMMISSION RATE RECORD                               
*        USE TODAY'S DATE TO DETERMINE BROADCAST MONTH                          
*        USE BROADCAST MONTH AS TEST AGAINST COMMISSION                         
*            RATE EFFECTIVE DATE                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,RATEDATE),(0,RATEDATE)                            
         GOTO1 =V(GETBROAD),DMCB,(0,RATEDATE),RATEDATE+6,0,0                    
         GOTO1 DATCON,DMCB,(0,RATEDATE+6),(3,RATEDATE)                          
*                                                                               
*  INITIALIZE EXCEPTION REPORTING TABLES, AND SET FIRST ADDRESSES,              
*     WHICH ARE SET 8 BYTES BEFORE THE FIRST ENTRY TO HANDLE 'FIRST-            
*     TIME' SITUATION.                                                          
*                                                                               
         LA    RE,LEFSTATS         ZERO OUT 'LEFT-STATION'                      
         LA    RF,LLEFSTAT                                                      
         XCEF                                                                   
         L     RE,SNOCOMMS          ZERO OUT 'NO COMMISSION RATES'              
         LA    RF,LNOCOMMS                                                      
         XCEF                                                                   
         LA    RF,LEFSTATS-8       SET FIRST ADDRESS                            
         ST    RF,ALEFSTAT                                                      
         L     RF,SNOCOMMS          ZERO OUT 'NO COMMISSION RATES'              
         SH    RF,=H'8'                                                         
         ST    RF,ANOCOMMS                                                      
         ST    RF,FNOCOMMS                                                      
*                                                                               
*   SET START/END MONTH NUMBERS FOR MONTH-BY-MONTH BUDGET RETRIEVAL             
*                                                                               
         PACK  DUB(8),QSTART+2(2)  REQUEST START MONTH                          
         CVB   RE,DUB                                                           
         STC   RE,STARTMO                                                       
         PACK  DUB(8),QEND+2(2)    REQUEST END MONTH                            
         CVB   RF,DUB                                                           
         STC   RF,ENDMO                                                         
         BCTR  RE,0                MAKE START MONTH ZERO RELATIVE               
         CR    RE,RF               START VS END: SAME YEAR?                     
         BNH   INIT0010            YES                                          
         LA    RF,12(RF)           NO  -                                        
INIT0010 EQU   *                                                                
         SR    RF,RE               CALCULATE NUMBER OF MONTHS                   
         STC   RF,NUMMOS           SAVE IT                                      
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
*   GRPINIT: FIRST GROUP MODE SETTING                                           
*                                                                               
GRPINIT  NTR1                                                                   
         XC    SORTREC,SORTREC              SET UP BASIC KEY                    
         MVC   SORTREC+SGROUP(2),RCONKGRP   INSERT GROUP                        
         MVC   SORTREC+SSTAT1(5),RCONKSTA   INSERT STATION                      
         MVC   SORTREC+SOFF1(2),RCONKOFF    INSERT OFFICE                       
*                                                                               
*  THIS IS SORT RECORD TYPE 1.  AT OUTPUT TIME, A SECOND SORT                   
*  RECORD TYPE 2 WILL BE GENERATED BY SWITCHING STATION AND OFFICE              
*  TO PRODUCE THE REPORTS IN THE SECOND SEQUENCE.                               
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
         B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFINIT: FIRST OFFICE                                                       
*                                                                               
OFFINIT  NTR1                                                                   
         MVC   SORTREC+SGROUP(2),RCONKGRP    INSERT GROUP                       
         MVC   SORTREC+SSTAT1(5),RCONKSTA    INSERT STATION                     
         MVC   SORTREC+SOFF1(2),RCONKOFF     INSERT OFFICE                      
         MVC   SORTREC+SOFFNM(20),ROFFNAME   INSERT OFFICE NAME                 
         B     MODEEXIT                                                         
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
*                                                                               
         CLC   =C'@@@@@@@',QUESTOR                                              
         BNE   POST0010                                                         
         MVC   P+2(11),=C'CONTRACT = '                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+13,4,=C'TOG'                              
         GOTO1 REPORT                                                           
POST0010 EQU   *                                                                
         SPACE 1                                                                
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
         L     R4,ANEWMON          A(MONTH TABLE)                               
         LA    R3,TMONTBL          A(ACCUMULATORS)                              
         XC    BUCKCTR,BUCKCTR     CLEAR BUCKET DISPL                           
*                                                                               
*   FIND REQUEST START DATE IN TABLE.                                           
*                                                                               
POST0020 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0030            FOUND - BEGIN TO PULL FIGURES                
         BH    MODEEXIT            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0020            GO BACK FOR NEXT                             
*                                                                               
POST0030 EQU   *                                                                
         CLI   0(R4),0             ANY TABLE ENTRY?                             
         BE    MODEEXIT            NO  - EXIT                                   
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    MODEEXIT            TABLE > END DATE - EXIT                      
*                                                                               
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         TM    FLAG6(R4),X'01'     ANY INVOICED $ IN BUCKET?                    
         BNO   POST0040            NO  - TAKE ORDERED $                         
         MVC   CONTOTS(4),CUASATIN(R6)                                          
*                                  YES = INVOICED THIS YEAR                     
         B     POST0050                                                         
POST0040 EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)                                            
*                                  NO  = ORDERED THIS YEAR                      
POST0050 EQU   *                                                                
*                                                                               
         OC    CONTOTS(4),CONTOTS  ANY VALUE IN BUCKET?                         
         BZ    POST0070            NO  - DON'T SET CONFLAG                      
         MVI   CONFLAG,C'Y'        YES - SET CONFLAG                            
POST0060 EQU   *                                                                
         CLC   =C'@@@@@@@',QUESTOR                                              
         BNE   POST0070                                                         
         BAS   RE,POSTDISP                                                      
POST0070 EQU   *                                                                
*                                                                               
* STORE CONTRACT TOTALS $ FOR EACH MONTH IN INV TABLE                           
* EACH ENTRY IN THE TABLE CONSISTS OF 4 BYTES INV$ + 4 BYTES YR/MNTH            
*                                                                               
         ZIC   RF,BUCKCTR          GET BUCKET NUMBER                            
         LA    RF,1(RF)            INCREMENT BUCKET COUNTER                     
         STC   RF,BUCKCTR          SAVE IT                                      
         BCTR  RF,0                CURRENT BUCKET                               
         SLA   RF,3                DISPL = COUNTER * 8                          
         LA    RE,INVTBL                                                        
         AR    RE,RF               GET CORRECT DISPL IN INV TABLE               
*                                                                               
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD CURRENT INV $                            
         A     R0,0(RE)            TO ACCUMULATOR CURR INV $                    
         ST    R0,0(RE)            STORE IT BACK                                
*                                                                               
         MVC   4(4,RE),0(R4)       YEAR/MONTH                                   
*                                                                               
*        MVC   P+09(6),0(R4)       TEST                                         
*        EDIT  (4,CONTOTS),(10,P+20),COMMAS=YES,MINUS=YES                       
*        GOTO1 REPORT                                                           
*                                                                               
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST0030            SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
POSTDISP NTR1                                                                   
         MVC   P+2(07),=C'DATE = '                                              
         MVC   P+09(6),0(R4)                                                    
         EDIT  (4,CONTOTS),(10,P+20),COMMAS=YES,MINUS=YES                       
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*   GRPDONE:  LAST GROUP MODE SETTING                                           
*                                                                               
GRPDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    GDEXIT              YES                                          
         BAS   RE,SORTGEN                                                       
         XC    INVTBL,INVTBL                                                    
GDEXIT   EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*   STADONE:  LAST STATION MODE SETTING                                         
*                                                                               
STADONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    SD099               YES                                          
         BAS   RE,SORTGEN                                                       
         XC    INVTBL,INVTBL                                                    
SD099    EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFDONE:  LAST OFFICE MODE SETTING                                          
*                                                                               
OFFDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    ODEXIT              YES                                          
         BAS   RE,SORTGEN                                                       
         XC    INVTBL,INVTBL                                                    
ODEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*             SORT FILE                                                         
*             COMPARE SORTED DATA KEYS VS BUDGET RECORDS ON FILE                
*             PRODUCE REPORT                                                    
*                                                                               
* OPTION VALUES USED TO PRODUCE REPORTS:                                        
*  OPTION1: S = STATION ONLY  O = OFFICE ONLY  R = REGIONAL  A = ALL            
*  OPTION2: A = BUDGET        B = BILLING      C = BOTH: A+B                    
*                                                                               
* PRINTLINE EQUATES FOR FORMATTING                                              
*                                                                               
POFF     EQU   13                  OFFICE CODE                                  
POFFNAM  EQU   20                  OFFICE NAME                                  
PSTN     EQU   13                  STATION CALL LETTERS                         
PSTNNAM  EQU   20                  STATION MARKET NAME                          
PMONTH   EQU   13                  MONTH                                        
*                                                                               
PTYPE    EQU    35                 CONTRACT TYPE DISPLAY                        
PCOL1    EQU    40                 COLUMN 1 FOR DETAILS/TOTALS                  
PCOL2    EQU    56                 COLUMN 2 FOR DETAILS/TOTALS                  
PCOL3    EQU    72                 COLUMN 3 FOR DETAILS/TOTALS                  
PCOL4    EQU    88                 COLUMN 4 FOR DETAILS/TOTALS                  
PCOL5    EQU    104                COLUMN 5 FOR DETAILS/TOTALS                  
*                                                                               
PCOL2COM EQU    59                 COLUMN 2 FOR COMMISSIONS                     
PCOL3COM EQU    74                 COLUMN 3 FOR COMMISSIONS                     
*                                                                               
RPTDONE  NTR1                                                                   
         XC    SAVESTA,SAVESTA     INITIALIZE                                   
         XC    KEY,KEY             ESTABLISH FIRST BUDGET KEY                   
         MVI   KEY,X'13'                                                        
         MVC   KEY+16(2),RCREPFL                                                
         MVC   KEY+18(2),QRGPROG   LOAD START YEAR                              
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RPTD0004                                                         
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RPTD0004                                                         
         MVC   KEY+20(5),QSTATION  YES - LOAD INTO KEY                          
*                                                                               
RPTD0004 GOTO1 HIGH                                                             
*                                                                               
RPTD0008 CLI   KEY,X'13'           SAME CODE?                                   
         BNE   RPTD0084            NO  - DONE EXTRACTING                        
         CLC   KEY+16(2),RCREPFL   SAME REP?                                    
         BNE   RPTD0084            NO  - DONE EXTRACTING                        
         CLC   KEY+18(2),QRGPROG   SAME YEAR?                                   
         BNE   RPTD0084            NO  - DONE EXTRACTING                        
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RPTD0012                                                         
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RPTD0012                                                         
         CLC   KEY+20(5),QSTATION  SAME STATION?                                
         BNE   RPTD0084            NO  - DONE EXTRACTING                        
*                                                                               
RPTD0012 EQU   *                                                                
         OC    QOFFICE,QOFFICE     ANY OFFICE ENTERED?                          
         BZ    RPTD0016                                                         
         CLC   QOFFICE(2),SPACES   DITTO LAST COMMENT                           
         BE    RPTD0016                                                         
         CLC   KEY+25(2),QOFFICE   SAME OFFICE?                                 
         BNE   RPTD0080            NO  - SKIP THIS OFFICE                       
RPTD0016 EQU   *                                                                
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
*                                                                               
RPTD0020 EQU   *                                                                
         CLC   KEY+20(5),SAVESTA   STATION PREVIOUSLY SEEN?                     
         BE    RPTD0024            YES - SKIP REREADING IT                      
         MVC   SAVESTA(5),KEY+20   SAVE STATION                                 
*                                                                               
*   RETRIEVE STATION RECORD FOR STATION NAME                                    
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION KEY                        
         MVI   KEY,X'02'                                                        
         MVC   KEY+20,RCREPFL                                                   
         MVC   KEY+22(5),SAVESTA   INSERT STATION                               
         GOTO1 HIGH                                                             
         CLC   KEY+20(2),RCREPFL   RECORD FOUND CHECKING                        
         BNE   RDDUMP              NOT FOUND - DUMP IT OUT                      
         CLC   SAVESTA(4),=C'C*MP'                                              
         BE    RPTD0080                                                         
         CLC   KEY+22(5),SAVESTA                                                
         BNE   RDDUMP                                                           
         BAS   RE,GETSTA           RETURN STATION RECORD                        
         MVC   SAVEGRUP,RSTAGRUP   SAVE GROUP FROM STATION RECORD               
         MVC   SAVEMKT,RSTAMKT     SAVE MARKET NAME                             
         MVC   SAVETVB,RSTATVB     SAVE TVB REGION                              
         MVC   SAVEOWNR,RSTAOWN    SAVE OWNERSHIP                               
         MVI   STAACTIV,C'Y'       SET STATION ACTIVE FLAG ON                   
         OC    RSTAEND,RSTAEND     ANY DATE ENTERED FOR END?                    
         BZ    RPTD0024            NO  - ACTIVE STATION                         
         MVI   STAACTIV,C'N'       YES - SET FLAG OFF                           
         BAS   RE,STATLEFT         ADD STATION TO 'LEFT LIST'                   
*                                                                               
RPTD0024 EQU   *                                                                
         CLC   QGROUP(1),SPACES        ANY GROUP FILTER?                        
         BE    RPTD0044                NO  - SKIP TEST                          
         CLC   QGROUP(1),RSTAGRUP      YES - BUDGET FOR GRP?                    
         BNE   RPTD0076                NO  - NO SORT OUTPUT                     
RPTD0044 EQU   *                                                                
         CLC   QSBGROUP(1),SPACES      ANY SUBGROUP FILTER?                     
         BE    RPTD0045                NO  - SKIP TEST                          
         CLC   QSBGROUP(1),RSTAGRUP+1  YES - BUDGET FOR SUBGRP?                 
         BNE   RPTD0076                NO  - NO SORT OUTPUT                     
RPTD0045 EQU   *                                                                
         L     R1,ADCONLST                                                      
         L     RE,MASTC-ADCONSD(R1)                                             
         LA    RF,MCIO-MASTD(,RE)                                               
         CLC   80(80,RF),SPACES    ANY CARD2 FILTERS?                           
         BE    RPTD0048            NO                                           
         LA    RF,80(RF)           BUMP TO SECOND CARD                          
         USING QREC2D,RF                                                        
         CLC   Q2TVB(2),SPACES         ANY TVB REGION FILTER?                   
         BE    RPTD0046                NO  - SKIP TEST                          
         CLC   Q2TVB(2),RSTATVB        YES - SAME TVB REGION?                   
         BNE   RPTD0076                NO  - NO SORT OUTPUT                     
RPTD0046 EQU   *                                                                
         CLC   Q2OWNER(2),SPACES       ANY OWNERSHIP  FILTER?                   
         BE    RPTD0048                NO  - SKIP TEST                          
         CLC   Q2OWNER(2),RSTAOWN      YES - SAME OWNERSHIP?                    
         BNE   RPTD0076                NO  - NO SORT OUTPUT                     
*                                                                               
         DROP  RF                                                               
*                                                                               
*  RETRIEVE OFFICE RECORD FOR OFFICE NAME: OFFICE IS MINOR, MUST BE             
*        READ FOR EACH RECORD PROCESSED                                         
*                                                                               
RPTD0048 EQU   *                                                                
         XC    KEY,KEY                   ESTABLISH OFFICE KEY                   
         CLC   KEYSAV2+25(2),=X'0000'    ANY OFFICE ENTERED?                    
         BNE   RPTD0052                                                         
         MVC   SAVEOFF(20),SPACES                                               
         MVC   SAVEOFF(09),=C'CORPORATE'                                        
         B     RPTD0056                                                         
RPTD0052 EQU   *                                                                
         MVI   KEY,X'04'                                                        
         MVC   KEY+23,RCREPFL                                                   
         MVC   KEY+25(2),KEYSAV2+25       INSERT OFFICE FROM BUD REC            
         GOTO1 HIGH                                                             
         CLC   KEY+23(2),RCREPFL   RECORD FOUND CHECKING                        
         BNE   RDDUMP              NOT FOUND - DUMP IT OUT                      
         CLC   KEY+25(2),KEYSAV2+25                                             
         BNE   RDDUMP                                                           
         BAS   RE,GETOFF           RETURN OFFICE RECORD                         
         MVC   SAVEOFF,ROFFNAME    SAVE OFFICE NAME                             
*                                                                               
RPTD0056 EQU   *                                                                
         MVC   KEY(27),KEYSAV2     RESET BUDGET KEY                             
         GOTO1 READ                REESTABLISH KEY                              
         BAS   RE,GETBUD           RETRIEVE BUDGET RECORD                       
         CLI   QOPTION2,C'T'       BUDGET BY TYPE?                              
         BNE   RPTD0058            NO                                           
         BAS   RE,OUTBYTYP                                                      
         B     RPTD0076            CONTINUE PROCESSING                          
RPTD0058 EQU   *                                                                
         GOTO1 =A(GETALLOC),DMCB,(RC),RR=RELO EXTRACT ALLOCATED $ FOR           
*                                  REQUESTED TYPE OR COMBINED                   
         CLI   INCFLAG,C'N'        FOUND?                                       
         BE    RPTD0076            NO  - NO OUTPUT                              
*                                                                               
         BAS   RE,OUTSORT2                                                      
RPTD0076 EQU   *                                                                
         MVC   KEY(27),KEYSAV2                RESTORE BUDGET KEY                
         GOTO1 READ                                                             
RPTD0080 EQU   *                                                                
         GOTO1 SEQ                 REESTABLISH SEQUENTIAL ACCESSING             
         B     RPTD0008            PROCESS THIS READ                            
*                                                                               
RDDUMP   DC    H'0'                                                             
*                                                                               
RPTD0084 EQU   *                                                                
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
*   THE SORTED FILE WILL CONTAIN UP TO 2 RECORDS FOR EACH                       
*   GROUP/STATION/OFFICE/YEAR/MONTH.  THE FIRST IS THE BUDGET RECORD,           
*   WHICH HAS A SUBTYPE OF 0.  THIS RECORD CONTAINS THE ALLOC-                  
*   ATION $, IF REQUESTED.  THE SECOND IS THE RECORD CONTAINING                 
*   THE CONTRACT DOLLARS, WHICH HAS A SUBTYPE OF 1.  EITHER                     
*   RECORD MAY BE MISSING.  THE REPORT MUST IDENTIFY THESE                      
*   CONDITIONS FOR THE USER.                                                    
*                                                                               
         XC    SORTREC2,SORTREC2                                                
         CLI   QOPTION2,C'T'       BUDGET ALLOCATIONS (BY TYPE) ONLY?           
         BNE   RPTD0086                                                         
         MVI   SUBPRG,0            YES - SET INCREMENT                          
         B     RPTD0096                                                         
*                                                                               
RPTD0086 EQU   *                                                                
         CLI   QOPTION2,C'A'       BUDGET ALLOCATIONS ONLY?                     
         BNE   RPTD0088                                                         
         MVI   SUBPRG,0            YES - SET INCREMENT                          
         B     RPTD0096                                                         
*                                                                               
RPTD0088 EQU   *                                                                
         CLI   QOPTION2,C'B'       BILLING FIGURES ONLY?                        
         BNE   RPTD0092                                                         
         MVI   SUBPRG,1            YES - SET INCREMENT                          
         B     RPTD0096                                                         
*                                                                               
RPTD0092 EQU   *                                                                
         CLI   QOPTION2,C'C'       ALLOC + BILLING FIGURES?                     
         BNE   RPTD0096                                                         
         MVI   SUBPRG,2            YES - SET INCREMENT                          
*                                                                               
RPTD0096 EQU   *                                                                
         BAS   R8,GETSORT                                                       
         CLI   STYP,X'FF'          EOF                                          
         BE    RPTD0184            YES                                          
         OC    SORTREC2,SORTREC2                                                
         BZ    RPTD0128            FIRST TIME IF ALL ZERO                       
         CLC   STYP,SRECTYP2       SAME RECORD TYPE?                            
         BE    RPTD0104            YES                                          
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         GOTO1 =A(GRDTOT),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         BAS   RE,COMPTOTL         PRODUCE COMPANY TOTALS                       
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
*                                                                               
*  STEP ON PREVIOUS KEY TO ENSURE KEYBREAK ON TYPE BREAK                        
*                                                                               
         MVC   SORTREC2+1(10),SPACES                                            
         B     RPTD0128                                                         
*                                                                               
RPTD0104 EQU   *                                                                
*                                                                               
*    BREAK TESTING:   GROUP BREAK                                               
*                                                                               
         CLC   SORTREC+SGROUP(2),SORTREC2+SGROUP                                
         BE    RPTD0108                                                         
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         GOTO1 =A(GRDTOT),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         B     RPTD0124                                                         
*                                                                               
RPTD0108 EQU   *                                                                
*                                                                               
*    BREAK TESTING:   STATION BREAK                                             
*                                                                               
         CLI   STYP,C'1'                                                        
         BNE   RPTD0116                                                         
*                                                                               
*   IF OFFICE = ZZ, FORCE SUBTOTAL BREAK                                        
*                                                                               
         CLC   SORTREC+SOFF1(2),=C'ZZ'                                          
         BNE   RPTD0112                                                         
*                                                                               
RPTD0112 EQU   *                                                                
         CLC   SORTREC+SOFF1(2),SORTREC2+SOFF1                                  
         BE    RPTD0114                                                         
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
*                                                                               
RPTD0114 CLC   SORTREC+SSTAT1(5),SORTREC2+SSTAT1                                
         BE    RPTD0124                                                         
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         MVI   PRTFLAG,C'Y'        TURN OON PRINT FLAG                          
         GOTO1 =A(GRDTOT),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         B     RPTD0124                                                         
*                                                                               
RPTD0116 EQU   *                                                                
*                                                                               
*    BREAK TESTING:   OFFICE BREAK                                              
*                                                                               
*                                                                               
*   IF STATION = ZZZZ, FORCE SUBTOTAL BREAK                                     
*                                                                               
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'                                       
         BNE   RPTD0120                                                         
RPTD0120 EQU   *                                                                
         CLC   SORTREC+SSTAT2(5),SORTREC2+SSTAT2                                
         BE    RPTD0122                                                         
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
*                                                                               
RPTD0122 CLC   SORTREC+SOFF2(2),SORTREC2+SOFF2                                  
         BE    RPTD0124                                                         
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         MVI   PRTFLAG,C'Y'        TURN ON FLAG                                 
         GOTO1 =A(GRDTOT),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
*                                                                               
RPTD0124 EQU   *                                                                
*        MVC   P+0(26),=C'REC1=                REC2='                           
*        MVC   P+5(15),SORTREC+1   **TEST**                                     
*        MVC   P+26(15),SORTREC2+1 **TEST**                                     
*        GOTO1 REPORT                                                           
         CLC   SORTREC+1(14),SORTREC2+1 GRP/STA/OFF/CONTYP = ?                  
         BNE   RPTD0128                 NO                                      
         MVI   ACCFLAG,C'Y'        ACCUMULATE TOTALS                            
         CLI   SUBPRG,0            BUDGET ALLOCATION ONLY REQUEST?              
         BE    RPTD0126            YES - NO CONTRACT DOLLARS DISPLAYED          
         GOTO1 =A(CONDOLRS),DMCB,(RC),RR=RELO                                   
*                                  YES - IT IS A CONTRACT $ RECORD              
RPTD0126 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         B     RPTD0180                                                         
*                                                                               
RPTD0128 EQU   *                                                                
         BAS   R8,COMMPRNT         LOAD COMMON FIELDS                           
         ZIC   R3,SUBPRG           COMPUTE REPORT HEADING #                     
         ZIC   R6,RCSUBPRG                                                      
         AR    R3,R6                                                            
         STC   R3,RCSUBPRG         RESET HEADER CONTROL                         
         CLI   SSUBTYP,C'0'        CONTRACT OR BUDGET?                          
         BNE   RPTD0160            NOT ZERO = CONTRACT                          
*                                                                               
*   LOAD INFORMATION FROM 'BUDGET' RECORD INPUT                                 
*                                                                               
         MVC   P+PTYPE(1),SORTREC+SCON2     DISPLAY TYPE (IF ANY)               
         LA    R3,P+PCOL1          A(ON COMBINED REPORT)                        
         CLI   SUBPRG,1            BILLING REVENUE REPORT?                      
         BE    RPTD012A            YES - DON'T DISPLAY BUDGET $                 
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD                      
         EDIT  (4,(R6)),(13,(R3)),COMMAS=YES,ZERO=NOBLANK                       
*                                                                               
*    RUN TOTALS FOR ALLOCATION $ FROM BUDGET RECORD                             
*                                                                               
RPTD012A EQU   *                                                                
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,TOTLALL$         ACCUMULATOR FOR BUDGET REV $                 
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R4,TSTLALL$         ACCUMULATOR FOR ALLOC $ (MONTH)              
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,COMPALL$         ACCUMULATOR FOR ALLOC $  (YEAR)              
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK (YEAR)                      
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R4,COMPTAL$         ACCUMULATOR FOR ALLOC $ (MONTH)              
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE ALLOC $                                 
         CLI   STYP,C'1'                                                        
         BNE   RPTD0129                                                         
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   RPTD0131            NO                                           
         B     RPTD0130            YES                                          
RPTD0129 EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'   STATION=ZZZZ?                       
         BNE   RPTD0131            NO                                           
RPTD0130 EQU   *                                                                
         LA    R6,ZZALL$           YES - ACCUMULATE ZZ ALLOC (YEAR)             
         LA    R3,ZZTAL$           YES - ACCUMULATE ZZ ALLOC (MONTH)            
         LA    R5,TOTZZALL         ALSO  ACCUMULATE TOT ZZ ALLOC                
         LA    R4,TSTZZALL                                                      
         B     RPTD0132                                                         
RPTD0131 EQU   *                                                                
         LA    R6,NOZZALL$    ACCUMULATE IN NOT ZZ ALLOCATION (YR)              
         LA    R3,NOZZTAL$    ACCUMULATE IN NOT ZZ ALLOCATION (MONTH)           
         LA    R5,TNOZZALL         ALSO  ACCUMULATE TOT NOT ZZ ALLOC            
         LA    R4,TNSZZALL         ALSO  ACCUMULATE TOT NOT ZZ ALLOC            
RPTD0132 EQU   *                                                                
         LR    R8,R0                                                            
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT (YEAR)              
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,RF                                                            
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT (MONTH)             
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R4,R3                                                            
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R8,0(R4)            ADD ACCUM TO BUD REC AMT (MONTH)             
         ST    R8,0(R4)            STORE TOTAL BACK (MONTH)                     
*                                                                               
         LA    R3,P+PCOL3COM       RATE: COMBINED REPORT                        
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    RPTD0133            YES                                          
         LA    R3,P+PCOL2COM       RATE: BUDGET OR BILLING REPORT               
RPTD0133 EQU   *                                                                
         LA    R6,SORTREC+SCOMM          COMM RATE FROM BUD RECORD              
         EDIT  (4,(R6)),(10,(R3))                                               
         MVC   GENWORK(4),6(R3)          EDIT 4 DECL PLACE RATE                 
         MVC   7(4,R3),GENWORK                                                  
         MVI   6(R3),C'.'                INSERT DECIMAL POINT                   
         CLI   SORTREC+SDEFCOM,C'Y'      DEFAULT RATE USED?                     
         BNE   RPTD0134                  NO  - DON'T FLAG                       
         MVI   11(R3),C'*'               YES - FLAG ON REPORT                   
RPTD0134 EQU   *                                                                
         SR    R3,R3               CLEAR FOR ACCUMULATION                       
         MVC   CONTOTS(4),SORTREC+SALL$                                         
         OC    CONTOTS(4),CONTOTS                                               
         BZ    RPTD0140            ENSURE NO ZERO DIVIDE                        
         L     R3,CONTOTS          ALLOCATION $ IN REGS 2,3                     
         MVC   CONTOTS(4),SORTREC+SCOMM                                         
         OC    CONTOTS(4),CONTOTS                                               
         BNZ   RPTD0136            ENSURE NO ZERO DIVIDE                        
         SR    R3,R3               NO COMMISSION: ZERO OUT FINAL RESULT         
         B     RPTD0160            NOW DO TOTALS                                
RPTD0136 EQU   *                                                                
         L     R4,CONTOTS          COMMISH RATE IN REG 4                        
         MR    R2,R4               MULTIPLY THEM                                
         LH    R4,=H'5000'                                                      
         AR    R3,R4               HALF ADD FOR ROUNDING                        
         LH    R4,=H'10000'                                                     
         DR    R2,R4               DECIMAL ALIGN                                
         LA    R6,P+PCOL4          BUDGET REVENUE: COMBINED REPORT              
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    RPTD0138            YES                                          
         LA    R6,P+PCOL3          BUDGET REVENUE: BUDGET ONLY                  
         CLI   SUBPRG,0            BUDGET REPORT?                               
         BNE   RPTD0140            NO  - DON'T PRINT BUDGET REVENUE             
RPTD0138 EQU   *                                                                
         EDIT  (R3),(13,(R6)),2,COMMAS=YES                                      
*                                                                               
*   CALCULATED REVENUE IS IN 'R3' AT THIS POINT - IT IS OPPORTUNE TO            
*      SAVE IT BEFORE GOING FURTHER                                             
*                                                                               
*                                                                               
*    RUN TOTALS FOR BUDGET REV $ FROM BUDGET RECORD                             
*                                                                               
RPTD0140 EQU   *                                                                
         ST    R3,INDEX1           SAVE BUDGET REV $ FOR INDEX CALC             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R6,TOTLBUD$         ACCUMULATOR FOR BUDGET REV $                 
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R4,TSTLBUD$         ACCUMULATOR FOR BUDGET REV $ (MONTH)         
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R6,COMPBUD$         ACCUMULATOR FOR BUDGET REV $ (YEAR)          
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R4,COMPTBD$         ACCUMULATOR FOR BUDGET REV $ (MONTH)         
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE BUDGET REV $                            
         CLI   STYP,C'1'                                                        
         BNE   RPTD0144                                                         
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   RPTD0152            NO                                           
         B     RPTD0148            YES                                          
RPTD0144 EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'   STATION=ZZZZ?                       
         BNE   RPTD0152            NO                                           
RPTD0148 EQU   *                                                                
         LA    R6,ZZBUD$    YES - ACCUMULATE ZZ BUD REV $ (YEAR)                
         LA    R3,ZZTBD$    YES - ACCUMULATE ZZ BUD REV $ (MONTH)               
         LA    R5,TOTZZBUD         ALSO  ACCUMULATE TOT ZZ BUD REV $            
         LA    R4,TSTZZBUD         ALSO  ACCUMULATE TOT ZZ BUD REV $            
         B     RPTD0156                                                         
RPTD0152 EQU   *                                                                
         LA    R6,NOZZBUD$  ACCUM IN NOT ZZ BUD REV $ (YEAR)                    
         LA    R3,NOZZTBD$  ACCUM IN NOT ZZ BUD REV $ (MONTH)                   
         LA    R5,TNOZZBUD         ALSO  ACCUM TOT NOT ZZ BUD REV $             
         LA    R4,TNSZZBUD         ALSO  ACCUM TOT NOT ZZ BUD REV $             
RPTD0156 EQU   *                                                                
         LR    R8,R0                                                            
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,RF                                                            
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK (MONTH)                     
         LR    R4,R3                                                            
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO GET CORRECT DISPLACEMENT           
         A     R8,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R8,0(R4)            STORE TOTAL BACK                             
         B     RPTD0180                                                         
*                                                                               
RPTD0160 EQU   *                                                                
         MVI   ACCFLAG,C'Y'        ACCUMULATE TOTALS                            
         CLI   SUBPRG,0            BUDGET ALLOCATION ONLY REQUEST?              
         BE    RPTD0166            YES - NO CONTRACT DOLLARS DISPLAYED          
         GOTO1 =A(CONDOLRS),DMCB,(RC),RR=RELO                                   
*                                  CONTRACT $ RECORD                            
RPTD0166 EQU   *                                                                
         MVC   P+1(09),=C'NO BUDGET'                                            
RPTD0168 EQU   *                                                                
*                                                                               
         GOTO1 REPORT                                                           
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         B     RPTD0180                                                         
*                                                                               
*    IF NO CALL TO REPORT IS MADE, PRINT-LINE MUST BE CLEARED                   
*                                                                               
RPTD0172 EQU   *                                                                
         MVC   P,SPACES            SPACE PRINT-LINE TO CLEAR                    
         CLI   PRTFLAG,C'X'        FIRST PASS/NO PRINT?                         
         BE    RPTD0176            YES - DON'T RESET FORCEHED                   
         MVI   FORCEHED,C'N'       TURN OFF PAGEBREAK                           
RPTD0176 EQU   *                                                                
         MVI   FORCEFUT,C'N'       TURN OFF FOOTBREAK                           
         MVC   SORTREC2(1),SORTREC SAVE RECORD TYPE ONLY                        
         B     RPTD0096                                                         
*                                                                               
RPTD0180 MVC   SORTREC2,SORTREC    SAVE PROCESSED KEY                           
         B     RPTD0096                                                         
*                                                                               
RPTD0184 CLI   SSUBTYP2,C'0'       LAST RECORD A BUDGET RECORD?                 
         BNE   RPTD0188            FINISHED - END JOB                           
         CLI   QOPTION2,C'T'       CONTRACT BY TYPE?                            
         BE    RPTD0186            YES - SKIP LINE DISPLAY                      
         MVC   P+1(11),=C'BUDGET ONLY'                                          
RPTD0186 EQU   *                                                                
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         GOTO1 REPORT              PRINT OUT LAST ITEM                          
         MVC   RESETREC,SORTREC    RESTORE CURRENT RECORD                       
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
RPTD0188 EQU   *                                                                
         GOTO1 =A(OFFTOT),DMCB,(RC),RR=RELO  PRODUCE OFFICE/STA TOTALS          
         MVI   FORCEHED,C'Y'                                                    
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         GOTO1 =A(GRDTOT),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         BAS   RE,COMPTOTL          PRODUCE COMPANY TOTALS                      
         BAS   RE,EXCEPTS          PRODUCE EXCEPTION LISTING                    
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   ESTABLISH COMMON PRINT FIELDS HERE                                          
*                                                                               
COMMPRNT BAS   RE,GETMONTH         GET EBCIDIC VALUE OF MONTH                   
         CLI   STYP,C'1'                 STATION/OFFICE REPORT?                 
         BNE   COMP0004                  NO                                     
         MVI   OFFFLAG,C'N'             SET OFF REP SWITCH OFF                  
         MVI   RCSUBPRG,0                     SET SUBPRG TO STATION             
         B     COMP0008                                                         
COMP0004 EQU   *                                                                
         MVI   RCSUBPRG,3                     SET SUBPRG TO OFFICE              
         MVI   OFFFLAG,C'Y'                   SET OFF REP SWITCH ON             
         CLI   STYP,C'2'                                                        
         BE    COMP0008                                                         
         MVI   RCSUBPRG,6                SET SUBPRG TO REGIONAL                 
         MVI   OFFFLAG,C'N'              SET OFF REP SWITCH OFF                 
COMP0008 EQU   *                                                                
         MVI   SPACING,2           ENTIRE REPORT IS DOUBLE SPACED               
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
* FIND EBCIDIC VALUE OF MONTH (MONTH IS IN SORTREC)                             
*                                                                               
GETMONTH NTR1                                                                   
         LA    R3,MTABLE                                                        
*                                                                               
GETM10   EQU   *                                                                
         CLC   0(2,R3),SORTREC+SMONTH    FOUND?                                 
         BE    GETM20              YES                                          
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    RDDUMP                                                           
         LA    R3,5(R3)            NEXT ENTRY IN THE TABLE                      
         B     GETM10                                                           
*                                                                               
GETM20   EQU   *                                                                
         MVC   P+PMONTH(3),2(R3)                                                
         MVI   P+PMONTH+3,C'/'                                                  
         MVC   P+PMONTH+4(2),SORTREC+SDATE                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  EXCEPTION LIST GENERATION ROUTINES:  STATIONS ARE ADDED TO LISTS             
*    FOR LATER PRINTING IF THEY A)  LEFT, OR  B)  HAD NO COMMISSION             
*    RATES, AND USED THE DEFAULT                                                
*                                                                               
STATLEFT NTR1                                                                   
         L     R5,ALEFSTAT         A(LAST SPACE USED)                           
         LA    R5,L'LEFSTATS(R5)   BUMP TO A(NEXT AVAILABLE SPACE)              
         CLC   0(5,R5),=C'ZZZZZ'   DELIMITER REACHED?                           
         BE    SLEF0099            YES  - NO MORE ROOM (TOUGH)                  
         MVC   0(5,R5),RSTAKSTA    SAVE STATION                                 
         ST    R5,ALEFSTAT         SAVE ADDRESS                                 
SLEF0099 EQU   *                                                                
         B     MODEEXIT                                                         
*                                                                               
*  GRAND/SUBTOTAL ROUTINES FOR PRINTING                                         
*                                                                               
*   NOTE:  STOTAL DISPLAY HAS NOT BEEN UPDATED!  NEW FIELDS AND                 
*          PRINT LINE ALIGNMENT IS NOT REFLECTED                                
*                                                                               
STOTAL   NTR1                                                                   
         CLC   SORTREC(10),ZZKEY   SAME KEY UP TO TYPE2?                        
         BE    STOT0099            YES - DON'T SHOW TOTS AGAIN                  
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         MVC   P+1(16),=C'SUB TOTAL ----->'                                     
         LA    R6,TOTLPRI$         PRIOR DOLLAR TOTAL                           
         LA    R3,PCOL1                                                         
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,PCOL4                                                         
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLALL$         ALLOCATION DOLLAR TOTAL                      
         LA    R3,P+87                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
STOT0099 EQU   *                                                                
         MVC   ZZKEY,SORTREC       SET DUPLICATE TEST                           
         B     MODEEXIT                                                         
         EJECT                                                                  
COMPTOTL NTR1                                                                   
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         MVC   SORTREC+1(9),SPACES                                              
         MVC   SORTREC+SMKTNM(20),=C'COMPANY TOTAL       '                      
         MVC   SORTREC+SOFFNM(20),=C'COMPANY TOTAL       '                      
         OC    ZZPRI$(196),ZZPRI$   ANY ZZ/ZZZZ TOTALS?                         
         BZ    COMT0046            NO                                           
         B     COMT0026            SKIP OFFICE ZZ TOTAL LINE                    
*                                                                               
*   OFFICE-ZZ/STATION-ZZZZ FIGURES                                              
*                                                                               
COMT0004 EQU   *                                                                
         LA    R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COMT0012            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    COMT0008            YES                                          
*                                  BUDGET REPORT                                
COMT0006 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,ZZTAL$           OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL           
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0007                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZTBD$           OFF=ZZ/STA=ZZZZ BUDGET REVENUE $             
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0007 EQU   *                   BILLING REPORT                               
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0006                                                         
* YEAR TOTALS                                                                   
         LA    R6,ZZALL$                                                        
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZBUD$           OFF=ZZ/STA=ZZZZ BUDGET REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0024                                                         
COMT0008 EQU   *                   BILLING REPORT                               
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,ZZTCR$           OFF=ZZ/STA=ZZZZ BILLG DOLLAR TOTAL           
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0009                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZTBL$           OFF=ZZ/STA=ZZZZ BILLNG REVENUE $             
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0009 EQU   *                   BILLING REPORT                               
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0008                                                         
* YEAR TOTALS                                                                   
         LA    R6,ZZCUR$           OFF=ZZ/STA=ZZZZ BILLG DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZBIL$           OFF=ZZ/STA=ZZZZ BILLNG REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0024                                                         
COMT0012 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
*                                                                               
         LA    R6,ZZTCR$           OFFICE ZZ CURRENT DOLLAR TOTAL               
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)       SKIP BLANK LINES                             
         BNZ   COMT0018                                                         
         LA    R6,ZZTAL$           OFFICE ZZ ALLOCATION DOLLAR TOTAL            
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   COMT0020                                                         
         B     COMT0022                                                         
COMT0018 EQU   *                                                                
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZTAL$           OFFICE ZZ ALLOCATION DOLLAR TOTAL            
         LA    R6,0(R4,R6)                                                      
COMT0020 EQU   *                                                                
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZTBD$           OFFICE ZZ BUDGET REVENUE $                   
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,ZZTBL$           OFFICE ZZ BILLING REVENUE $                  
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0022 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0012                                                         
* YEAR TOTALS                                                                   
         LA    R6,ZZCUR$           OFFICE ZZ CURRENT DOLLAR TOTAL               
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZALL$           OFFICE ZZ ALLOCATION DOLLAR TOTAL            
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,ZZBUD$           OFFICE ZZ BUDGET REVENUE $                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,ZZBIL$           OFFICE ZZ BILLING REVENUE $                  
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
COMT0024 EQU   *                                                                
         MVC   P+1(20),=C'OFFICE ZZ TOTAL---->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    *+10                                                             
         MVC   P+1(20),=C'STATION ZZZZ TOTAL->'                                 
         GOTO1 REPORT                                                           
COMT0026 EQU   *                                                                
*                                                                               
*   NON-OFFICE-ZZ/NON-STATION-ZZZZ FIGURES                                      
*                                                                               
         LA    R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COMT0036            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    COMT0032            YES                                          
COMT0028 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,NOZZTAL$         NON OFF ZZ      ALLOC DOLLAR TOTAL           
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0030                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZTBD$         NON OFF ZZ      BUDGET REVENUE $             
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0030 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0028                                                         
* YEAR TOTALS                                                                   
         LA    R6,NOZZALL$         NON OFF ZZ      ALLOC DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZBUD$         NON OFF ZZ      BUDGET REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0044                                                         
COMT0032 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,NOZZTCR$         NON OFF ZZ      BILLG DOLLAR TOTAL           
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0034                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZTBL$         NON OFF ZZ      BILLNG REVENUE $             
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0034 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0032                                                         
* YEAR TOTALS                                                                   
         LA    R6,NOZZCUR$         NON OFF ZZ      BILLG DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZBIL$         NON OFF ZZ      BILLNG REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0044                                                         
COMT0036 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,NOZZTCR$         NON OFFICE ZZ CURRENT DOLLAR TOTAL           
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   COMT0038                                                         
         LA    R6,NOZZTAL$         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   COMT0040                                                         
         B     COMT0042                                                         
COMT0038 EQU   *                                                                
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZTAL$         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
         LA    R6,0(R4,R6)                                                      
COMT0040 EQU   *                                                                
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZTBD$         NON OFFICE ZZ BUDGET REVENUE $               
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,NOZZTBL$         NON OFFICE ZZ BUDGET REVENUE $               
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0042 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0036                                                         
* YEAR TOTALS                                                                   
         LA    R6,NOZZCUR$         NON OFFICE ZZ CURRENT DOLLAR TOTAL           
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZALL$         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZBUD$         NON OFFICE ZZ BUDGET REVENUE $               
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,NOZZBIL$         NON OFFICE ZZ BUDGET REVENUE $               
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
COMT0044 EQU   *                                                                
         MVC   P+1(20),=C'ALL OTHER OFFICES-->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    *+10                                                             
         MVC   P+1(20),=C'ALL OTHER STATIONS->'                                 
         GOTO1 REPORT                                                           
*                                                                               
COMT0046 EQU   *                                                                
         LA    R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COMT0056            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    COMT0052            YES                                          
*                                  BUDGET REPORT                                
COMT0048 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,COMPTAL$         ALLOCATION DOLLAR TOTAL                      
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0050                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPTBD$         BUDGET REVENUE $                             
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0050 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0048                                                         
* YEAR TOTALS                                                                   
         LA    R6,COMPALL$         ALLOCATION DOLLAR TOTAL                      
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPBUD$         BUDGET REVENUE $                             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0064                                                         
COMT0052 EQU   *                   BILLING REPORT                               
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,COMPTCR$         BILLING DOLLAR TOTAL                         
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    COMT0054                                                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPTBL$         BILLING REVENUE $                            
         LA    R6,0(R4,R6)                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0054 EQU   *                   BILLING REPORT                               
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0052                                                         
* YEAR TOTALS                                                                   
         LA    R6,COMPCUR$         BILLING DOLLAR TOTAL                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPBIL$         BILLING REVENUE $                            
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     COMT0064                                                         
COMT0056 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,COMPTCR$         CURRENT DOLLAR TOTAL                         
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   COMT0058                                                         
         LA    R6,COMPTAL$         ALLOCATION DOLLAR TOTAL                      
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   COMT0060                                                         
         B     COMT0062                                                         
COMT0058 EQU   *                                                                
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPTAL$         ALLOCATION DOLLAR TOTAL                      
         LA    R6,0(R4,R6)                                                      
COMT0060 EQU   *                                                                
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPTBD$         BUDGET REVENUE $                             
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,COMPTBL$         BILLING REVENUE $                            
         LA    R6,0(R4,R6)                                                      
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
COMT0062 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    COMT0056                                                         
* YEAR TOTALS                                                                   
         LA    R6,COMPCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPALL$         ALLOCATION DOLLAR TOTAL                      
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,COMPBUD$         BUDGET REVENUE $                             
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,COMPBIL$         BILLING REVENUE $                            
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
COMT0064 EQU   *                                                                
         MVC   P+1(20),=C'COMPANY TOTAL------>'                                 
         GOTO1 REPORT                                                           
         XC    COMPPRI$(20),COMPPRI$   ZERO OUT ACCUMULATOR (YEAR)              
         XC    ZZPRI$(20),ZZPRI$       ZERO OUT ACCUMULATOR (YEAR)              
         XC    NOZZPRI$(20),NOZZPRI$   ZERO OUT ACCUMULATOR (YEAR)              
         XC    COMPTPR$(196),COMPTPR$   ZERO OUT ACCUMULATOR (MONTH)            
         XC    ZZTPR$(196),ZZTPR$       ZERO OUT ACCUMULATOR (MONTH)            
         XC    NOZZTPR$(196),NOZZTPR$   ZERO OUT ACCUMULATOR (MONTH)            
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
         B     MODEEXIT                                                         
         SPACE 3                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
TOTEDIT  EQU   *                                                                
         EDIT  (4,(R6)),(13,(R3)),COMMAS=YES                                    
         BR    R8                  RETURN                                       
*                                                                               
TOTEDDEC EQU   *                                                                
         EDIT  (4,(R6)),(13,(R3)),2,COMMAS=YES                                  
         BR    R8                  RETURN                                       
         EJECT                                                                  
*                                                                               
*   CALCULATE INDEX OF PROJECTED REV $ TO BILLING REV $                         
*                                                                               
INDXCALC NTR1                                                                   
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         OC    INDEX2,INDEX2       ANY BILLING REVENUE?                         
         BZ    INDX0004            NO  - DISPLAY ZERO                           
         L     R3,INDEX2           LOAD BILLING REVENUE                         
         LH    R4,=H'10000'        SET DECIMAL ALIGNMENT                        
         MR    R2,R4               MULT BILL REV$ BY 100                        
         OC    INDEX1,INDEX1       ANY BUDGET REVENUE?                          
         BZ    INDX0008            NO  - EXIT ROUTINE                           
         L     R4,INDEX1           YES - LOAD BUDGET REVENUE                    
         SRL   R4,1                DIVIDE BY 2                                  
         AR    R3,R4               HALF-ADD FOR ROUNDING                        
         L     R4,INDEX1           RELOAD BUDGET REVENUE                        
         DR    R2,R4               DIVIDE BILLING BY BUDGET                     
INDX0004 EQU   *                                                                
         LA    R6,P+125                                                         
         EDIT  (R3),(6,(R6)),2,COMMAS=YES                                       
INDX0008 EQU   *                                                                
         XC    INDEX1,INDEX1                                                    
         XC    INDEX2,INDEX2                                                    
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS:  INSERT RATE INTO EACH UNIQUE STATION FOR             
*     THESE RECORDS, WHICH CONTAIN THE CONTRACT DOLLARS                         
*                                                                               
SORTGEN  NTR1                                                                   
         MVC   SAVESTA(5),SORTREC+SSTAT1                                        
         NI    FLAG,X'FF'-X'40'                                                 
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         CLI   CONFLAG,C'Y'        CONFLAG SET?                                 
         BNE   SORG0080            NO  - DON'T PUT OUT RECORD ON BREAK          
         MVI   CONFLAG,C'N'        RESET FLAG TO NO                             
*                                                                               
SORG0012 EQU   *                                                                
         CLC   QUESTOR(4),=C'<UHR'         REPLACE NETWORK WITH ZZ?             
         BNE   SORG0020                    NO                                   
         CLC   SORTREC+SSTAT1(4),=C'KDKA'  REPLACE STATION?                     
         BNE   SORG0016                                                         
         MVC   SORTREC+SSTAT1(4),=C'ZZZZ'                                       
SORG0016 EQU   *                                                                
         CLC   SORTREC+SOFF1(2),=C'SL'     REPLACE OFFICE?                      
         BNE   SORG0020                                                         
         MVC   SORTREC+SOFF1(2),=C'ZZ'                                          
SORG0020 EQU   *                                                                
         LA    R3,INVTBL           TABLE OF MONTHLY INV $                       
         ZIC   R4,BUCKCTR          NUMBER OF BUCKETS                            
         XC    SVDATE,SVDATE                                                    
*                                                                               
*                                                                               
SORG0022 EQU   *                                                                
*        OC    0(4,R3),0(R3)       SOMETHING IN THE BUCKET?                     
*        BZ    SORG0075            NO - DON'T NEED TO SORT                      
*                                                                               
         BAS   RE,NOTZREC2                                                      
*                                                                               
SORG0075 EQU   *                                                                
         LA    R3,8(R3)            NEXT BUCKET                                  
         BCT   R4,SORG0022                                                      
*                                                                               
SORG0080 EQU   *                                                                
         TM    FLAG,X'40'                                                       
         BZ    SORG0090                                                         
         MVC   KEY(27),KEYSAV2                                                  
         GOTO1 READ                                                             
SORG0090 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
NOTZREC2 NTR1                                                                   
         MVC   SORTREC+SDATE(4),4(R3)           YEAR/MONTH                      
         MVI   SORTREC,C'1'                     SET STATION MAJOR TYPE          
         MVI   SORTREC+STYP2,C'1'               SET SUB-TYPE                    
         MVC   SORTREC+SCURR(4),0(R3)           INSERT CURRENT BEST $           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),6(R3)        MOVE MONTH                                   
         MVI   DUB+2,C'/'                                                       
         MVC   DUB+3(2),4(R3)      YEAR                                         
         GOTO1 =V(PERVAL),DMCB,(5,DUB),BLOCK                                    
         TM    DMCB+4,X'01'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   RATEYEAR(2),BLOCK+28   BINARY DATE                               
*                                                                               
*  COMMISSION RATE LOGIC:  THE COMMISSION RATE IS TO BE TAKEN FROM              
*                                                                               
         CLC   SVDATE,RATEYEAR     IS IT THE SAME RATE?                         
         BH    SORG0072            YES - RECORD HAS ALREADY BEEN READ           
         MVI   DEFCOMM,C'Y'        SET 'DEFAULT COMMISSION USED' FLAG           
         MVC   SAVERATE,=F'127500' SET DEFAULT COMMISSION RATE                  
         OI    FLAG,X'40'          WILL NEED TO RESTART THE KEY                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'29'           GET COMMISSION RATE KEY                      
         MVC   KEY+11(2),RCREPFL                                                
         MVC   KEY+13(5),SAVESTA   INSERT STATION                               
         MVC   KEYSAV3(18),KEY     SAVE THIS KEY                                
         GOTO1 HIGH                                                             
SORG0060 EQU   *                                                                
         CLC   KEYSAV3(18),KEY        RECORD FOUND?                             
         BNE   SORG0070               NO  - USE CONTENTS OF SAVERATE            
         OC    KEY+18(7),KEY+18       OFFICE, ADV, OR TYPE IN KEY?              
         BNZ   SORG0065               YES - SKIP IT                             
         CLC   KEY+25(2),RATEYEAR     BUDGET YEAR VS KEY YEAR                   
         BH    SORG0070               KEYDATE > BUDYEAR: FINISHED               
         BAS   RE,GETCOM              GET COMMISSION RATE RECORD                
         MVC   SAVERATE(4),RCOMELEM+2 SAVE RATE                                 
         MVI   DEFCOMM,C'N'        SET DEFAULT COMM NOT USED' FLAG              
*                                                                               
*   RECORD LAYOUT FULL-WORD ALIGNS RCOMRAT1.  RCOMELEM+2 IS THE                 
*        CORRECT FIELD FOR THE RATE                                             
*                                                                               
SORG0065 EQU   *                                                                
         MVC   SVDATE,KEY+25       DATE FOR DEFAULT RATE                        
         GOTO1 SEQ                    GET NEXT COMMISSION RECORD                
         B     SORG0060               RETURN TO TEST IT                         
SORG0070 EQU   *                                                                
         CLI   DEFCOMM,C'Y'                                                     
         BE    *+10                                                             
         MVC   SVDATE,KEY+25        DATE - IF IT NOT DEFAULT RATE USED          
SORG0072 EQU   *                                                                
         MVC   SORTREC+SCOMM(4),SAVERATE        INSERT RATE FOR STATION         
         MVC   SORTREC+SDEFCOM(1),DEFCOMM       DEF COMM USED/NOT USED          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   RESETREC,SORTREC                 SAVE FOR RESETTING              
*        MVC   P+5(07),=C'SORTGEN' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        EDIT  (4,SAVERATE),(10,P+50),COMMAS=YES,MINUS=YES                      
*        GOTO1 REPORT              **TEST**                                     
         MVI   SORTREC,C'2'                     SET OFFICE MAJOR TYPE           
         MVC   SORTREC+SGROUP(2),SPACES         NO GRP FOR OFF ORDER            
         MVC   SWAPSTA(5),SORTREC+SSTAT1        SWITCH STATION/OFFICE           
         MVC   SORTREC+SOFF2(2),SORTREC+SOFF1   MOVE OFFICE MAJOR               
         MVC   SORTREC+SSTAT2(5),SWAPSTA        MOVE STATION MINOR              
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'3'        SET UP FOR REGIONAL OUTPUT                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*        MVC   P+5(07),=C'SORTGEN' **TEST**                                     
*        MVC   P+15(11),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
         MVC   SORTREC,RESETREC           RESET TYPE 1 SORT RECORD              
         AP    SORTCTR,=P'3'              ADD TO SORTED RECORDS                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  CLI   STYP,X'FF'          EOF REACHED?                                 
         BER   R8                  YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZR   R8                  TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(80),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
                                                                                
*                                                                               
* IF THIS RECORD IS A BUDGET, AND PREVIOUS RECORD WAS A BUDGET,                 
*     THE PREVIOUS RECORD MUST BE PRINTED BEFORE THE NEW ONE                    
*     CAN BE PROCESSED.  THE FIELDS ARE SWAPPED SO HEADER CORRECTLY             
*     DISPLAYS VALUES.                                                          
*                                                                               
*   FOR 'CONTRACT BY TYPE' REPORT, IGNORE ALL CONTRACT $ RECORDS                
*                                                                               
         CLI   QOPTION2,C'T'       CONTRACT BY TYPE?                            
         BNE   GS012               NO                                           
         CLI   SORTREC+STYP2,C'1'  CONTRACT RECORD?                             
         BE    GETSORT             YES - SKIP CONTRACT RECORDS                  
GS012    EQU   *                                                                
*        CLI   SORTREC+STYP2,C'1'  **TEST**                                     
*        BNE   GSTEST              **TEST**                                     
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(80),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GSTEST   EQU   *                                                                
         CLI   QOPTION1,C'A'       ALL REPORTS?                                 
         BE    GS020               YES                                          
         CLI   QOPTION1,C'S'       STATION ONLY?                                
         BNE   GS018               NO  - CHECK OFFICE OR REGIONAL               
         CLI   STYP,C'1'           YES - STATION RECORD?                        
         BE    GS020               YES                                          
         B     GETSORT             RETURN                                       
GS018    EQU   *                                                                
         CLI   QOPTION1,C'O'       OFFICE ONLY?                                 
         BNE   GS019               NO  - CHECK REGIONAL                         
         CLI   STYP,C'2'           YES - OFFICE RECORD?                         
         BE    GS020               YES                                          
         B     GETSORT             RETURN                                       
GS019    CLI   STYP,C'3'           REGIONAL RECORD?                             
         BNE    GETSORT            NO - SKIP IT                                 
GS020    EQU   *                                                                
         CLC   SORTREC(15),SORTREC2  SAME RECORD GROUP, INCL RECTYP             
         BE    GS025               YES                                          
         CLI   SSUBTYP2,C'0'       PREVIOUS RECORD BUDGET?                      
         BNE   GS025               NO  -                                        
         MVI   SSUBTYP2,C'9'       INDICATE BUDGET RECORD PRINTED               
         MVC   RESETREC,SORTREC    IN CASE THIS HAPPENS AT A PAGE               
         MVC   SORTREC,SORTREC2    BREAK, VALUES FOR BREAK MUST BE              
         CLI   QOPTION2,C'T'       CONTRACT BY TYPE?                            
         BE    GS023               YES - SKIP FLAG                              
         MVC   P+1(11),=C'BUDGET ONLY'                                          
GS023    EQU   *                                                                
         GOTO1 REPORT                                                           
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         MVC   SORTREC,RESETREC    RESET FOR PRINTING                           
GS025    EQU   *                                                                
         BR    R8                                                               
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
*                                                                               
         CLI   QOPTION2,C'T'       CONTRACT BY TYPE?                            
         BNE   AH004               NO                                           
         MVC   HEAD7+34(3),=C'TYP' YES - SET 'TYPE' COLUMN HEADING              
AH004    EQU   *                                                                
         CLI   QRGPROG,C'9'        BUDGET YEAR IN '90'S?                        
         BE    AH006               YES - IGNORE PRIOR TO 90'S                   
         MVC   HEAD1+39(2),=C'20'  NO  - MOVE IN NEXT CENTURY                   
         B     AH008                                                            
AH006    EQU   *                                                                
         MVC   HEAD1+39(2),=C'19'                                               
AH008    EQU   *                                                                
         MVC   HEAD1+41(2),QRGPROG LOAD BUDGET YEAR                             
         CLI   STYP,C'1'           TYPE 1?                                      
         BNE   AH010               TYPE 2 OR TYPE 3                             
         MVC   HEAD4+11(2),SORTREC+SGROUP                                       
         MVC   HEAD5+15(5),SORTREC+SSTAT1                                       
         MVC   HEAD5+24(20),SORTREC+SMKTNM                                      
         MVC   HEAD6+17(2),SORTREC+SOFF1                                        
         MVC   HEAD6+26(20),SORTREC+SOFFNM                                      
         B     MODEEXIT                                                         
AH010    EQU   *                                                                
         MVC   HEAD5+15(2),SORTREC+SOFF2                                        
         MVC   HEAD5+24(20),SORTREC+SOFFNM                                      
         MVC   HEAD6+17(5),SORTREC+SSTAT2                                       
         MVC   HEAD6+26(20),SORTREC+SMKTNM                                      
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   DISPLAY ENTRIES IN EXCEPTION TABLES.  BECAUSE LOADING TABLE                 
*        STARTS WITH AN ADDRESS ON ENTRY BEFORE THE ACTUAL FIRST                
*        POSITION, AN EMPTY TABLE REFLECTS THIS SITUATION                       
*                                                                               
EXCEPTS  NTR1                                                                   
         MVC   SORTREC,SPACES      SPACE OUT SORTREC FOR PRINT                  
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         MVI   RCSUBPRG,9          SET REPORT #                                 
         LA    R5,LEFSTATS         ANY ENTRIES IN TABLE                         
         L     R6,ALEFSTAT                                                      
         CR    R5,R6               LEFSTATS > ALEFSTAT = NO ENTRIES             
         BNH   EXC0010                                                          
         MVC   P+1(35),=C'NO DEPARTED STATIONS TO BE REPORTED'                  
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         B     EXC0050             GO TO PRINT NO-COMMS SECTION                 
EXC0010  EQU   *                                                                
         CR    R5,R6               CHECK FOR LAST TABLE ENTRY                   
         BH    EXC0050             REACHED                                      
         MVC   P+20(5),0(R5)       MOVE TABLE ENTRY TO PRINT                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R5,L'LEFSTATS(R5)   BUMP TO NEXT ENTRY                           
         B     EXC0010             GO BACK FOR NEXT                             
EXC0050  EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE CHANGE                            
         MVI   RCSUBPRG,10         SET REPORT #                                 
         L     R5,SNOCOMMS          ANY ENTRIES IN TABLE                        
         L     R6,ANOCOMMS                                                      
         CR    R5,R6               NOCOMMS > ANOCOMMS = NO ENTRIES              
         BNH   EXC0060                                                          
         MVC   P+1(33),=C'NO STATIONS USED THE DEFAULT RATE'                    
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         B     EXC0099             FINISHED                                     
EXC0060  EQU   *                                                                
         CR    R5,R6               CHECK FOR LAST TABLE ENTRY                   
         BH    EXC0099             REACHED                                      
         MVC   P+20(5),0(R5)       MOVE TABLE ENTRY TO PRINT                    
         CLC   5(2,R5),=X'0000'    ANY OFFICE ENTERED?                          
         BE    EXC0064             NO                                           
         MVC   P+28(2),5(R5)       YES - MOVE IT TO PRINT                       
EXC0064  EQU   *                                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R5,L'NOCOMMS(R5)    BUMP TO NEXT ENTRY                           
         B     EXC0060             GO BACK FOR NEXT                             
EXC0099  EQU   *                                                                
         MVC   P+53(25),=C'****  END OF REPORT  ****'                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
*   DEVELOP SORT RECORDS FOR EACH OF THE CONTRACT TYPES IN THE                  
*     BUDGET RECORD                                                             
*                                                                               
OUTBYTYP NTR1                                                                   
         LA    R6,RBUDREC          A(BUDGET RECORD)                             
         MVI   ELCODE,X'02'        BUDGETS BY CONTRACT TYPE?                    
         BAS   RE,GETEL            RETRIEVE TYPE 02 ELEMENT                     
         BE    OBYT0008            FOUND - SKIP TYPE 01 ELEMENT                 
         LA    R6,RBUDELEM         HANDLE BASIC (01) ELEMENT                    
         XC    SAVEALL$,SAVEALL$                                                
         GOTO1 =A(GA100),DMCB,(RC),RR=RELO                                      
         BAS   RE,OUTSORT          PUT OUT SORT REC FOR BASIC                   
         B     OBYT0020            EXIT THIS ROUTINE                            
OBYT0008 EQU   *                                                                
         XC    SAVEALL$,SAVEALL$                                                
         GOTO1 =A(GA100),DMCB,(RC),RR=RELO                                      
         BAS   RE,OUTSORT          PUT OUT SORT RECORDS BY TYPE                 
         BAS   RE,NEXTEL           TRY FOR NEXT '02' ELEMENT                    
         BE    OBYT0008            FOUND - PUT IT OUT                           
OBYT0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PUT OUT SORT RECORDS FOR EACH BUDGET RECORD/CONTRACT TYPE                   
*                                                                               
OUTSORT  NTR1                                                                   
         CLI   QCONTYP2,C' '       CONTRACT TYPE FILTER ENTERED?                
         BE    OS0024              NO  - PROCESS ALL ELEMENTS                   
         TM    QCONTYPE,X'40'      UPPER OR LOWER CASE?                         
         BO    OS0016              ON:  UPPER=INCLUDE TYPE (NO BASIC)           
         CLI   0(R6),X'01'         OFF: LOWER=TEST TYPE                         
         BE    OS0024              PROCESS 01 ELEMENT FOR EXCLUDE               
         CLC   QCONTYP2(1),DCONTYP(R6)            SAME TYPE?                    
         BE    OS00EX              YES - EXCLUDE IT                             
         B     OS0024              NO  - INCLUDE IT                             
OS0016   EQU   *                                                                
         CLI   0(R6),X'01'         INCLUDE:  BASIC NOT ALLOWED                  
         BE    OS00EX                                                           
         CLC   QCONTYP2(1),DCONTYP(R6)            SAME TYPE?                    
         BNE   OS00EX              NO  - SKIP IT                                
OS0024   EQU   *                                                                
         BAS   RE,OUTSORT2                                                      
OS00EX   EQU   *                                                                
         XIT1                                                                   
*                                                                               
OUTSORT2 NTR1                                                                   
*                                                                               
OS0026   EQU   *                                                                
         LA    R3,SAVEALL$                                                      
         ZIC   R4,BUCKCTR          NUMBER OF BUCKETS                            
         XC    SVDATE,SVDATE                                                    
*                                                                               
OS0027   EQU   *                                                                
*        OC    0(4,R3),0(R3)       ENTRY IN THE BUCKET?                         
*        BZ    OS0099              NO - GO TO NEXT BUCKET                       
*                                                                               
         BAS   RE,NOTZEREC         PERFORM SORT                                 
OS0099   EQU   *                                                                
         LA    R3,8(R3)            NEXT BUCKET IN THE ARRAY                     
         BCT   R4,OS0027                                                        
*                                                                               
OSEX     EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
NOTZEREC NTR1                                                                   
         XC    SORTREC,SORTREC                                                  
         MVI   SORTREC,C'1'                                                     
         MVC   SORTREC+SGROUP(2),SAVEGRUP                                       
         MVC   SORTREC+SSTAT1(5),SAVESTA                                        
         MVC   SORTREC+SOFF1(2),KEYSAV2+25    OFFICE FROM BGT REC               
*        MVC   P+00(12),=C'KEYSAV2=    '    **TEST**                            
*        MVC   P+12(80),KEYSAV2             **TEST**                            
*        GOTO1 REPORT                       **TEST**                            
         CLC   QUESTOR(4),=C'?UHR' SPECIAL REQUEST                              
         BNE   OS0036              NO                                           
         CLC   SORTREC+SSTAT1(4),=C'KDKA'                                       
         BNE   OS0030                                                           
         MVC   SORTREC+SSTAT1(4),=C'ZZZZ'                                       
OS0030   EQU   *                                                                
         CLC   SORTREC+SOFF1(2),=C'SL'                                          
         BNE   OS0036                                                           
         MVC   SORTREC+SOFF1(2),=C'ZZ'                                          
OS0036   EQU   *                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(2),6(R3)        MOVE MONTH                                   
         MVI   DUB+2,C'/'                                                       
         MVC   DUB+3(2),4(R3)      YEAR                                         
         GOTO1 =V(PERVAL),DMCB,(5,DUB),BLOCK CONVERT IT TO BIN FORMAT           
         MVC   RATEYEAR(2),BLOCK+28   BINARY DATE (Y/M)                         
         BAS   RE,COMRATE     GET COMMISION RATE FOR SPECIFIC DATE              
         MVC   SORTREC+SCOMM(4),SAVERATE      INSERT COMMISSION RATE            
         CLI   QOPTION2,C'T'                  BDGT BY TYPE?                     
         BNE   OS0040                         NO                                
*        CLI   0(R6),X'01'                    BASIC ELEMENT?                    
*        BNE   OS0040                         YES - NO TYPE IN ELEMENT          
         MVC   SORTREC+SCON2(1),DCONTYP(R6)   INSERT CONTRACT TYPE              
OS0040   EQU   *                                                                
         MVC   SORTREC+SALL$(4),0(R3)         BUDGET                            
         MVC   SORTREC+SDATE(4),4(R3)         YEAR/MONTH                        
         MVC   SORTREC+SMKTNM(20),SAVEMKT                                       
         MVC   SORTREC+SOFFNM(20),SAVEOFF                                       
         MVI   SORTREC+STYP2,C'0'             SETS SUBTYP TO X'F0'              
         MVC   SORTREC+SLEFT(1),STAACTIV      ACTIVE/INACTIVE FLAG              
         MVC   SORTREC+SDEFCOM(1),DEFCOMM     DEF COMM USED/NOT USED            
OS0042   EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*        MVC   P+1(06),=C'OUT:  '             ****TEST                          
*        MVC   P+10(80),SORTREC               ****TEST                          
*        EDIT  (4,SAVERATE),(10,P+60),2,COMMAS=YES                              
*        GOTO1 REPORT                         ****TEST                          
         MVI   SORTREC,C'2'                                                     
         MVC   SORTREC+SGROUP(2),SPACES       NO GRP FOR OFF ORDER              
         MVC   SORTREC+SSTAT2(5),SAVESTA      SWAP KEYS FOR TYPE 2              
         MVC   SORTREC+SOFF2(2),KEYSAV2+25                                      
         CLC   QUESTOR(4),=C'?UHR' SPECIAL REQUEST                              
         BNE   OS0056              NO                                           
         CLC   SORTREC+SSTAT2(4),=C'KDKA'                                       
         BNE   OS0046                                                           
         MVC   SORTREC+SSTAT2(4),=C'ZZZZ'                                       
OS0046   EQU   *                                                                
         CLC   SORTREC+SOFF2(2),=C'SL'                                          
         BNE   OS0056                                                           
         MVC   SORTREC+SOFF2(2),=C'ZZ'                                          
OS0056   EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'3'        SET UP REGIONAL OUTPUT                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    BUDCTR,=P'3'                   ADD BGT RECS GEN'D                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  FIND CORRESPONDING COMMISSION RATE FOR SPECIFIC MONTH                        
*                                                                               
COMRATE  NTR1                                                                   
         CLC   SVDATE,RATEYEAR     SAME RATE?                                   
         BH    CMRATEX             YES - RECORD HAS ALREADY BEEN READ           
         MVI   DEFCOMM,C'Y'        SET 'DEFAULT COMMISSION USED' FLAG           
         MVC   SAVERATE,=F'127500' SET DEFAULT COMMISSION RATE                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'29'           GET COMMISSION RATE KEY                      
         MVC   KEY+11(2),RCREPFL                                                
         MVC   KEY+13(5),SAVESTA   INSERT STATION                               
         MVC   KEYSAV3(20),KEY     SAVE THIS KEY                                
         GOTO1 HIGH                                                             
CMRATE28 EQU   *                                                                
         CLC   KEYSAV3(18),KEY        RECORD FOUND?                             
         BNE   CMRATE36               NO  - USE CONTENTS OF SAVERATE            
         OC    KEY+18(7),KEY+18       OFFICE, ADV, OR TYPE IN KEY?              
         BNZ   CMRATE32               YES - SKIP IT                             
         CLC   KEY+25(2),RATEYEAR     BUDGET YEAR VS KEY YEAR                   
         BH    CMRATE36               KEYDATE > BUDYEAR: FINISHED               
         BAS   RE,GETCOM              GET COMMISSION RATE RECORD                
         MVC   SAVERATE(4),RCOMELEM+2 SAVE RATE                                 
         MVI   DEFCOMM,C'N'           SET 'DEFAULT COMM NOT USED' FLAG          
*                                                                               
*   RECORD LAYOUT FULL-WORD ALIGNS RCOMRAT1.  RCOMELEM+2 IS THE                 
*        CORRECT FIELD FOR THE RATE                                             
*                                                                               
CMRATE32 EQU   *                                                                
         MVC   SVDATE,KEY+25       SAVE DATE                                    
         GOTO1 SEQ                                                              
         B     CMRATE28               RETURN TO TEST IT                         
CMRATE36 EQU   *                                                                
         CLI   DEFCOMM,C'Y'                                                     
         BNE   CMRATE40                                                         
         BAS   RE,DEFUSED             YES - ADD TO EXCEPT LIST                  
         B     CMRATEX                                                          
CMRATE40 EQU   *                   NOT DEFAULT RATE IS USED                     
         MVC   SVDATE,KEY+25          UPDATE SVDATE                             
CMRATEX  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DEFUSED  NTR1                                                                   
         CLC   QGROUP(1),SPACES        ANY GROUP FILTER?                        
         BE    SFUS0010                NO  - SKIP TEST                          
         CLC   QGROUP(1),RSTAGRUP      YES - BUDGET FOR GRP?                    
         BNE   SFUS0099                NO  - DON'T ADD TO EXCEPT LIST           
SFUS0010 EQU   *                                                                
         CLC   QSBGROUP(1),SPACES      ANY SUBGROUP FILTER?                     
         BE    SFUS0020                NO  - SKIP TEST                          
         CLC   QSBGROUP(1),RSTAGRUP+1  YES - BUDGET FOR SUBGRP?                 
         BNE   SFUS0099                NO  - DON'T ADD TO EXCEPT LIST           
SFUS0020 EQU   *                                                                
         L     R5,FNOCOMMS         A(FIRST SPACE USED)                          
         L     R6,ANOCOMMS         A(LAST SPACE USED)                           
SFUS0025 EQU   *                                                                
         CR    R5,R6                                                            
         BH    SFUS0030                                                         
         CLC   0(5,R5),SAVESTA     DUPLICATE ENTRY?                             
         BE    SFUS0099            YES DON'T ADD IT TO THE LIST                 
SFUS0027 EQU   *                                                                
         LA    R5,L'NOCOMMS(R5)                                                 
         B     SFUS0025                                                         
SFUS0030 EQU   *                                                                
*        LA    R5,L'NOCOMMS(R5)    BUMP TO A(NEXT AVAILABLE SPACE)              
         CLC   0(5,R5),=C'ZZZZZ'   DELIMITER REACHED?                           
         BE    SFUS0099            YES  - NO MORE ROOM (TOUGH)                  
         MVC   0(5,R5),SAVESTA     SAVE STATION                                 
         ST    R5,ANOCOMMS         SAVE ADDRESS                                 
SFUS0099 EQU   *                                                                
         XIT1                                                                   
*                                                                               
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETBUD   LA    RF,RBUDREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETCOM   LA    RF,RCOMREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
*                                                                               
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
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
         SPACE 3                                                                
CONFLAG  DS    CL1                                                              
PRTFLAG  DC    CL1'X'                                                           
*                                                                               
*   PRTFLAG SETTING:  X  = NO: FIRST PASS                                       
*                     N  = NO: ALL OTHER PASSES                                 
*                     Y  = YES: PRINT TOTALS                                    
*                                                                               
ACCFLAG  DC    CL1'Y'              ACCUMULATE/DON'T ACCUMULATE                  
STARTMO  DS    XL1                 REQUEST START MONTH                          
ENDMO    DS    XL1                 REQUEST END   MONTH                          
NUMMOS   DS    XL1                 NUMBER OF MONTHS IN REQUEST                  
         DS    0F                                                               
INVTBL   DS    0CL96               EACH ENTRY CONSISTS OF:                      
         DC    12D'0'              4 BYTES INV $ + 4 BYTES YEAR/MONTH           
CONTOTS  DS    0CL16                                                            
         DC    4F'0'                                                            
TMONTBL  DS    0CL8                                                             
         DC    2F'00'              BUCKET ACCUMULATOR                           
BUCKCTR  DS    X                   BUCKET COUNTER                               
*  12 MONTHS AGGREGATED AS SINGLE BUCKET                                        
*  2 FULLWORDS, ONE WORD PER BUCKET                                             
*        1ST FULLWORD   = CURRENT YEAR BEST $ FIGURE                            
*        2ND FULLWORD   = PRIOR YEAR INVOICED                                   
         SPACE 1                                                                
SORTREC  DS    0CL76                                                            
STYP     DS    CL1                                                              
         DS    CL13                                                             
SCONTYP  DS    CL1                 CONTRACT TYPE                                
SSUBTYP  DS    CL1                                                              
SPRIDOL  DS    CL4                 PRIOR YEAR ACTUALS                           
SCURDOL  DS    CL4                 CURRENT YEAR BEST DOLLAR                     
SALLDOL  DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAME DS    CL20                MARKET NAME                                  
SOFFNAME DS    CL20                OFFICE NAME                                  
SCOMMRTE DS    CL4                 COMMISSION RATE                              
SLEFTFLG DS    CL1                 STATION LEFT FLAG                            
SDEFCOMM DS    CL1                 DEFAULT COMM USED FLAG                       
         DS    CL2                 SPARE                                        
*                                                                               
*  KEY BUILDING EQUATES:  DISPLACEMENTS INTO SORTREC                            
*                                                                               
SGROUP   EQU   1                                                                
SSTAT1   EQU   3                   STATION MAJOR                                
SOFF1    EQU   8                   OFFICE MINOR                                 
SOFF2    EQU   3                   OFFICE MAJOR                                 
SSTAT2   EQU   5                   STATION MINOR                                
SDATE    EQU   10                  YEAR                                         
SMONTH   EQU   12                  MONTH                                        
SCON2    EQU   14                  CONTRACT TYPE                                
STYP2    EQU   15                  SUBTYPE FOR BUDGET RECORDS                   
SPRIOR   EQU   16                  PRIOR INVOICE DOLLARS DISPLACEMENTNT         
SCURR    EQU   20                  CURRENT INVOICE DOLLARS DISPLACEMENT         
SALL$    EQU   24                  ALLOCATED DOLLARS DISPLACEMENT               
SMKTNM   EQU   28                                                               
SOFFNM   EQU   48                                                               
SCOMM    EQU   68                  COMMISSION RATE DISPLACEMENT                 
SLEFT    EQU   72                  STATION LEFT FLAG DISPLACEMENT               
SDEFCOM  EQU   73                  DEF COMM USED FLAG DISPLACEMENT              
*                                                                               
SORTREC2 DS    0CL76               SAVED SORTKEY                                
SRECTYP2 DS    CL1                                                              
         DS    CL13                                                             
SCONTYP2 DS    CL1                                                              
SSUBTYP2 DS    CL1                                                              
SPRIDOL2 DS    CL4                 PRIOR YEAR ACTUALS                           
SCURDOL2 DS    CL4                 CURRENT YEAR BEST DOLLAR                     
SALLDOL2 DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAM2 DS    CL20                                                             
SOFFNAM2 DS    CL20                                                             
SCOMMRT2 DS    CL4                 COMMISSION RATE                              
SLEFTFL2 DS    CL1                 STATION LEFT FLAG                            
SDEFCOM2 DS    CL1                 DEFAULT COMM USED FLAG                       
         DS    CL2                 SPARE                                        
*                                                                               
RESETREC DS    CL72                                                             
ZZKEY    DS    CL10                                                             
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
SAVESTA  DC    CL5'     '          SAVED STATION CALLS                          
SAVEGRUP DC    CL2'  '             SAVED GROUP CODE                             
SAVETVB  DC    CL2'  '             SAVED TVB REGION                             
SAVEOWNR DC    CL3'   '            SAVED OWNERSHIP                              
INDEX1   DS    F                   BUDGET REVENUE $ FOR INDEX                   
INDEX2   DS    F                   BILLNG REVENUE $ FOR INDEX                   
SAVEALL$ DS    0CL96                                                            
         DC    12D'0'              SAVED ALLOCATION DOLLARS                     
BUDYEAR  DS    CL2                 BUDGET YEAR                                  
FLAG     DS    X                                                                
YRADDED  EQU   X'80'               YEAR HAS BEEN INCREMENTED ALREADY            
NZREC    EQU   X'40'               NOT ZERO REC                                 
SAVEMKT  DS    CL20                SAVE MARKET NAME                             
SAVEOFF  DS    CL20                SAVE OFFICE NAME                             
STAACTIV DS    CL1                 STATION ACTIVE/INACTIVE FLAG                 
SAVERATE DC    XL4'0'              SAVE COMMISSION RATE                         
DEFCOMM  DS    CL1                 DEFAULT COMMISSION FLAG                      
KEYSAV3  DS    CL27                ALTERNATE KEY SAVE AREA                      
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
OFFFLAG  DC    C'N'                                                             
CO$FLAG  DC    C'N'                                                             
INCFLAG  DC    C'N'                INCLUDE ALLOCATION FLAG                      
QCONTYP2 DS    CL1                 ALTERNATE CONTRACT TYPE FLAG                 
TESTCNTR DS    XL1'00'             **TEST**                                     
ALEFSTAT DS    F                   A(NEXT AVAILABLE ENTRY)                      
*                                                                               
GENWORK  EQU   *                                                                
RATEDATE DS   0CL24                                                             
RATEYEAR DS    XL1                 FINAL STORAGE FOR TODAY'S DATE               
RATEMON  DS    XL1                 BROADCAST MONTH                              
RATEDAY  DS    XL1                                                              
RDWORK   DS    CL21                                                             
SVDATE   DC    XL2'0'              SAVED RATE DATE (BINARY)                     
TEMPCNT  DC    F'0'                                                             
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
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=76'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
MTABLE   DS    0CL61                                                            
         DC    CL2'01'                                                          
         DC    CL3'JAN'                                                         
         DC    CL2'02'                                                          
         DC    CL3'FEB'                                                         
         DC    CL2'03'                                                          
         DC    CL3'MAR'                                                         
         DC    CL2'04'                                                          
         DC    CL3'APR'                                                         
         DC    CL2'05'                                                          
         DC    CL3'MAY'                                                         
         DC    CL2'06'                                                          
         DC    CL3'JUN'                                                         
         DC    CL2'07'                                                          
         DC    CL3'JUL'                                                         
         DC    CL2'08'                                                          
         DC    CL3'AUG'                                                         
         DC    CL2'09'                                                          
         DC    CL3'SEP'                                                         
         DC    CL2'10'                                                          
         DC    CL3'OCT'                                                         
         DC    CL2'11'                                                          
         DC    CL3'NOV'                                                         
         DC    CL2'12'                                                          
         DC    CL3'DEC'                                                         
         DC    X'FF'                                                            
ANOCOMMS DS    F                   A(NEXT AVAILABLE ENTRY)                      
FNOCOMMS DS    F                   A(FIRST ENTRY)                               
* LOWER LEVEL TOTALS (OFFICE/STATION)                                           
TOTLPRI$ DC    XL4'0'      OFFICE  TOTAL PRIOR DOLLARS                          
TOTLCUR$ DC    XL4'0'      OFFICE  TOTAL CURRENT DOLLARS                        
TOTLALL$ DC    XL4'0'      OFFICE  TOTAL ALLOCATION DOLLARS                     
TOTLBUD$ DC    XL4'0'      OFFICE  TOTAL BUDGET REVENUE DOLLARS                 
TOTLBIL$ DC    XL4'0'      OFFICE  TOTAL BILLNG REVENUE DOLLARS                 
* GRAND TOTALS FOR 12 MONTHS                                                    
TSTLPRI$ DC    XL4'0'              GRAND TOTAL PRIOR DOLLARS (MONTH)            
TSTLCUR$ DC    12F'0'              GRAND TOTAL CURRENT DOLLARS                  
*                                  (12 MONTHS)                                  
TSTLALL$ DC    12F'0'              GRAND TOTAL ALLOCATION DOLLARS               
*                                  (12 MONTHS)                                  
TSTLBUD$ DC    12F'0'              GRAND TOTAL BUDGET REVENUE DOLLARS           
*                                  (12 MONTHS)                                  
TSTLBIL$ DC    12F'0'              GRAND TOTAL BILLNG REVENUE DOLLARS           
*                                  (12 MONTHS)                                  
* GRAND TOTALS FOR 1 YEAR                                                       
TSTLTPR$ DC    XL4'0'        GRAND TOTAL PRIOR DOLLARS                          
TSTLTCR$ DC    XL4'0'        GRAND TOTAL CURRENT DOLLARS                        
TSTLTAL$ DC    XL4'0'        GRAND TOTAL ALLOCATION DOLLARS                     
TSTLTBD$ DC    XL4'0'        GRAND TOTAL BUDGET REVENUE DOLLARS                 
TSTLTBL$ DC    XL4'0'        GRAND TOTAL BILLNG REVENUE DOLLARS                 
* LOWER LEVEL TOTALS (OFFICE/STATION)                                           
TOTZZPRI DC    XL4'0'       OFFICE TOTAL OFFICE ZZ PRIOR DOLLARS                
TOTZZCUR DC    XL4'0'       OFFICE TOTAL OFFICE ZZ CURRENT DOLLARS              
TOTZZALL DC    XL4'0'       OFFICE TOTAL OFFICE ZZ ALLOCATION DOLLARS           
TOTZZBUD DC    XL4'0'       OFFICE TOTAL OFFICE ZZ BUDGET REV DOLLARS           
TOTZZBIL DC    XL4'0'       OFFICE TOTAL OFFICE ZZ BLG    REV DOLLARS           
* GRAND TOTALS FOR 12 MONTHS                                                    
TSTZZPRI DC    XL4'0'       OFFICE TOTAL OFFICE ZZ PRIOR DOLLARS                
TSTZZCUR DC    12F'0'      GRAND TOTAL OFFICE ZZ CURRENT DOLLARS                
*                                  (12 MONTHS)                                  
TSTZZALL DC    12F'0'      GRAND TOTAL OFFICE ZZ ALLOCATION DOLLARS             
*                                  (12 MONTHS)                                  
TSTZZBUD DC    12F'0'      GRAND TOTAL OFFICE ZZ BUDGET REV DOLLARS             
*                                  (12 MONTHS)                                  
TSTZZBIL DC    12F'0'      GRAND TOTAL OFFICE ZZ BLG    REV DOLLARS             
*                                  (12 MONTHS)                                  
* GRAND TOTALS FOR 1 YEAR                                                       
TSTZZTPR DC    XL4'0'        GRAND TOTAL OFFICE ZZ PRIOR DOLLARS                
TSTZZTCR DC    XL4'0'        GRAND TOTAL OFFICE ZZ CURRENT DOLLARS              
TSTZZTAL DC    XL4'0'        GRAND TOTAL OFFICE ZZ ALLOCATION DOLLARS           
TSTZZTBD DC    XL4'0'        GRAND TOTAL OFFICE ZZ BUDGET REV DOLLARS           
TSTZZTBL DC    XL4'0'        GRAND TOTAL OFFICE ZZ BLG    REV DOLLARS           
* LOWER LEVEL TOTALS (OFFICE/STATION)                                           
TNOZZPRI DC    XL4'0'       OFFICE TOTAL NOT OFFICE ZZ PRIOR DOLLARS            
TNOZZCUR DC    XL4'0'       OFFICE TOTAL NOT OFFICE ZZ CURRENT DOLLARS          
TNOZZALL DC    XL4'0'       OFFICE TOTAL NOT OFFICE ZZ ALLOC DOLLARS            
TNOZZBUD DC    XL4'0'       OFFICE TOTAL NOT OFF ZZ BUDGET REV DOLLARS          
TNOZZBIL DC    XL4'0'       OFFICE TOTAL NOT OFF ZZ BLG    REV DOLLARS          
* GRAND TOTALS FOR 12 MONTHS                                                    
TNSZZPRI DC    XL4'0'        GRAND TOTAL NOT OFFICE ZZ PRIOR DOLLARS            
TNSZZCUR DC    12F'0'        GRAND TOTAL NOT OFFICE ZZ CURRENT DOLLARS          
*                                  (12 MONTHS)                                  
TNSZZALL DC    12F'0'        GRAND TOTAL NOT OFFICE ZZ ALLOC DOLLARS            
*                                  (12 MONTHS)                                  
TNSZZBUD DC    12F'0'        GRAND TOTAL NOT OFF ZZ BUDGET REV DOLLARS          
*                                  (12 MONTHS)                                  
TNSZZBIL DC    12F'0'        GRAND TOTAL NOT OFF ZZ BLG    REV DOLLARS          
*                                  (12 MONTHS)                                  
* GRAND TOTALS FOR 1 YEAR                                                       
TNSZZTPR DC    XL4'0'        GRAND TOTAL NOT OFFICE ZZ PRIOR DOLLARS            
TNSZZTCR DC    XL4'0'        GRAND TOTAL NOT OFFICE ZZ CURRENT DOLLARS          
*                            (1 YEAR)                                           
TNSZZTAL DC    XL4'0'        GRAND TOTAL NOT OFFICE ZZ ALLOC DOLLARS            
*                            (1 YEAR)                                           
TNSZZTBD DC    XL4'0'        GRAND TOTAL NOT OFF ZZ BUDGET REV DOLLARS          
*                            (1 YEAR)                                           
TNSZZTBL DC    XL4'0'        GRAND TOTAL NOT OFF ZZ BLG    REV DOLLARS          
*                            (1 YEAR)                                           
* COMPANY TOTALS FOR 1 YEAR                                                     
COMPPRI$ DC    XL4'0'              COMPANY PRIOR DOLLARS                        
*                            (1 YEAR)                                           
COMPCUR$ DC    XL4'0'              COMPANY CURRENT DOLLARS                      
*                            (1 YEAR)                                           
COMPALL$ DC    XL4'0'              COMPANY ALLOCATION DOLLARS                   
*                            (1 YEAR)                                           
COMPBUD$ DC    XL4'0'              COMPANY BUDGET REV DOLLARS                   
*                            (1 YEAR)                                           
COMPBIL$ DC    XL4'0'              COMPANY BLG    REV DOLLARS                   
*                            (1 YEAR)                                           
* COMPANY TOTALS FOR 12 MONTHS                                                  
COMPTPR$ DC    XL4'0'              COMPANY PRIOR DOLLARS                        
*                            (12 MONTH)                                         
COMPTCR$ DC    12F'0'              COMPANY CURRENT DOLLARS                      
*                            (12 MONTH)                                         
COMPTAL$ DC    12F'0'              COMPANY ALLOCATION DOLLARS                   
*                            (12 MONTH)                                         
COMPTBD$ DC    12F'0'              COMPANY BUDGET REV DOLLARS                   
*                            (12 MONTH)                                         
COMPTBL$ DC    12F'0'              COMPANY BLG    REV DOLLARS                   
*                            (12 MONTH)                                         
* COMPANY TOTALS FOR 1 YEAR                                                     
ZZPRI$   DC    XL4'0'              OFFICE ZZ PRIOR DOLLARS                      
*                            (1 YEAR)                                           
ZZCUR$   DC    XL4'0'              OFFICE ZZ CURRENT DOLLARS                    
*                            (1 YEAR)                                           
ZZALL$   DC    XL4'0'              OFFICE ZZ ALLOCATION DOLLARS                 
*                            (1 YEAR)                                           
ZZBUD$   DC    XL4'0'              OFFICE ZZ BUDGET REV DOLLARS                 
*                            (1 YEAR)                                           
ZZBIL$   DC    XL4'0'              OFFICE ZZ BLG    REV DOLLARS                 
*                            (1 YEAR)                                           
* COMPANY TOTALS FOR 12 MONTHS                                                  
ZZTPR$   DC    XL4'0'              OFFICE ZZ PRIOR DOLLARS                      
*                            (12 MONTH)                                         
ZZTCR$   DC    12F'0'              OFFICE ZZ CURRENT DOLLARS                    
*                            (12 MONTH)                                         
ZZTAL$   DC    12F'0'              OFFICE ZZ ALLOCATION DOLLARS                 
*                            (12 MONTH)                                         
ZZTBD$   DC    12F'0'              OFFICE ZZ BUDGET REV DOLLARS                 
*                            (12 MONTH)                                         
ZZTBL$   DC    12F'0'              OFFICE ZZ BLG    REV DOLLARS                 
*                            (12 MONTH)                                         
* COMPANY TOTALS FOR 1 YEAR                                                     
NOZZPRI$ DC    XL4'0'              NOT OFFICE ZZ PRIOR DOLLARS                  
*                            (1 YEAR)                                           
NOZZCUR$ DC    XL4'0'              NOT OFFICE ZZ CURRENT DOLLARS                
*                            (1 YEAR)                                           
NOZZALL$ DC    XL4'0'              NOT OFFICE ZZ ALLOCATION DOLLARS             
*                            (1 YEAR)                                           
NOZZBUD$ DC    XL4'0'              NOT OFFICE ZZ BUDGET REV DOLLARS             
*                            (1 YEAR)                                           
NOZZBIL$ DC    XL4'0'              NOT OFFICE ZZ BLG    REV DOLLARS             
*                            (1 YEAR)                                           
* COMPANY TOTALS FOR 12 MONTHS                                                  
NOZZTPR$ DC    XL4'0'              NOT OFFICE ZZ PRIOR DOLLARS                  
*                            (12 MONTH)                                         
NOZZTCR$ DC    12F'0'              NOT OFFICE ZZ CURRENT DOLLARS                
*                            (12 MONTH)                                         
NOZZTAL$ DC    12F'0'              NOT OFFICE ZZ ALLOCATION DOLLARS             
*                            (12 MONTH)                                         
NOZZTBD$ DC    12F'0'              NOT OFFICE ZZ BUDGET REV DOLLARS             
*                            (12 MONTH)                                         
NOZZTBL$ DC    12F'0'              NOT OFFICE ZZ BLG    REV DOLLARS             
*                            (12 MONTH)                                         
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
BLOCK    DS    CL56                OUTPUT AREA FOR PERVAL                       
*                                                                               
SNOCOMMS DC    A(NOCOMMS)                                                       
LEFSTATS DS    200CL8              200 8-BYTE ENTRIES                           
ENLEFSTS DC    CL5'ZZZZZ'          DELIMITER                                    
LLEFSTAT EQU   *-LEFSTATS                                                       
*                                  BYTES 1-5  =  STATION CALLS                  
*                                  BYTES 6-7  =  OFFICE                         
*                                  BYTE 8  =  FLAG (UNUSED)                     
NOCOMMS  DS    400CL8              400 8-BYTE ENTRIES                           
ENNOCOMM DC    CL5'ZZZZZ'          DELIMITER                                    
LNOCOMMS EQU   *-NOCOMMS                                                        
*                                  BYTES 1-5  =  STATION CALLS                  
*                                  BYTES 6-7  =  OFFICE                         
*                                  BYTE 8  =  FLAG (UNUSED)                     
         EJECT                                                                  
         PRINT GEN                                                              
OFFTOT   NMOD1 0,*OFTT*                                                         
         PRINT NOGEN                                                            
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         CLI   PRTFLAG,C'Y'        ANYTHING TO PRINT?                           
         BNE   OFTL0044            NO- GET OUT                                  
         MVI   PRTFLAG,C'N'        TURN OFF FLAG                                
         OC    TOTZZPRI(20),TOTZZPRI ANY ZZ/ZZZZ TOTALS?                        
         BZ    OFTL0032            NO  -                                        
         B     OFTL0016            SKIP OFFICE ZZ TOTAL LINE                    
*                                                                               
         MVC   P+1(20),=C'OFFICE ZZ TOTAL---->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    OFTL0004            YES                                          
         MVC   P+1(20),=C'STATION ZZZZ TOTAL->'                                 
OFTL0004 EQU   *                                                                
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    OFTL0012            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    OFTL0008            YES                                          
*                                  BUDGET REPORT                                
         LA    R6,TOTZZALL         OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZBUD         OFF=ZZ/STA=ZZZZ BUDGET REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     OFTL0016                                                         
OFTL0008 EQU   *                   BILLING REPORT                               
         LA    R6,TOTZZCUR         OFF=ZZ/STA=ZZZZ BILLG DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZBIL         OFF=ZZ/STA=ZZZZ BILLNG REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     OFTL0016                                                         
OFTL0012 EQU   *                                                                
         LA    R6,TOTZZALL         OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZCUR         OFF=ZZ/STA=ZZZZ CURRENT DOLLAR TOTAL         
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZBUD         OFF=ZZ/STA=ZZZZ BUDGET REVENUE $             
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TOTZZBIL         OFF=ZZ/STA=ZZZZ BILLING REVENUE $            
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
OFTL0016 EQU   *                                                                
         MVC   P+1(20),=C'ALL OTHER OFFICES-->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    OFTL0020            YES                                          
         MVC   P+1(20),=C'ALL OTHER STATIONS->'                                 
OFTL0020 EQU   *                                                                
*                                                                               
*   NON-OFFICE-ZZ/NON-STATION-ZZZZ FIGURES                                      
*                                                                               
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    OFTL0028            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    OFTL0024            YES                                          
         LA    R6,TNOZZALL         NON OFF ZZ      ALLOC DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZBUD         NON OFF ZZ      BUDGET REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     OFTL0032                                                         
OFTL0024 EQU   *                                                                
         LA    R6,TNOZZCUR         NON OFF ZZ      BILLG DOLLAR TOTAL           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZBIL         NON OFF ZZ      BILLNG REVENUE $             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     OFTL0032                                                         
OFTL0028 EQU   *                                                                
         LA    R6,TNOZZALL         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZCUR         NON OFFICE ZZ CURRENT DOLLAR TOTAL           
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZBUD         NON OFFICE ZZ BUDGET REVENUE $               
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TNOZZBIL         NON OFFICE ZZ BILLING REVENUE $              
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
OFTL0032 EQU   *                                                                
         MVC   P+1(20),=C'OFFICE TOTAL ------>'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    OFTL0034            YES                                          
         MVC   P+1(20),=C'STATION TOTAL------>'                                 
OFTL0034 CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    OFTL0040            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    OFTL0036            YES                                          
*                                  BUDGET REPORT                                
         LA    R6,TOTLALL$         ALLOCATION DOLLAR TOTAL                      
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLBUD$         BUDGET REVENUE $                             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    OFTL0044            NO                                           
*        MVI   FORCEFUT,C'Y'       FORCE FOOTING OUT                            
*        GOTO1 REPORT                                                           
         B     OFTL0044                                                         
OFTL0036 EQU   *                   BILLING REPORT                               
         LA    R6,TOTLCUR$         BILLING DOLLAR TOTAL                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLBIL$         BILLING REVENUE $                            
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    OFTL0044            NO                                           
*        MVI   FORCEFUT,C'Y'       FORCE FOOTING OUT                            
*        GOTO1 REPORT                                                           
         B     OFTL0044                                                         
OFTL0040 EQU   *                                                                
         LA    R6,TOTLCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLALL$         ALLOCATION DOLLAR TOTAL                      
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLBUD$         BUDGET REVENUE $                             
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TOTLBIL$         BILLING REVENUE $                            
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC         CALCULATE INDEXED FOR GRAND TOTS             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    OFTL0044            NO                                           
*        MVI   FORCEFUT,C'Y'       FORCE FOOTING OUT                            
*        GOTO1 REPORT                                                           
OFTL0044 EQU   *                                                                
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
         XC    TOTLPRI$(20),TOTLPRI$   ZERO OUT ACCUMULATOR                     
         XC    TOTZZPRI(20),TOTZZPRI   ZERO OUT ACCUMULATOR                     
         XC    TNOZZPRI(20),TNOZZPRI   ZERO OUT ACCUMULATOR                     
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
*                                                                               
* PRODUCE GRAND TOTALS                                                          
*                                                                               
GRDTOT   NMOD1 0,*GRTT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         CLI   PRTFLAG,C'Y'        ANYTHING TO PRINT?                           
         BNE   GRTL0064            NO- GET OUT                                  
         CLI   STYP,C'1'           TYPE 1?                                      
         BNE   GRTL0002            TYPE 2 OR TYPE 3                             
*                                                                               
         MVC   SORTREC+SOFF1(2),SPACES                                          
         MVC   SORTREC+SOFFNM(20),=C'GRAND TOTAL         '                      
         B     GRTL0003                                                         
*                                                                               
GRTL0002 EQU   *                                                                
         MVC   SORTREC+SMKTNM(20),=C'GRAND TOTAL         '                      
         MVC   SORTREC+SSTAT2(5),SPACES                                         
*                                                                               
GRTL0003 EQU   *                                                                
         OC    TSTZZPRI(196),TSTZZPRI ANY ZZ/ZZZZ TOTALS?                       
         BZ    GRTL0046            NO  -                                        
         B     GRTL0024            SKIP OFFICE ZZ TOTAL LINE                    
*                                                                               
GRTL0004 EQU   *                                                                
         LA    R5,1                COUNTER                                      
         BCTR  R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    GRTL0012            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    GRTL0008            YES                                          
*                                  BUDGET REPORT                                
GRTL0006 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TSTZZALL                                                      
         LA    R6,0(R4,R6)  OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL                  
         OC    0(4,R6),0(R6)       SKIP?                                        
         BZ    GRTL0007            YES                                          
*                                                                               
         L     R0,TSTZZTAL         (YEAR) TOTAL                                 
         A     R0,0(R6)            GRAND TOTAL ALLOC DOLLAR (YEAR)              
         ST    R0,TSTZZTAL                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZBUD                                                      
         LA    R6,0(R4,R6)    OFF=ZZ/STA=ZZZZ BUDGET REVENUE $                  
*                                                                               
         L     R0,TSTZZTBD                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVEN (YEAR)              
         ST    R0,TSTZZTBD                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0007 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0006                                                         
*                                                                               
         LA    R6,TSTZZTAL         GRAND TOTAL FOR YEAR                         
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZTBD                                                      
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0022                                                         
GRTL0008 EQU   *                   BILLING REPORT                               
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TSTZZCUR        OFF=ZZ/STA=ZZZZ BILLG DOLLAR TOTAL            
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)       SKIP?                                        
         BZ    GRTL0009            YES                                          
*                                                                               
         L     R0,TSTZZTCR                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING DOLLAR(YEAR)             
         ST    R0,TSTZZTCR                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZBIL   OFF=ZZ/STA=ZZZZ BILLNG REVENUE $                   
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTZZTBL                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING REVEN(YEAR)              
         ST    R0,TSTZZTBL                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0009 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0008                                                         
*                                                                               
         LA    R6,TSTZZTCR        OFF=ZZ/STA=ZZZZ BILLG DOLLAR TOTAL            
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZTBL   OFF=ZZ/STA=ZZZZ BILLNG REVENUE $                   
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0022                                                         
GRTL0012 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
*                                                                               
         LA    R6,TSTZZALL    OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL                
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0016                                                         
*                                                                               
         LA    R6,TSTZZCUR   OFF=ZZ/STA=ZZZZ CURRENT DOLLAR TOTAL               
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0018                                                         
         B     GRTL0020                                                         
*                                                                               
GRTL0016 EQU   *                                                                
         L     R0,TSTZZTAL                                                      
         A     R0,0(R6)            GRAND TOTAL ALLOC DOLLAR(YEAR)               
         ST    R0,TSTZZTAL                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZCUR   OFF=ZZ/STA=ZZZZ CURRENT DOLLAR TOTAL               
         LA    R6,0(R4,R6)                                                      
*                                                                               
GRTL0018 EQU   *                                                                
         L     R0,TSTZZTCR                                                      
         A     R0,0(R6)            GRAND TOTAL CURRENT DOLLAR(YEAR)             
         ST    R0,TSTZZTCR                                                      
*                                                                               
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZBUD   OFF=ZZ/STA=ZZZZ BUDGET REVENUE $                   
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTZZTBD                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVENUE(YEAR)             
         ST    R0,TSTZZTBD                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TSTZZBIL   OFF=ZZ/STA=ZZZZ BILLING REVENUE $                  
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTZZTBL                                                      
         A     R0,0(R6)            GRAND TOTAL BILLIN REVENUE(YEAR)             
         ST    R0,TSTZZTBL                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0020 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0006                                                         
*                                                                               
         LA    R6,TSTZZTAL    OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL                
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZTCR   OFF=ZZ/STA=ZZZZ CURRENT DOLLAR TOTAL               
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTZZTBD   OFF=ZZ/STA=ZZZZ BUDGET REVENUE $                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TSTZZTBL   OFF=ZZ/STA=ZZZZ BILLING REVENUE $                  
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
*                                                                               
GRTL0022 EQU   *                                                                
         MVC   P+1(20),=C'OFFICE ZZ TOTAL---->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    *+10                YES                                          
         MVC   P+1(20),=C'STATION ZZZZ TOTAL->'                                 
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*   NON-OFFICE-ZZ/NON-STATION-ZZZZ FIGURES                                      
*                                                                               
GRTL0024 EQU   *                                                                
         LA    R5,1                COUNTER                                      
         BCTR  R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    GRTL0036            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    GRTL0030            YES                                          
*                                                                               
GRTL0026 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TNSZZALL   NON OFF ZZ      ALLOC DOLLAR TOTAL                 
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    GRTL0028                                                         
*                                                                               
         L     R0,TNSZZTAL                                                      
         A     R0,0(R6)            GRAND TOTAL ALLOC DOLLAR(YEAR)               
         ST    R0,TNSZZTAL                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZBUD   NON OFF ZZ      BUDGET REVENUE $                   
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TNSZZTBD                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVENUE(YEAR)             
         ST    R0,TNSZZTBD                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0028 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0026                                                         
*                                                                               
         LA    R6,TNSZZTAL   NON OFF ZZ      ALLOC DOLLAR TOTAL (YEAR)          
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZTBD   NON OFF ZZ      BUDGET REVENUE $ (YEAR)            
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0044                                                         
*                                                                               
GRTL0030 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TNSZZCUR   NON OFF ZZ      BILLG DOLLAR TOTAL                 
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    GRTL0032                                                         
                                                                                
         L     R0,TNSZZTCR                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING DOLLAR(YEAR)             
         ST    R0,TNSZZTCR                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZBIL   NON OFF ZZ      BILLNG REVENUE $                   
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TNSZZTBL                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING REVENUE(YEAR)            
         ST    R0,TNSZZTBL                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0032 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0030                                                         
*                                                                               
         LA    R6,TNSZZTCR   NON OFF ZZ      BILLG DOLLAR TOTAL(YEAR)           
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZTBL   NON OFF ZZ      BILLNG REVENUE $(YEAR)             
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0044                                                         
GRTL0036 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TNSZZALL      NON OFFICE ZZ ALLOC DOLLAR TOTAL                
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0038                                                         
         LA    R6,TNSZZCUR   NON OFFICE ZZ CURRENT DOLLAR TOTAL                 
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0040                                                         
         B     GRTL0042                                                         
*                                                                               
GRTL0038 EQU   *                                                                
         L     R0,TNSZZTAL                                                      
         A     R0,0(R6)            GRAND TOTAL ALLOC DOLLAR(YEAR)               
         ST    R0,TNSZZTAL                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZCUR   NON OFFICE ZZ CURRENT DOLLAR TOTAL                 
         LA    R6,0(R4,R6)                                                      
*                                                                               
GRTL0040 EQU   *                                                                
         L     R0,TNSZZTCR                                                      
         A     R0,0(R6)            GRAND TOTAL CURR DOLLAR(YEAR)                
         ST    R0,TNSZZTCR                                                      
*                                                                               
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZBUD    NON OFFICE ZZ BUDGET REVENUE $                    
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TNSZZTBD                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVENUE(YEAR)             
         ST    R0,TNSZZTBD                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TNSZZBIL    NON OFFICE ZZ BILLING REVENUE $                   
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TNSZZTBL                                                      
         A     R0,0(R6)            GRAND TOTAL BILLIN REVENUE(YEAR)             
         ST    R0,TNSZZTBL                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC                                                      
         MVI   SPACING,1                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
GRTL0042 EQU   *                                                                
         LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0036                                                         
*                                                                               
         LA    R6,TNSZZTAL      NON OFFICE ZZ ALLOC DOLLAR TOTAL(YEAR)          
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZTCR   NON OFFICE ZZ CURRENT DOLLAR TOTAL(YEAR)           
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TNSZZTBD    NON OFFICE ZZ BUDGET REVENUE $ YEAR               
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TNSZZTBL    NON OFFICE ZZ BILLING REVENUE $ YEAR              
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
GRTL0044 EQU   *                                                                
         MVC   P+1(20),=C'ALL OTHER OFFICES-->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    *+10                                                             
         MVC   P+1(20),=C'ALL OTHER STATIONS->'                                 
         GOTO1 REPORT                                                           
*        MVI   FORCEFUT,C'Y'       FORCE FOOTING OUT                            
*                                                                               
GRTL0046 LA    R5,1                COUNTER                                      
         BCTR  R5,0                                                             
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    GRTL0056            YES                                          
         CLI   SUBPRG,1            BILLING REPORT?                              
         BE    GRTL0052            YES                                          
*                                  BUDGET REPORT                                
*                                                                               
GRTL0048 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TSTLALL$     ALLOCATION DOLLAR TOTAL                          
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    GRTL0050                                                         
*                                                                               
         L     R0,TSTLTAL$                                                      
         A     R0,0(R6)            GRAND TOTAL ALLOC DOLLAR(YEAR)               
         ST    R0,TSTLTAL$                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLBUD$     BUDGET REVENUE $                                 
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTLTBD$                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVEN(YEAR)               
         ST    R0,TSTLTBD$                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    GRTL0050            NO                                           
*                                                                               
GRTL0050 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0048                                                         
*                                                                               
         LA    R6,TSTLTAL$     ALLOCATION DOLLAR TOTAL (YEAR)                   
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLTBD$     BUDGET REVENUE $ (YEAR)                          
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0064                                                         
*                                                                               
GRTL0052 EQU   *                   BILLING REPORT                               
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TSTLCUR$     BILLING DOLLAR TOTAL                             
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BZ    GRTL0054                                                         
*                                                                               
         L     R0,TSTLTCR$                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING DOLLAR (YEAR)            
         ST    R0,TSTLTCR$                                                      
*        EDIT  (4,TSTLTCR$),(10,P),COMMAS=YES    **TEST**                       
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLBIL$     BILLING REVENUE $                                
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTLTBL$                                                      
         A     R0,0(R6)            GRAND TOTAL BILLING REVENUE (YEAR)           
         ST    R0,TSTLTBL$                                                      
*                                                                               
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    GRTL0054            NO                                           
GRTL0054 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0052                                                         
*                                                                               
         LA    R6,TSTLTCR$     BILLING DOLLAR TOTAL (YEAR)                      
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLTBL$     BILLING REVENUE $ (YEAR)                         
         LA    R3,P+PCOL3                                                       
         BAS   R8,TOTEDDEC                                                      
         B     GRTL0064                                                         
*                                                                               
GRTL0056 EQU   *                                                                
         LR    R4,R5                                                            
         SLA   R4,2                                                             
         LA    R6,TSTLCUR$     CURRENT DOLLAR TOTAL                             
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0058                                                         
         LA    R6,TSTLALL$     ALLOCATION DOLLAR TOTAL                          
         LA    R6,0(R4,R6)                                                      
         OC    0(4,R6),0(R6)                                                    
         BNZ   GRTL0060                                                         
         B     GRTL0062                                                         
*                                                                               
GRTL0058 EQU   *                                                                
         L     R0,TSTLTCR$                                                      
         A     R0,0(R6)            GRAND TOTAL CURRENT DOLLAR (YEAR)            
         ST    R0,TSTLTCR$                                                      
*                                                                               
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLALL$     ALLOCATION DOLLAR TOTAL                          
         LA    R6,0(R4,R6)                                                      
*                                                                               
GRTL0060 EQU   *                                                                
         L     R0,TSTLTAL$                                                      
         A     R0,0(R6)            GRAND TOTAL ALLOCAT DOLAR (YEAR)             
         ST    R0,TSTLTAL$                                                      
*                                                                               
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLBUD$     BUDGET REVENUE $                                 
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTLTBD$                                                      
         A     R0,0(R6)            GRAND TOTAL BUDGET REVENUE(YEAR)             
         ST    R0,TSTLTBD$                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX1           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TSTLBIL$     BILLING REVENUE $                                
         LA    R6,0(R4,R6)                                                      
*                                                                               
         L     R0,TSTLTBL$                                                      
         A     R0,0(R6)            GRAND TOTAL BILLIN REVENUE(YEAR)             
         ST    R0,TSTLTBL$                                                      
*                                                                               
         L     R0,0(R6)                                                         
         ST    R0,INDEX2           SAVE FOR INDEX CALCULATION                   
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
         BAS   RE,INDXCALC         CALCULATE INDEXED FOR GRAND TOTS             
         MVI   SPACING,2                                                        
         GOTO1 =A(GETMNTH2),DMCB,(RC),RR=RELO                                   
         GOTO1 REPORT                                                           
*        CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
*        BE    GRTL0062            NO                                           
GRTL0062 LA    R5,1(R5)                                                         
         CH    R5,=H'12'                                                        
         BL    GRTL0056                                                         
*                                                                               
         LA    R6,TSTLTCR$     CURRENT DOLLAR TOTAL (YEAR)                      
         LA    R3,P+PCOL2                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLTAL$     ALLOCATION DOLLAR TOTAL (YEAR)                   
         LA    R3,P+PCOL1                                                       
         BAS   R8,TOTEDIT                                                       
         LA    R6,TSTLTBD$     BUDGET REVENUE $ (YEAR)                          
         LA    R3,P+PCOL4                                                       
         BAS   R8,TOTEDDEC                                                      
         LA    R6,TSTLTBL$     BILLING REVENUE $                                
         LA    R3,P+PCOL5                                                       
         BAS   R8,TOTEDDEC                                                      
GRTL0064 EQU   *                                                                
         MVC   P+1(20),=C'GRAND TOTAL ------->'                                 
         GOTO1 REPORT                                                           
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
         XC    TSTLPRI$(196),TSTLPRI$   ZERO OUT ACCUMULATOR (MONTH)            
         XC    TSTZZPRI(196),TSTZZPRI   ZERO OUT ACCUMULATOR (MONTH)            
         XC    TNSZZPRI(196),TNSZZPRI   ZERO OUT ACCUMULATOR (MONTH)            
         XC    TSTLTPR$(20),TSTLTPR$   ZERO OUT ACCUMULATOR (YEAR)              
         XC    TSTZZTPR(20),TSTZZTPR   ZERO OUT ACCUMULATOR (YEAR)              
         XC    TNSZZTPR(20),TNSZZTPR   ZERO OUT ACCUMULATOR (YEAR)              
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*  CHECK QCONTYPE:  IF PRESENT, EXTRACT ONLY THAT ALLOCATION.  IF               
*        NOT PRESENT, CYCLE 02 ELEMENTS, ADD UP THE ALLOCATIONS.                
*        FOR TEST PURPOSES, IF NO 02 ELEMENTS, USE BASIC 01                     
*        ALLOCATION (KEY THIS TO REQUESTOR OPTION)                              
*                                                                               
*                                                                               
*                                                                               
         PRINT GEN                                                              
GETALLOC NMOD1 0,*GALL*                                                         
         PRINT NOGEN                                                            
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   INCFLAG,C'N'        SET FLAG: DON'T INCLUDE ALLOCATION           
         XC    SAVEALL$,SAVEALL$   ZERO OUT                                     
         CLI   QCONTYP2,C' '       CON TYPE FILTER ENTERED?                     
         BE    GA001               NO  - INCLUDE BASIC                          
         TM    QCONTYPE,X'40'      UPPER OR LOWER CASE?                         
         BO    GA002               ON: UPPER=INCLUDE/NO BASIC                   
GA001    EQU   *                                                                
         LA    R6,RBUDELEM         TAKE 'BASIC' ALLOCATION                      
         GOTO1 =A(GA100),DMCB,(RC),RR=RELO                                      
*        BAS   R8,GA100            TOTAL UP THE ALLOCATIONS                     
GA002    EQU   *                                                                
         LA    R6,RBUDREC          TAKE CONTRACT TYPE ALLOCS                    
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   GA099                                                            
GA004    EQU   *                                                                
         CLI   QCONTYP2,C' '          CON TYPE FILTER ENTERED?                  
         BE    GA006                  NO  - DON'T TEST FOR IT                   
         TM    QCONTYPE,X'40'         UPPER OR LOWER CASE?                      
         BO    GA005                  ON:  UPPER=INCLUDE TYPE                   
         CLC   QCONTYP2,DCONTYP(R6)   OFF: LOWER=EXCLUDE TYPE                   
         BE    GA008                  NOT RIGHT TYPE - SKIP                     
         B     GA006                                                            
GA005    EQU   *                                                                
         CLC   QCONTYP2,DCONTYP(R6)   YES - CHECK FOR IT                        
         BNE   GA008                  NOT RIGHT TYPE - SKIP                     
GA006    EQU   *                                                                
         GOTO1 =A(GA100),DMCB,(RC),RR=RELO                                      
*        BAS   R8,GA100            TOTAL UP THE ALLOCATIONS                     
GA008    EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT 02 ELEMENT                          
         BE    GA004               FOUND - TAKE ITS $                           
*        XC    CONTOTS,CONTOTS     ZERO WORKAREA                                
GA099    EQU   *                                                                
         XIT1                      RETURN                                       
*                                                                               
GA100    NMOD1 0,*GA100*                                                        
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,SAVEALL$         ADDRESS ACCUMULATOR                          
         MVI   INCFLAG,C'Y'        SET FLAG: INCLUDE MONTHLY FIGURES            
         ZIC   RE,STARTMO          REQUEST START MONTH                          
         BCTR  RE,0                MAKE ZERO RELATIVE                           
         ZIC   RF,ENDMO            REQUEST END MONTH                            
         CR    RE,RF               MONTHS IN SAME YEAR?                         
         BNH   GA122               START MO NOT > END MONTH                     
         LA    RF,12(RF)           END MONTH < START: DIFF YRS                  
GA122    EQU   *                                                                
         LA    R5,12                                                            
         MVC   BUDYEAR(2),QSTART   BUDGET YEAR                                  
         NI    FLAG,X'FF'-YRADDED                                               
         SR    RF,RE               CALCULATE NUMBER OF MONTHS                   
         LR    R4,RE               CURRENT MONTH NUMBER                         
         MH    RE,=H'4'            MULT START BY SIZE (4 BYTES)                 
         LA    RE,2(RE,R6)         FIND FIRST BUCKET                            
GA123    EQU   *                                                                
*                                                                               
         CR    R4,R5               NEXT YEAR ?                                  
         BNH   GA130                                                            
         TM    FLAG,YRADDED        SHOULD WE GO TO THE NEXT YEAR?               
         BO    GA130               NO                                           
         PACK  FULL,BUDYEAR        YES- IT IS JAN OF NEXT YEAR                  
         AP    FULL,=P'1'                                                       
         EDIT  FULL,(2,BUDYEAR)    NEXT YEAR                                    
         LA    R4,1                MONTH - JANUARY                              
         OI    FLAG,YRADDED        NEXT YEAR                                    
*                                                                               
GA130    EQU   *                                                                
         L     R0,0(RE)               LOAD 'TABLE ENTRY' TO REGISTER            
         A     R0,0(R3)               ADD PREVIOUS 'CONTOTS' TO REG             
         ST    R0,0(R3)               STORE TO PREVIOUS 'SAVEALL$'              
*                                                                               
         MVC   4(2,R3),BUDYEAR        YEAR                                      
         LA    R4,1(R4)               CURRENT MONTH NUMBER                      
         EDIT  (R4),(2,6(R3)),FILL=0                                            
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT BUCKET                          
         LA    R3,8(R3)            BUMP TO NEXT ENTRY IN ARRAY                  
         BCT   RF,GA123            DO EACH BUCKET                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DALLOC$  EQU   RBUD$TOT-RBUDELEM                                                
DALL$BAS EQU   RBUDSTOT-RBUDELEM                                                
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
*                                                                               
*                                                                               
CONDOLRS NMOD1 0,*CONDOL*                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         LA    R6,SORTREC+SCURR                                                 
         LA    R3,P+PCOL2                                                       
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COND0004            YES                                          
         LA    R3,P+PCOL1          NO  - PUT INTO 1ST COLUMN                    
COND0004 EQU   *                                                                
         EDIT  (4,(R6)),(13,(R3)),COMMAS=YES                                    
         OC    SORTREC+SCOMM(4),SORTREC+SCOMM  ANY RATE AVAILABLE?              
         BNZ   COND0008            YES                                          
         MVI   13(R3),C'*'         FLAG AS 'NOT IN TOTALS'                      
         B     COND0068            SKIP TOTALLING                               
COND0008 EQU   *                                                                
*                                                                               
*   PUT OUT RATE FROM CONTRACT CARD, ON OFF-CHANCE THAT THERE WAS NO            
*     BUDGET RECORD WHICH FILLED IN THE FIELD                                   
*                                                                               
         LA    R3,P+PCOL3COM       RATE: COMBINED REPORT                        
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COND0012            YES                                          
         LA    R3,P+PCOL2COM       RATE: BUDGET OR BILLING REPORT               
COND0012 EQU   *                                                                
         LA    R6,SORTREC+SCOMM          COMM RATE FROM BUD RECORD              
         EDIT  (4,(R6)),(10,(R3))                                               
         MVC   GENWORK(4),6(R3)          EDIT 4 DECL PLACE RATE                 
         MVC   7(4,R3),GENWORK                                                  
         MVI   6(R3),C'.'                INSERT DECIMAL POINT                   
         CLI   SORTREC+SDEFCOM,C'Y'      DEFAULT RATE USED?                     
         BNE   COND0016                  NO  - DON'T FLAG                       
         MVI   11(R3),C'*'               YES - FLAG ON REPORT                   
COND0016 EQU   *                                                                
*                                                                               
*   CALCULATE ACTUAL REVENUES FROM CURRENT BILLING                              
*                                                                               
COND0020 EQU   *                                                                
         MVC   CONTOTS(4),SORTREC+SCURR                                         
         OC    CONTOTS(4),CONTOTS                                               
         BZ    COND0048            ENSURE NO ZERO DIVIDE                        
         L     R3,CONTOTS          CURR BILLING IN REGS 2,3                     
         MVC   CONTOTS(4),SORTREC+SCOMM             RATE                        
         OC    CONTOTS(4),CONTOTS                                               
         BNZ   COND0024            ENSURE NO ZERO DIVIDE                        
         SR    R3,R3               NO COMMISSION: ZERO OUT FINAL RESULT         
         B     COND0048            NOW DO TOTALS                                
COND0024 EQU   *                                                                
         L     R4,CONTOTS          COMMISH RATE IN REG 4                        
         MR    R2,R4               MULTIPLY THEM                                
         LH    R4,=H'5000'                                                      
         AR    R3,R4               HALF ADD FOR ROUNDING                        
         LH    R4,=H'10000'                                                     
         DR    R2,R4               DECIMAL ALIGN                                
         LA    R6,P+PCOL5                                                       
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BE    COND0028            YES                                          
         LA    R6,P+PCOL3          NO  - PUT INTO 3RD COLUMN                    
COND0028 EQU   *                                                                
         EDIT  (R3),(13,(R6)),2,COMMAS=YES                                      
*                                                                               
*   CALCULATED REVENUE IS IN 'R3' AT THIS POINT - IT IS OPPORTUNE TO            
*      SAVE IT BEFORE GOING FURTHER                                             
*                                                                               
*                                                                               
*    RUN TOTALS FOR BILLING REV $ FROM BUDGET RECORD                            
*                                                                               
         ST    R3,INDEX2           SAVE BILLING REV $ FOR INDEX                 
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R6,TOTLBIL$         ACCUMULATOR FOR BILLNG REV $                 
         A     R0,0(R6)            ADD ACCUM TO CON REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R4,TSTLBIL$         ACCUMULATOR FOR BILLNG REV $                 
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R0,0(R4)            ADD ACCUM TO CON REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R6,COMPBIL$         ACCUMULATOR FOR BILLNG REV $ (YEAR)          
         A     R0,0(R6)            ADD ACCUM TO CON REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LA    R4,COMPTBL$         ACCUMULATOR FOR BILLNG REV $ (MONTH)         
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R0,0(R4)            ADD ACCUM TO CON REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R0,R3               ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE BILLNG REV $                            
         CLI   STYP,C'1'                                                        
         BNE   COND0032                                                         
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   COND0040            NO                                           
         B     COND0036            YES                                          
COND0032 EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'   STATION=ZZZZ?                       
         BNE   COND0040            NO                                           
COND0036 EQU   *                                                                
         LA    R6,ZZBIL$           YES - ACCUMULATE ZZ ALLOC (YEAR)             
         LA    R3,ZZTBL$           YES - ACCUMULATE ZZ ALLOC (MONTH)            
         LA    R5,TOTZZBIL         ALSO  ACCUMULATE TOT ZZ ALLOC                
         LA    R4,TSTZZBIL         ALSO  ACCUMULATE TOT ZZ ALLOC                
         B     COND0044                                                         
COND0040 EQU   *                                                                
         LA    R6,NOZZBIL$   ACCUMULATE IN NOT ZZ ALLOCATION (YEAR)             
         LA    R3,NOZZTBL$   ACCUMULATE IN NOT ZZ ALLOCATION (MONTH)            
         LA    R5,TNOZZBIL         ALSO  ACCUMULATE TOT NOT ZZ ALLOC            
         LA    R4,TNSZZBIL         ALSO  ACCUMULATE TOT NOT ZZ ALLOC            
COND0044 EQU   *                                                                
         LR    R8,R0                                                            
         A     R0,0(R6)            ADD ACCUM TO CON REC AMT (YEAR)              
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,RF                                                            
         A     RF,0(R5)            ADD ACCUM TO CON REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R0,0(R4)            ADD ACCUM TO CON REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R4,R3                                                            
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R8,0(R4)            ADD ACCUM TO CON REC AMT (MONTH)             
         ST    R8,0(R4)            STORE TOTAL BACK                             
COND0048 EQU   *                                                                
         LA    R6,SORTREC+SCURR    CURRENT $ FROM CONTRACT REC                  
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,TOTLCUR$         ACCUMULATOR FOR CURRENT $                    
         A     R0,0(R6)            ADD ACCUM TO CONTRACT REC AMT                
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SCURR    CURRENT $ FROM CONTRACT REC                  
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R4,TSTLCUR$         ACCUMULATOR FOR CURRENT $                    
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
*        GOTO1 REPORT              **TEST**                                     
*        LR    R5,R0               **TEST                                       
*        EDIT  (4,(R4)),(10,P),COMMAS=YES,ZERO=NOBLANK **TEST**                 
*        LR    R0,R5               **TEST                                       
         A     R0,0(R4)            ADD ACCUM TO CONTRACT REC AMT                
         ST    R0,0(R4)            STORE TOTAL BACK                             
*        MVC   P+20(4),=C'SUM='    **TEST**                                     
*        EDIT  (4,(R4)),(10,P+25),COMMAS=YES,ZERO=NOBLANK **TEST**              
*        GOTO1 REPORT              **TEST**                                     
         LA    R6,SORTREC+SCURR    CURRENT $ FROM CONTRCT REC (AGAIN)           
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,COMPCUR$         ACCUMULATOR FOR CURRENT $ (YEAR)             
         A     R0,0(R6)            ADD ACCUM TO CONTRACT REC AMT                
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SCURR    CURRENT $ FROM CONTRCT REC (AGAIN)           
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R4,COMPTCR$         ACCUMULATOR FOR CURRENT $ (MONTH)            
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R0,0(R4)            ADD ACCUM TO CONTRACT REC AMT                
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SCURR    CURRENT $ FROM CONTRCT REC (AGAIN)           
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE CURRENT $                               
         CLI   STYP,C'1'                                                        
         BNE   COND0052                                                         
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   COND0060            NO                                           
         B     COND0056            YES                                          
COND0052 EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'  STATION=ZZZZ?                        
         BNE   COND0060            NO                                           
COND0056 EQU   *                                                                
         LA    R6,ZZCUR$           YES - ACCUMULATE ZZ CURRENT (YEAR)           
         LA    R3,ZZTCR$           YES - ACCUMULATE ZZ CURRENT (MONTH)          
         LA    R5,TOTZZCUR         ALSO  ACCUMULATE TOT ZZ CURRENT              
         LA    R4,TSTZZCUR         ALSO  ACCUMULATE TOT ZZ CURRENT              
         B     COND0064                                                         
COND0060 EQU   *                                                                
         LA    R6,NOZZCUR$         ACCUMULATE NOT ZZ CURRENT (YEAR)             
         LA    R3,NOZZTCR$         ACCUMULATE NOT ZZ CURRENT (MONTH)            
         LA    R5,TNOZZCUR         ALSO  ACCUMULATE TOT NOT ZZ CURRENT          
         LA    R4,TNSZZCUR         ALSO  ACCUMULATE TOT NOT ZZ CURRENT          
COND0064 EQU   *                                                                
         LR    R8,R0                                                            
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LR    R0,RF                                                            
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R0,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R4)            STORE TOTAL BACK                             
         LR    R4,R3                                                            
         GOTO1 =A(FNDBUCK),DMCB,(RC),RR=RELO                                    
         A     R8,0(R4)            ADD ACCUM TO BUD REC AMT                     
         ST    R8,0(R4)            STORE TOTAL BACK                             
         B     COND0068                                                         
COND0068 EQU   *                                                                
         CLI   SUBPRG,2            COMBINED REPORT?                             
         BNE   COND0072            NO                                           
         BAS   RE,INDXCLC2         YES - CALCULATE INDEX VALUE                  
COND0072 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCULATE INDEX OF PROJECTED REV $ TO BILLING REV $                         
*                                                                               
INDXCLC2 NTR1                                                                   
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         OC    INDEX2,INDEX2       ANY BILLING REVENUE?                         
         BZ    INXC0004            NO  - DISPLAY ZERO                           
         L     R3,INDEX2           LOAD BILLING REVENUE                         
         LH    R4,=H'10000'        SET DECIMAL ALIGNMENT                        
         MR    R2,R4               MULT BILL REV$ BY 100                        
         OC    INDEX1,INDEX1       ANY BUDGET REVENUE?                          
         BZ    INXC0008            NO  - EXIT ROUTINE                           
         L     R4,INDEX1           YES - LOAD BUDGET REVENUE                    
         SRL   R4,1                DIVIDE BY 2                                  
         AR    R3,R4               HALF-ADD FOR ROUNDING                        
         L     R4,INDEX1           RELOAD BUDGET REVENUE                        
         DR    R2,R4               DIVIDE BILLING BY BUDGET                     
INXC0004 EQU   *                                                                
         LA    R6,P+125                                                         
         EDIT  (R3),(6,(R6)),2,COMMAS=YES                                       
INXC0008 EQU   *                                                                
         XC    INDEX1,INDEX1                                                    
         XC    INDEX2,INDEX2                                                    
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
*                                                                               
* FIND EBCIDIC VALUE OF MONTH (MONTH IS IN REG=R5)                              
*                                                                               
GETMNTH2 NMOD1 0,*FNDBUK*                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         LA    R3,MTABLE                                                        
*                                                                               
GETMTH10 EQU   *                                                                
         LA    R6,1(R5)                                                         
         EDIT  (R6),(2,HALF),FILL=0                                             
         CLC   HALF,0(R3)                                                       
         BE    GETMTH20                                                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,5(R3)            NEXT ENTRY IN THE TABLE                      
         B     GETMTH10                                                         
*                                                                               
GETMTH20 EQU   *                                                                
         MVC   P+PMONTH(3),2(R3)                                                
         MVI   P+PMONTH+3,C'/'                                                  
         MVC   P+PMONTH+4(2),SORTREC+SDATE                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CALCULATE THE DIPLACEMENT IN THE ARRAY FOR THE CORRESPONDING MONTH            
*                                                                               
FNDBUCK  NMOD1 0,*FNDBUK*                                                       
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         PACK  DUB(8),SORTREC+SMONTH(2)                                         
         CVB   R5,DUB                                                           
         BCTR  R5,0                0 RELEVANT                                   
         SLA   R5,2                MULTIPLY IT BY 4 BYTES                       
         AR    R4,R5               CORRECT DISPLACEMENT                         
         XIT1  REGS=(R4)                                                        
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
QREC2D   DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110REREP1L02 05/01/02'                                      
         END                                                                    
