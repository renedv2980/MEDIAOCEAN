*          DATA SET REREP1A02  AT LEVEL 023 AS OF 05/01/02                      
*PHASE RE1A02C,*                                                                
         TITLE 'REREP1A02 - BUDGET WORKSHEET/PRELIM/FINALS'                     
*********************************************************************           
*                                                                   *           
*        REREP1A02 --- REPPAK BUDGET WORKSHEET/PRELIM/FINALS        *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* AUG13/90 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* SEP11/90 (BU ) --- TEMPORARY CHANGES TO SUPPRESS THE PRINTING OF  *           
*                    CONTRACT $, AND TO REARRANGE THE PRINTED OUTPUT*           
*                    TO TAKE ADVANTAGE OF THE SPACE.  CHANGES FLAG'D*           
*                    AS '*--->' INDICATE AN INSTRUCTION WHICH WILL  *           
*                    BE REINSTATED TO REINSERT THE CONTRACT $, AND  *           
*                    REARRANGE THE REPORT AGAIN.  CAREFULLY CHECK TO*           
*                    SEE WHICH FLAGGED INSTRUCTIONS ARE 'PAIRED'.   *           
*                                                                   *           
* OCT30/90 (BU ) --- KILL 'DATE LEFT' TESTING - ADD COMPANY TOTALS  *           
*                    TO EACH SECTION - ACTIVATE 'EXCLUDE TYPE' TEST *           
*                                                                   *           
* NOV08/90 (BU ) --- SPLIT COMPANY TOTALS ONTO A SEPARATE PAGE.     *           
*                                                                   *           
* MAR13/91 (BU ) --- INSTALL AN OFFICE FILTER FOR BUDGET RECORDS    *           
*                                                                   *           
* SEP09/91 (BU ) --- REMOVE '*--->' RESTRICTIONS AND DELETE 'PAIRED'*           
*                    INSTRUCTIONS                                   *           
*                                                                   *           
* OCT18/91 (BU ) --- EXPAND SIZE OF TOTAL DISPLAY FIELDS            *           
*                                                                   *           
* OCT23/91 (BU ) --- RE-INSERT '*--->' RESTRICTIONS, PAIRED CODE.   *           
*                                                                   *           
* OCT24/91 (BU ) --- DISPLAY 0 FOR BUDGET OF ZERO DOLLARS           *           
*                                                                   *           
* OCT28/91 (BU ) --- SUPPRESS 'OFFICE ZZ/STATION ZZZZ TOTAL' LINE   *           
*                                                                   *           
* NOV05/91 (BU ) --- ADJUST FOR NEW MONTABLE SETUP FOR VALUENEW     *           
*                                                                   *           
* JAN08/92 (BU ) --- INCORPORATE CROSS-COMPANY PROCESSING           *           
*                                                                   *           
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                  *           
*                    REREPRGEQU ---> REREPRGEQA                     *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS                                   *           
*                                                                   *           
* MAY23/97 (BU ) --- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* JUN24/97 (BU ) --- SKIP C*MP BUDGET RECORDS                       *           
*                                                                   *           
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
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
RE1A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1A02,R7,RR=RE                                              
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
         B     MODEEXIT                                                         
         EJECT                                                                  
*   GRPINIT: FIRST GROUP MODE SETTING                                           
*                                                                               
GRPINIT  NTR1                                                                   
         XC    SORTREC,SORTREC              SET UP BASIC KEY                    
         MVC   SORTREC+SREP(2),RCONKREP     INSERT REP CODE                     
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
         MVC   SORTREC+SREP(2),RCONKREP      INSERT REP CODE                    
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
         MVC   SORTREC+SREP(2),RCONKREP      INSERT REP CODE                    
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
         SPACE 1                                                                
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
         L     R4,ANEWMON          A(MONTH TABLE)                               
         LA    R3,TMONTBL          A(ACCUMULATORS)                              
*                                                                               
*   FIND REQUEST START DATE IN TABLE.                                           
*                                                                               
POST08   EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST12              FOUND - BEGIN TO PULL FIGURES                
         BH    POST20              NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST08              GO BACK FOR NEXT                             
*                                                                               
POST12   EQU   *                                                                
         CLI   0(R4),0             ANY TABLE ENTRY?                             
         BE    POST20              NO  - EXIT                                   
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    POST20              TABLE > END DATE - EXIT                      
*                                                                               
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         TM    FLAG6(R4),X'01'        ANY INVOICED $ IN BUCKET?                 
         BZ    POST14                 NO  - TAKE ORDERED $                      
         MVC   CONTOTS(4),CUASATIN(R6)    YES = INVOICED THIS YEAR              
         B     POST16                                                           
*                                                                               
POST14   EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)      NO  = ORDERED THIS YEAR               
*                                                                               
POST16   EQU   *                                                                
         MVC   CONTOTS+4(4),PRTOTINV(R6)  INVOICED - LAST YEAR                  
         TM    6(R4),X'02'                TEST ANY PRIOR INVOICES               
         BO    POST18                                                           
         MVC   CONTOTS+4(4),PRTOTORD(R6)  NO - USE PRI TOT EST                  
*                                                                               
POST18   EQU   *                                                                
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD CURRENT BEST $                           
         A     R0,0(R3)            TO ACCUMULATOR CURR BEST $                   
         ST    R0,0(R3)            STORE IT BACK                                
         L     R0,4(R2)            ADD INVOICED LAST YEAR                       
         A     R0,4(R3)            TO ACCUMULATOR INVOICED LAST YEAR            
         ST    R0,4(R3)            STORE IT BACK                                
         L     R0,CONTOTS+8        LOAD TOTAL CURR BEST $                       
         A     R0,CONTOTS          ADD CURRENT BEST $                           
         ST    R0,CONTOTS+8        STORE IT BACK                                
         L     R0,CONTOTS+12       LOAD TOTAL LAST YEAR INV$                    
         A     R0,CONTOTS+4        ADD LAST YEAR INV$                           
         ST    R0,CONTOTS+12       STORE IT BACK                                
*                                                                               
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST12              SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
POST20   EQU   *                                                                
         MVI   CONFLAG,C'Y'        SET CONTRACT FLAG TO YES                     
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*   GRPDONE:  LAST GROUP MODE SETTING                                           
*                                                                               
GRPDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    GDEXIT              YES                                          
         BAS   RE,SORTGEN                                                       
GDEXIT   B     MODEEXIT                                                         
         EJECT                                                                  
*   STADONE:  LAST STATION MODE SETTING                                         
*                                                                               
STADONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    SD099               YES                                          
         BAS   RE,SORTGEN                                                       
SD099    EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*   OFFDONE:  LAST OFFICE MODE SETTING                                          
*                                                                               
OFFDONE  NTR1                                                                   
         CLC   SORTREC,RESETREC    SORT REC ALREADY GENERATED?                  
         BE    ODEXIT              YES                                          
         BAS   RE,SORTGEN                                                       
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
*  OPTION2: W = WORKSHEET     P = PRELIMINARY  F = FINAL                        
*                                                                               
RPTDONE  NTR1                                                                   
         L     RE,VXADDR           MASTER OR NON-MASTER REP?                    
         USING VXADDRD,RE                                                       
         L     RE,VXSREP           A(REP SUBSIDIARY TABLE)                      
         DROP  RE                                                               
         USING RSREPD,RE                                                        
         CLI   MASTREP,0           CHECK MASTER FLAG                            
         BE    RD001               NOT A MASTER - NO SETUP                      
         LA    R1,SUBREPTB         BEGINNING OF TABLE                           
         MVC   RCREPFL,0(R1)       MOVE IN 1ST REP IN TABLE                     
         LA    R1,LSUBREP(R1)      BUMP TO NEXT REP IN TABLE                    
         ST    R1,ASUBREPT         A(NEXT REP IN TABLE)                         
*                                                                               
         DROP  RE                                                               
*                                                                               
RD001    EQU   *                                                                
         XC    KEY,KEY             ESTABLISH FIRST BUDGET KEY                   
         MVI   KEY,X'13'                                                        
         MVC   KEY+16(2),RCREPFL                                                
         MVC   KEY+18(2),QRGPROG   LOAD START YEAR                              
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RD004                                                            
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RD004                                                            
         MVC   KEY+20(5),QSTATION  YES - LOAD INTO KEY                          
*                                                                               
RD004    GOTO1 HIGH                                                             
*                                                                               
RD008    CLI   KEY,X'13'           SAME CODE?                                   
         BNE   RD075               NO  - DONE EXTRACTING                        
         CLC   KEY+16(2),RCREPFL   SAME REP?                                    
         BNE   RD075               NO  - DONE EXTRACTING                        
         CLC   KEY+18(2),QRGPROG   SAME YEAR?                                   
         BNE   RD075               NO  - DONE EXTRACTING                        
         CLC   =C'C*MP',KEY+20     SPECIAL BUDGET?                              
         BE    RD037               YES - SKIP IT                                
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RD009                                                            
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RD009                                                            
         CLC   KEY+20(5),QSTATION  SAME STATION?                                
         BNE   RD075               NO  - DONE EXTRACTING                        
*                                                                               
RD009    EQU   *                                                                
         OC    QOFFICE,QOFFICE     ANY OFFICE ENTERED?                          
         BZ    RD010                                                            
         CLC   QOFFICE(2),SPACES   DITTO LAST COMMENT                           
         BE    RD010                                                            
         CLC   KEY+25(2),QOFFICE   SAME OFFICE?                                 
         BNE   RD037               NO  - SKIP THIS OFFICE                       
RD010    EQU   *                                                                
         MVC   KEYSAV2(27),KEY     SAVE KEY FOR RESTART                         
         CLI   QOPTION2,C'W'       WORKSHEET REPORT ORDERED?                    
         BE    RD011               YES - SKIP BUDGET REC READ                   
         BAS   RE,GETBUD           RETRIEVE BUDGET RECORD                       
         BAS   RE,GETALLOC         DERIVE ALLOCATED $                           
         CLI   INCFLAG,C'Y'        ALLOCATION FOUND?                            
         BNE   RD035               NO  - NO OUTPUT                              
*                                                                               
RD011    EQU   *                                                                
         CLC   KEY+20(5),SAVESTA   STATION PREVIOUSLY SEEN?                     
         BE    RD020               YES - SKIP REREADING IT                      
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
         CLC   KEY+22(5),SAVESTA                                                
         BNE   RDDUMP                                                           
         BAS   RE,GETSTA           RETURN STATION RECORD                        
         MVC   SAVEGRUP,RSTAGRUP   SAVE GROUP FROM STATION RECORD               
         MVC   SAVEMKT,RSTAMKT     SAVE MARKET NAME                             
RD020    EQU   *                                                                
         CLC   QGROUP(1),SPACES    ANY GROUP FILTER?                            
         BE    RD022               NO  - SKIP TEST                              
         CLC   QGROUP(1),RSTAGRUP  YES - BUDGET FOR GRP?                        
         BNE   RD035               NO  - NO SORT OUTPUT                         
RD022    EQU   *                                                                
         CLC   QSBGROUP(1),SPACES      ANY SUBGROUP FILTER?                     
         BE    RD023                   NO  - SKIP TEST                          
         CLC   QSBGROUP(1),RSTAGRUP+1  YES - BUDGET FOR SUBGRP?                 
         BNE   RD035                   NO  - NO SORT OUTPUT                     
*                                                                               
*  RETRIEVE OFFICE RECORD FOR OFFICE NAME: OFFICE IS MINOR, MUST BE             
*        READ FOR EACH RECORD PROCESSED                                         
*                                                                               
RD023    EQU   *                                                                
         XC    KEY,KEY             ESTABLISH OFFICE KEY                         
         CLC   KEYSAV2+25(2),=X'0000'    ANY OFFICE ENTERED?                    
         BNE   RD025                                                            
         MVC   SAVEOFF(20),SPACES                                               
         MVC   SAVEOFF(09),=C'CORPORATE'                                        
         B     RD027                                                            
RD025    EQU   *                                                                
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
RD027    EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         MVI   SORTREC,C'1'                                                     
         MVC   SORTREC+SREP(2),RCREPFL                                          
         MVC   SORTREC+SGROUP(2),SAVEGRUP                                       
         MVC   SORTREC+SSTAT1(5),SAVESTA                                        
         MVC   SORTREC+SOFF1(2),KEYSAV2+25    OFFICE FROM BGT REC               
         CLC   QUESTOR(4),=C'<UHR'            REPLACE NETWORK WITH ZZ?          
         BNE   RD029                          NO                                
*                                                                               
         CLC   SORTREC+SSTAT1(4),=C'KDKA'     REPLACE STATION?                  
         BNE   RD028                                                            
         MVC   SORTREC+SSTAT1(4),=C'ZZZZ'                                       
RD028    EQU   *                                                                
         CLC   SORTREC+SOFF1(2),=C'SL'         REPLACE OFFICE?                  
         BNE   RD029                                                            
         MVC   SORTREC+SOFF1(2),=C'ZZ'                                          
RD029    EQU   *                                                                
         MVC   SORTREC+SALL$(4),SAVEALL$      INSERT ALLOCATED $                
         MVC   SORTREC+SMKTNM(20),SAVEMKT                                       
         MVC   SORTREC+SOFFNM(20),SAVEOFF                                       
         MVI   SORTREC+STYP2,C'0'             SETS SUBTYP TO X'F0'              
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'2'                                                     
         MVC   SORTREC+SGROUP(2),SPACES       NO GRP FOR OFF ORDER              
         MVC   SORTREC+SSTAT2(5),SAVESTA      SWAP KEYS FOR TYPE 2              
         MVC   SORTREC+SOFF2(2),KEYSAV2+25                                      
         CLC   QUESTOR(4),=C'<UHR'            REPLACE NETWORK WITH ZZ?          
         BNE   RD032                          NO                                
         CLC   SORTREC+SSTAT2(4),=C'KDKA'     REPLACE STATION?                  
         BNE   RD031                                                            
         MVC   SORTREC+SSTAT2(4),=C'ZZZZ'                                       
RD031    EQU   *                                                                
         CLC   SORTREC+SOFF2(2),=C'SL'        REPLACE OFFICE?                   
         BNE   RD032                                                            
         MVC   SORTREC+SOFF2(2),=C'ZZ'                                          
RD032    EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'3'        SET UP REGIONAL OUTPUT                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    BUDCTR,=P'3'                   ADD BGT RECS GEN'D                
RD035    EQU   *                                                                
         MVC   KEY(27),KEYSAV2                RESTORE BUDGET KEY                
         GOTO1 READ                                                             
RD037    EQU   *                                                                
         GOTO1 SEQ                 REESTABLISH SEQUENTIAL ACCESSING             
         B     RD008               PROCESS THIS READ                            
*                                                                               
RDDUMP   DC    H'0'                                                             
*                                                                               
RD075    EQU   *                                                                
         L     RE,VXADDR           MASTER OR NON-MASTER REP?                    
         USING VXADDRD,RE                                                       
         L     RE,VXSREP           A(REP SUBSIDIARY TABLE)                      
         DROP  RE                                                               
         USING RSREPD,RE                                                        
         CLI   MASTREP,0           CHECK MASTER FLAG                            
         BE    RD099               NOT A MASTER - NO SETUP                      
         L     R1,ASUBREPT         CHECK NEXT ENTRY IN TABLE                    
         CLC   0(2,R1),=X'FFFF'    ANY ENTRY?                                   
         BE    RD099               NO  - ALL DONE                               
         MVC   RCREPFL,0(R1)       MOVE IN NEXT REP IN TABLE                    
         LA    R1,LSUBREP(R1)      BUMP TO NEXT REP IN TABLE                    
         ST    R1,ASUBREPT         A(NEXT REP IN TABLE)                         
         B     RD001               GO BACK AND PROCESS NEXT                     
*                                                                               
         DROP  RE                                                               
*                                                                               
RD099    EQU   *                                                                
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
*   THE SORTED FILE WILL CONTAIN UP TO 2 RECORDS FOR EACH                       
*     GROUP/STATION/OFFICE.  THE FIRST IS THE BUDGET RECORD,                    
*     WHICH HAS A SUBTYPE OF 0.  THIS RECORD CONTAINS THE ALLOC-                
*     ATION $, IF REQUESTED.  THE SECOND IS THE RECORD CONTAINING               
*     THE CONTRACT DOLLARS, WHICH HAS A SUBTYPE OF 1.  EITHER                   
*     RECORD MAY BE MISSING.  THE REPORT MUST IDENTIFY THESE                    
*     CONDITIONS FOR THE USER.                                                  
*                                                                               
         XC    SORTREC2,SORTREC2                                                
         CLI   QOPTION2,C'W'       WORKSHEET REQUEST?                           
         BNE   RD099A                                                           
         MVI   SUBPRG,0            YES - SET INCREMENT                          
         B     RD100                                                            
*                                                                               
RD099A   EQU   *                                                                
         CLI   QOPTION2,C'P'       PRELIM REQUEST?                              
         BNE   RD099B                                                           
         MVI   SUBPRG,1            YES - SET INCREMENT                          
         B     RD100                                                            
*                                                                               
RD099B   EQU   *                                                                
         CLI   QOPTION2,C'F'       FINAL REQUEST?                               
         BNE   RD100                                                            
         MVI   SUBPRG,2            YES - SET INCREMENT                          
*                                                                               
RD100    EQU   *                                                                
         BAS   R8,GETSORT                                                       
*                                                                               
         CLI   STYP,X'FF'          EOF                                          
         BE    RD198               YES                                          
         OC    SORTREC2,SORTREC2                                                
         BNZ   RD100A              FIRST TIME IF ALL ZERO                       
         BAS   RE,SETUPREP         RESTORE REP RECORD                           
         B     RD112                                                            
RD100A   EQU   *                                                                
         CLC   STYP,SRECTYP2       SAME RECORD TYPE?                            
         BE    RD101               YES                                          
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         BAS   RE,CTOTAL           PRODUCE COMPANY TOTAL                        
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         B     RD112                                                            
*                                                                               
RD101    EQU   *                                                                
*                                                                               
*    BREAK TESTING:   REP BREAK                                                 
*                                                                               
*    DEPENDING UPON THE DATA WHICH COMES IN, NO PREVIOUSLY RECOG-               
*      NIZABLE/USABLE RECORD MAY HAVE BEEN MOVED TO SORTREC2, ALTHO             
*      THE TYPE FIELD HAS BEEN INITIALIZED.  A CHECK FOR EXISTENCE              
*      IS MADE IN THE TWO INSTRUCTIONS IMMEDIATELY BELOW.                       
*                                                                               
         OC    SORTREC2+SREP(2),SORTREC2+SREP                                   
         BZ    RD102                                                            
         CLC   SORTREC+SREP(2),SORTREC2+SREP                                    
         BE    RD102                                                            
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         BAS   RE,CTOTAL           PRODUCE COMPANY TOTAL                        
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         BAS   RE,SETUPREP         RETRIEVE REP RECORD                          
         B     RD106                                                            
*                                                                               
RD102    EQU   *                                                                
*                                                                               
*    BREAK TESTING:   GROUP BREAK                                               
*                                                                               
         CLC   SORTREC+SGROUP(2),SORTREC2+SGROUP                                
         BE    RD103                                                            
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         B     RD106                                                            
*                                                                               
RD103    EQU   *                                                                
*                                                                               
*    BREAK TESTING:   STATION BREAK                                             
*                                                                               
         CLI   STYP,C'1'                                                        
         BNE   RD105                                                            
*                                                                               
*   IF OFFICE = ZZ, FORCE SUBTOTAL BREAK                                        
*                                                                               
         CLC   SORTREC+SOFF1(2),=C'ZZ'                                          
         BNE   RD103A                                                           
*****>   BAS   RE,STOTAL                                                        
RD103A   EQU   *                                                                
         CLC   SORTREC+SSTAT1(5),SORTREC2+SSTAT1                                
         BE    RD106                                                            
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
         B     RD106                                                            
*                                                                               
RD105    EQU   *                                                                
*                                                                               
*    BREAK TESTING:   OFFICE BREAK                                              
*                                                                               
*                                                                               
*   IF STATION = ZZZZ, FORCE SUBTOTAL BREAK                                     
*                                                                               
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'                                       
         BNE   RD105A                                                           
*****>   BAS   RE,STOTAL                                                        
RD105A   EQU   *                                                                
         CLC   SORTREC+SOFF2(2),SORTREC2+SOFF2                                  
         BE    RD106                                                            
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'       FORCE HEADING BREAK                          
*                                                                               
RD106    CLC   SORTREC+1(11),SORTREC2+1  SAME REP/GROUP/STN/OFFICE?             
         BNE   RD112                     NO                                     
         MVI   ACCFLAG,C'Y'              ACCUMULATE TOTALS                      
         PRINT GEN                                                              
*--->    GOTO1 RD200               YES - IT IS A CONTRACT $ RECORD              
*                                                                               
*   IF RD200 IS REACTIVATED, DEACTIVATE TESTDOLR                                
*                                                                               
         BAS   RE,TESTDOLR         TEST DISPLAY OF DOLLARS                      
         GOTO1 REPORT                                                           
         PRINT NOGEN                                                            
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         B     RD130                                                            
*                                                                               
RD112    EQU   *                                                                
         BAS   R8,RD210            LOAD COMMON FIELDS                           
         ZIC   R3,SUBPRG                 COMPUTE REPORT HEADING #               
         ZIC   R6,RCSUBPRG                                                      
         AR    R3,R6                                                            
         STC   R3,RCSUBPRG         RESET HEADER CONTROL                         
         CLI   SSUBTYP,C'0'        CONTRACT OR BUDGET?                          
         BNE   RD120               NOT ZERO = CONTRACT                          
         CLI   QOPTION2,C'W'       WORKSHEET REQUEST?                           
         BE    RD130               YES - NO ALLOCATION $                        
*--->    LA    R3,P+90                                                          
         LA    R3,P+50                                                          
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD                      
         EDIT  (4,(R6)),(10,(R3)),COMMAS=YES,ZERO=NOBLANK                       
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,TOTLALL$         ACCUMULATOR FOR ALLOC $                      
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,COMPALL$         ACCUMULATOR FOR ALLOC $                      
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SALL$    ALLOC $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE ALLOC $                                 
         CLI   STYP,C'1'                                                        
         BNE   RD120A                                                           
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   RD120C              NO                                           
         B     RD120B              YES                                          
RD120A   EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'   STATION=ZZZZ?                       
         BNE   RD120C              NO                                           
RD120B   EQU   *                                                                
         LA    R6,ZZALL$           YES - ACCUMULATE ZZ ALLOC                    
         LA    R5,TOTZZALL         ALSO  ACCUMULATE TOT ZZ ALLOC                
         B     RD120D                                                           
RD120C   EQU   *                                                                
         LA    R6,NOZZALL$         ACCUMULATE IN NOT ZZ ALLOCATION              
         LA    R5,TNOZZALL         ALSO  ACCUMULATE TOT NOT ZZ ALLOC            
RD120D   EQU   *                                                                
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         B     RD130                                                            
*                                                                               
RD120    EQU   *                                                                
         MVI   ACCFLAG,C'Y'        ACCUMULATE TOTALS                            
         CLI   QOPTION2,C'W'       WORKSHEET REPORT ORDERED?                    
         BE    RD121               YES - ACCUMULATE TOTALS                      
         MVI   ACCFLAG,C'N'        NO  - DON'T ACCUMULATE TOTALS                
RD121    EQU   *                                                                
*--->    GOTO1 RD200               CONTRACT $ RECORD                            
*                                                                               
*   IF RD200 IS REACTIVATED, DEACTIVATE TESTDOLR                                
*                                                                               
         BAS   RE,TESTDOLR         TEST DISPLAY OF DOLLARS                      
         CLI   QOPTION2,C'W'       WORKSHEET REPORT ORDERED?                    
         BE    RD122               YES - SKIP 'NO BUDGET' LEGEND                
         MVC   P+1(09),=C'NO BUDGET'                                            
RD122    EQU   *                                                                
*                                                                               
*    DON'T PUT OUT NO-BUDGET RECORD LINE (UNTIL SOMEONE CHANGES                 
*        HIS/HER MIND) IF NOT WORKSHEET                                         
*                                                                               
         CLI   QOPTION2,C'W'       WORKSHEET REPORT ORDERED?                    
         BNE   RD124               NO  - DON'T PRINT 'NO BUDGET' LINES          
         GOTO1 REPORT                                                           
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         B     RD130                                                            
*                                                                               
*    IF NO CALL TO REPORT IS MADE, PRINT-LINE MUST BE CLEARED                   
*                                                                               
RD124    EQU   *                                                                
         MVC   P,SPACES            SPACE PRINT-LINE TO CLEAR                    
         CLI   PRTFLAG,C'X'        FIRST PASS/NO PRINT?                         
         BE    RD125               YES - DON'T RESET FORCEHED                   
         MVI   FORCEHED,C'N'       TURN OFF PAGEBREAK                           
RD125    EQU   *                                                                
         MVI   FORCEFUT,C'N'       TURN OFF FOOTBREAK                           
         MVC   SORTREC2(1),SORTREC SAVE RECORD TYPE ONLY                        
         B     RD100                                                            
*                                                                               
RD130    MVC   SORTREC2,SORTREC    SAVE PROCESSED KEY                           
         B     RD100                                                            
*                                                                               
RD198    CLI   SSUBTYP2,C'0'       LAST RECORD A BUDGET RECORD?                 
         BNE   RD199               FINISHED - END JOB                           
*--->    MVC   P+1(11),=C'BUDGET ONLY'                                          
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         GOTO1 REPORT              PRINT OUT LAST ITEM                          
         MVC   RESETREC,SORTREC    RESTORE CURRENT RECORD                       
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
RD199    EQU   *                                                                
         BAS   RE,GTOTAL           PRODUCE GRAND TOTAL                          
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,CTOTAL           PRODUCE COMPANY TOTAL                        
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
SETUPREP NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),SORTREC+SREP   INSERT REP CODE                         
         GOTO1 HIGH                                                             
         BAS   RE,GETREP                                                        
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
TESTDOLR NTR1                                                                   
         CLC   QUESTOR(5),=C'TEST$'                                             
         BNE   TDOL0099                                                         
         LA    R6,SORTREC+SPRIOR                                                
         LA    R3,P+100                                                         
         EDIT  (4,(R6)),(10,(R3)),COMMAS=YES                                    
         LA    R6,SORTREC+SCURR                                                 
         LA    R3,P+114                                                         
         EDIT  (4,(R6)),(10,(R3)),COMMAS=YES                                    
TDOL0099 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
RD200    NTR1                                                                   
         LA    R6,SORTREC+SPRIOR                                                
         LA    R3,P+50                                                          
         EDIT  (4,(R6)),(10,(R3)),COMMAS=YES                                    
         CLI   ACCFLAG,C'Y'        ACCUMULATE TOTALS?                           
         BNE   RD204               NO  -                                        
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,TOTLPRI$         ACCUMULATOR FOR PRIOR $                      
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SPRIOR   PRIOR $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,COMPPRI$         ACCUMULATOR FOR PRIOR $                      
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SPRIOR   PRIOR $ FROM BUD RECORD (AGAIN)              
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE PRIOR $                                 
         CLI   STYP,C'1'           OFFICE WITHIN STATION REPORT?                
         BNE   RD201A              NO  - STATION WITHIN OFFICE                  
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   RD201C              NO                                           
         B     RD201B              YES                                          
RD201A   EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'   STATION=ZZZZ?                       
         BNE   RD201C              NO                                           
RD201B   EQU   *                                                                
         LA    R6,ZZPRI$           YES - ACCUMULATE ZZ PRIOR                    
         LA    R5,TOTZZPRI         ALSO  ACCUMULATE TOT ZZ PRIOR                
         B     RD201D                                                           
RD201C   EQU   *                                                                
         LA    R6,NOZZPRI$         ACCUMULATE NOT ZZ PRIOR                      
         LA    R5,TNOZZPRI         ALSO  ACCUMULATE TOT NOT ZZ PRIOR            
RD201D   EQU   *                                                                
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
RD202    EQU   *                                                                
         LA    R6,SORTREC+SCURR                                                 
         LA    R3,P+70                                                          
         EDIT  (4,(R6)),(10,(R3)),COMMAS=YES                                    
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,TOTLCUR$         ACCUMULATOR FOR CURRENT $                    
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SCURR    CURRENT $ FROM BUD RECORD (AGAIN)            
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LA    R6,COMPCUR$         ACCUMULATOR FOR CURRENT $                    
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         LA    R6,SORTREC+SCURR    CURRENT $ FROM BUD RECORD (AGAIN)            
         L     R0,0(R6)            ACCUMULATE TOTALS                            
         LR    RF,R0               SAVE CURRENT $                               
         CLI   STYP,C'1'                                                        
         BNE   RD202A                                                           
         CLC   SORTREC+SOFF1(2),=C'ZZ'   OFFICE = ZZ?                           
         BNE   RD202C              NO                                           
         B     RD202B              YES                                          
RD202A   EQU   *                                                                
         CLC   SORTREC+SSTAT2(4),=C'ZZZZ'  STATION=ZZZZ?                        
         BNE   RD202C              NO                                           
RD202B   EQU   *                                                                
         LA    R6,ZZCUR$           YES - ACCUMULATE ZZ CURRENT                  
         LA    R5,TOTZZCUR         ALSO  ACCUMULATE TOT ZZ CURRENT              
         B     RD202D                                                           
RD202C   EQU   *                                                                
         LA    R6,NOZZCUR$         ACCUMULATE NOT ZZ CURRENT                    
         LA    R5,TNOZZCUR         ALSO  ACCUMULATE TOT NOT ZZ CURRENT          
RD202D   EQU   *                                                                
         A     R0,0(R6)            ADD ACCUM TO BUD REC AMT                     
         ST    R0,0(R6)            STORE TOTAL BACK                             
         A     RF,0(R5)            ADD ACCUM TO BUD REC AMT                     
         ST    RF,0(R5)            STORE TOTAL BACK                             
         B     RD204                                                            
RD204    EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   ESTABLISH COMMON PRINT FIELDS HERE                                          
*                                                                               
RD210    CLI   STYP,C'1'                 STATION/OFFICE REPORT?                 
         BNE   RD212                     NO                                     
         MVI   OFFFLAG,C'N'        SET OFF REP SWITCH OFF                       
         MVI   RCSUBPRG,0                SET SUBPRG TO STATION                  
         MVC   P+20(2),SORTREC+SOFF1     YES - SHOW OFFICE                      
         MVC   P+27(15),SORTREC+SOFFNM   SHOW OFFICE NAME                       
         B     RD214                                                            
RD212    MVC   P+20(5),SORTREC+SSTAT2    NO - SHOW STATION                      
         MVC   P+27(15),SORTREC+SMKTNM   SHOW STATION MARKET                    
         MVI   RCSUBPRG,3                SET SUBPRG TO OFFICE                   
         MVI   OFFFLAG,C'Y'              SET OFF REP SWITCH ON                  
         CLI   STYP,C'2'                                                        
         BE    RD214                                                            
         MVI   RCSUBPRG,6                SET SUBPRG TO REGIONAL                 
         MVI   OFFFLAG,C'N'              SET OFF REP SWITCH OFF                 
RD214    EQU   *                                                                
         CLI   QOPTION2,C'W'       WORKSHEET OPTION?                            
         BNE   RD215                                                            
*--->    MVC   P+90(20),UNDRSCOR   YES - UNDERSCORE FIELD                       
         MVC   P+50(20),UNDRSCOR   YES - UNDERSCORE FIELD                       
         B     RD219                                                            
RD215    CLI   QOPTION2,C'P'       PRELIMINARY OPTION?                          
         BNE   RD219                                                            
*                                                                               
*  NOTE - 'FINAL' OPTION GETS NO UNDERSCORES!!!!                                
*                                                                               
*  'REVISED' FIELD ON PRELIMINARY/OFFICE REPORT ONLY                            
*                                                                               
         CLI   STYP,C'2'                                                        
         BNE   RD219                                                            
*--->    MVC   P+110(20),UNDRSCOR  YES - UNDERSCORE FIELD                       
         MVC   P+070(20),UNDRSCOR  YES - UNDERSCORE FIELD                       
RD219    MVI   SPACING,2           ENTIRE REPORT IS DOUBLE SPACED               
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
*  CHECK QCONTYPE:  IF PRESENT, EXTRACT ONLY THAT ALLOCATION.  IF               
*        NOT PRESENT, CYCLE 02 ELEMENTS, ADD UP THE ALLOCATIONS.                
*        FOR TEST PURPOSES, IF NO 02 ELEMENTS, USE BASIC 01                     
*        ALLOCATION (KEY THIS TO REQUESTOR OPTION)                              
*                                                                               
*                                                                               
DALLOC$  EQU   RBUD$TOT-RBUDELEM                                                
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
*                                                                               
GETALLOC NTR1                                                                   
         MVI   INCFLAG,C'N'        SET FLAG: DON'T INCLUDE ALLOCATION           
         XC    SAVEALL$,SAVEALL$   ZERO OUT ACCUMULATOR                         
         LA    R3,SAVEALL$         ADDRESS ACCUMULATOR                          
         LA    R2,CONTOTS          ADDRESS ACCUM WORKAREA                       
         CLI   QCONTYP2,C' '       CON TYPE FILTER ENTERED?                     
         BE    GA001               NO  - INCLUDE BASIC                          
         TM    QCONTYPE,X'40'      UPPER OR LOWER CASE?                         
         BO    GA002               ON: UPPER=INCLUDE/NO BASIC                   
GA001    EQU   *                                                                
         LA    R6,RBUDELEM         TAKE 'BASIC' ALLOCATION                      
         BAS   R8,GA100                                                         
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
         BAS   R8,GA100            TOTAL UP THE ALLOCATIONS                     
GA008    EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT 02 ELEMENT                          
         BE    GA004               FOUND - TAKE ITS $                           
         XC    CONTOTS,CONTOTS     ZERO WORKAREA                                
GA099    EQU   *                                                                
         B     MODEEXIT            RETURN                                       
*                                                                               
GA100    EQU   *                                                                
         MVI   INCFLAG,C'Y'        SET FLAG: INCLUDE ALLOCATION                 
         MVC   CONTOTS(4),DALLOC$(R6) UNLOAD CONTYPE ALLOC$                     
         L     R0,0(R2)               ADD 'CONTOTS' TO REGISTER                 
         A     R0,0(R3)               ADD PREVIOUS 'CONTOTS' TO REG             
         ST    R0,0(R3)               STORE TO PREVIOUS 'CONTOTS'               
         BR    R8                                                               
*                                                                               
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*  GRAND/SUBTOTAL ROUTINES FOR PRINTING                                         
*                                                                               
STOTAL   NTR1                                                                   
         CLC   SORTREC(10),ZZKEY   SAME KEY UP TO TYPE2?                        
         BE    STOT0099            YES - DON'T SHOW TOTS AGAIN                  
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         MVC   P+1(16),=C'SUB TOTAL ----->'                                     
         LA    R6,TOTLPRI$         PRIOR DOLLAR TOTAL                           
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLALL$         ALLOCATION DOLLAR TOTAL                      
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
STOT0099 EQU   *                                                                
         MVC   ZZKEY,SORTREC       SET DUPLICATE TEST                           
         B     MODEEXIT                                                         
         SPACE 3                                                                
GTOTAL   NTR1                                                                   
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         CLI   PRTFLAG,C'Y'        ANYTHING TO PRINT?                           
         BNE   SGTOT99             NO- GET OUT                                  
         MVI   PRTFLAG,C'N'        TURN OFF FLAG                                
         OC    TOTZZPRI(12),TOTZZPRI ANY ZZ/ZZZZ TOTALS?                        
         BZ    GTOT0020            NO  -                                        
         B     GTOT0006            SKIP OFFICE ZZ TOTAL LINE                    
*                                                                               
         MVC   P+1(20),=C'OFFICE ZZ TOTAL---->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    GTOT0004            YES                                          
         MVC   P+1(20),=C'STATION ZZZZ TOTAL->'                                 
GTOT0004 EQU   *                                                                
         LA    R6,TOTZZPRI         OFF=ZZ/STA=ZZZZ PRIOR DOLLAR TOTAL           
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZCUR         OFF=ZZ/STA=ZZZZ CURRENT DOLLAR TOTAL         
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTZZALL         OFF=ZZ/STA=ZZZZ ALLOC DOLLAR TOTAL           
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
GTOT0006 EQU   *                                                                
         MVC   P+1(20),=C'ALL OTHER OFFICES-->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    GTOT0008            YES                                          
         MVC   P+1(20),=C'ALL OTHER STATIONS->'                                 
GTOT0008 EQU   *                                                                
*                                                                               
*   NON-OFFICE-ZZ/NON-STATION-ZZZZ FIGURES                                      
*                                                                               
         LA    R6,TNOZZPRI         NON OFFICE ZZ PRIOR DOLLAR TOTAL             
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZCUR         NON OFFICE ZZ CURRENT DOLLAR TOTAL           
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TNOZZALL         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
GTOT0020 EQU   *                                                                
         MVC   P+1(20),=C'GRAND TOTAL ------->'                                 
         LA    R6,TOTLPRI$         PRIOR DOLLAR TOTAL                           
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,TOTLALL$         ALLOCATION DOLLAR TOTAL                      
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   OFFFLAG,C'N'        OFFICE REPORT?                               
         BE    SGTOT99             NO                                           
         MVI   FORCEFUT,C'Y'       FORCE FOOTING OUT                            
         GOTO1 REPORT                                                           
SGTOT99  EQU   *                                                                
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
         XC    TOTLPRI$(12),TOTLPRI$   ZERO OUT ACCUMULATOR                     
         XC    TOTZZPRI(12),TOTZZPRI   ZERO OUT ACCUMULATOR                     
         XC    TNOZZPRI(12),TNOZZPRI   ZERO OUT ACCUMULATOR                     
         B     MODEEXIT                                                         
         SPACE 3                                                                
CTOTAL   NTR1                                                                   
         MVC   RESETREC,SORTREC    SAVE CURRENT RECORD                          
         MVC   SORTREC,SORTREC2    RESET PREVIOUS FOR PRINTING                  
         MVC   SORTREC+1(9),SPACES                                              
         MVC   SORTREC+SMKTNM(20),=C'COMPANY TOTAL       '                      
         MVC   SORTREC+SOFFNM(20),=C'COMPANY TOTAL       '                      
         OC    ZZPRI$(12),ZZPRI$   ANY ZZ/ZZZZ TOTALS?                          
         BZ    CTOT0020            NO                                           
         B     CTOT0006            SKIP OFFICE ZZ TOTAL LINE                    
*                                                                               
*   OFFICE-ZZ/STATION-ZZZZ FIGURES                                              
*                                                                               
         MVC   P+1(20),=C'OFFICE ZZ TOTAL---->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    CTOT0004            YES                                          
         MVC   P+1(20),=C'STATION ZZZZ TOTAL->'                                 
CTOT0004 EQU   *                                                                
         LA    R6,ZZPRI$           OFFICE ZZ PRIOR DOLLAR TOTAL                 
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,ZZCUR$           OFFICE ZZ CURRENT DOLLAR TOTAL               
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,ZZALL$           OFFICE ZZ ALLOCATION DOLLAR TOTAL            
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
CTOT0006 EQU   *                                                                
*                                                                               
*   NON-OFFICE-ZZ/NON-STATION-ZZZZ FIGURES                                      
*                                                                               
         MVC   P+1(20),=C'ALL OTHER OFFICES-->'                                 
         CLI   STYP,C'1'           OFFICE REPORT?                               
         BE    CTOT0008            YES                                          
         MVC   P+1(20),=C'ALL OTHER STATIONS->'                                 
CTOT0008 EQU   *                                                                
         LA    R6,NOZZPRI$         NON OFFICE ZZ PRIOR DOLLAR TOTAL             
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZCUR$         NON OFFICE ZZ CURRENT DOLLAR TOTAL           
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,NOZZALL$         NON OFFICE ZZ ALLOC DOLLAR TOTAL             
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
CTOT0020 EQU   *                                                                
         MVC   P+1(20),=C'COMPANY TOTAL------>'                                 
         LA    R6,COMPPRI$         PRIOR DOLLAR TOTAL                           
         LA    R3,P+47                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,COMPCUR$         CURRENT DOLLAR TOTAL                         
         LA    R3,P+67                                                          
*--->    BAS   R8,TOTEDIT                                                       
         LA    R6,COMPALL$         ALLOCATION DOLLAR TOTAL                      
*--->    LA    R3,P+87                                                          
         LA    R3,P+47                                                          
         BAS   R8,TOTEDIT                                                       
         XC    COMPPRI$(12),COMPPRI$   ZERO OUT ACCUMULATOR                     
         XC    ZZPRI$(12),ZZPRI$       ZERO OUT ACCUMULATOR                     
         XC    NOZZPRI$(12),NOZZPRI$   ZERO OUT ACCUMULATOR                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   SORTREC,RESETREC    RESTORE CURRENT RECORD                       
         B     MODEEXIT                                                         
*                                                                               
TOTEDIT  EQU   *                                                                
         EDIT  (4,(R6)),(13,(R3)),COMMAS=YES                                    
         BR    R8                  RETURN                                       
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1                                                                   
         CLI   CONFLAG,C'Y'        CONFLAG SET?                                 
         BNE   SG099               NO  - DON'T PUT OUT RECORD ON BREAK          
         MVI   CONFLAG,C'N'        RESET FLAG TO NO                             
         CLC   QUESTOR(4),=C'<UHR'         REPLACE NETWORK WITH ZZ?             
         BNE   SG015                       NO                                   
         CLC   SORTREC+SSTAT1(4),=C'KDKA'  REPLACE STATION?                     
         BNE   SG010                                                            
         MVC   SORTREC+SSTAT1(4),=C'ZZZZ'                                       
SG010    EQU   *                                                                
         CLC   SORTREC+SOFF1(2),=C'SL'     REPLACE OFFICE?                      
         BNE   SG015                                                            
         MVC   SORTREC+SOFF1(2),=C'ZZ'                                          
SG015    EQU   *                                                                
         MVI   SORTREC,C'1'                     SET STATION MAJOR TYPE          
         MVI   SORTREC+STYP2,C'1'               SET SUB-TYPE                    
         MVC   SORTREC+SCURR(4),TMONTBL         INSERT CURRENT BEST $           
         MVC   SORTREC+SPRIOR(4),TMONTBL+4      INSERT PRIOR INV                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   RESETREC,SORTREC                 SAVE FOR RESETTING              
*        MVC   P+5(07),=C'SORTGEN' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
         MVI   SORTREC,C'2'                     SET OFFICE MAJOR TYPE           
         MVC   SORTREC+SGROUP(2),SPACES         NO GRP FOR OFF ORDER            
         MVC   SWAPSTA(5),SORTREC+SSTAT1        SWITCH STATION/OFFICE           
         MVC   SORTREC+SOFF2(2),SORTREC+SOFF1   MOVE OFFICE MAJOR               
         MVC   SORTREC+SSTAT2(5),SWAPSTA        MOVE STATION MINOR              
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTREC,C'3'        SET UP FOR REGIONAL OUTPUT                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*        MVC   P+5(07),=C'SORTGEN'                                              
*        MVC   P+15(11),SORTREC                                                 
*        GOTO1 REPORT                                                           
         MVC   SORTREC,RESETREC           RESET TYPE 1 SORT RECORD              
         XC    TMONTBL,TMONTBL                                                  
         AP    SORTCTR,=P'3'              ADD TO SORTED RECORDS                 
SG099    B     MODEEXIT                                                         
         EJECT                                                                  
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
*        CLI   SORTREC+STYP2,C'1'  **TEST**                                     
*        BNE   GSTEST              **TEST**                                     
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GSTEST   EQU   *                                                                
*                                                                               
* IF THIS RECORD IS A BUDGET, AND PREVIOUS RECORD WAS A BUDGET,                 
*     THE PREVIOUS RECORD MUST BE PRINTED BEFORE THE NEW ONE                    
*     CAN BE PROCESSED.  THE FIELDS ARE SWAPPED SO HEADER CORRECTLY             
*     DISPLAYS VALUES.                                                          
*                                                                               
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
         CLC   SORTREC+SREP(11),SORTREC2+SREP     SAME RECORD GROUP?            
         BE    GS025               YES                                          
         CLI   SSUBTYP2,C'0'       PREVIOUS RECORD BUDGET?                      
         BNE   GS025               NO  -                                        
         MVI   SSUBTYP2,C'9'       INDICATE BUDGET RECORD PRINTED               
         MVC   RESETREC,SORTREC    IN CASE THIS HAPPENS AT A PAGE               
         MVC   SORTREC,SORTREC2    BREAK, VALUES FOR BREAK MUST BE              
*--->    MVC   P+1(11),=C'BUDGET ONLY'                                          
         GOTO1 REPORT                                                           
         MVI   PRTFLAG,C'Y'        TURN ON PRINT FLAG                           
         MVC   SORTREC,RESETREC    RESET FOR PRINTING                           
GS025    EQU   *                                                                
         BR    R8                                                               
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         CLI   QRGPROG,C'9'        BUDGET YEAR IN '90'S?                        
         BE    AH006               YES - IGNORE PRIOR TO 90'S                   
         MVC   HEAD1+50(2),=C'20'  NO  - MOVE IN NEXT CENTURY                   
         B     AH008                                                            
AH006    EQU   *                                                                
         MVC   HEAD1+50(2),=C'19'                                               
AH008    EQU   *                                                                
         MVC   HEAD1+52(2),QRGPROG LOAD BUDGET YEAR                             
         CLI   STYP,C'1'           TYPE 1?                                      
         BNE   AH010               TYPE 2 OR TYPE 3                             
         MVC   HEAD4+11(2),SORTREC+SGROUP                                       
         MVC   HEAD5+15(5),SORTREC+SSTAT1                                       
         MVC   HEAD5+24(20),SORTREC+SMKTNM                                      
         B     MODEEXIT                                                         
AH010    EQU   *                                                                
         MVC   HEAD5+15(2),SORTREC+SOFF2                                        
         MVC   HEAD5+24(20),SORTREC+SOFFNM                                      
         B     MODEEXIT                                                         
         EJECT                                                                  
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETBUD   LA    RF,RBUDREC                                                       
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
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
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
         DS    0F                                                               
CONTOTS  DS    0CL16                                                            
         DC    4F'0'                                                            
TMONTBL  DS    0CL8                                                             
         DC    2F'00'              BUCKET ACCUMULATOR                           
*  12 MONTHS AGGREGATED AS SINGLE BUCKET                                        
*  2 FULLWORDS, ONE WORD PER BUCKET                                             
*        1ST FULLWORD   = CURRENT YEAR BEST $ FIGURE                            
*        2ND FULLWORD   = PRIOR YEAR INVOICED                                   
         SPACE 1                                                                
SORTREC  DS    0CL65                                                            
STYP     DS    CL1                                                              
         DS    CL2                 NEW FIELD FOR SUB REP                        
         DS    CL9                                                              
SSUBTYP  DS    CL1                                                              
SPRIDOL  DS    CL4                 PRIOR YEAR ACTUALS                           
SCURDOL  DS    CL4                 CURRENT YEAR BEST DOLLAR                     
SALLDOL  DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAME DS    CL20                MARKET NAME                                  
SOFFNAME DS    CL20                OFFICE NAME                                  
*                                                                               
*  KEY BUILDING EQUATES:  DISPLACEMENTS INTO SORTREC                            
*                                                                               
SREP     EQU   1                   REP DELINEATION                              
SGROUP   EQU   3                                                                
SSTAT1   EQU   5                   STATION MAJOR                                
SOFF1    EQU   10                  OFFICE MINOR                                 
SOFF2    EQU   5                   OFFICE MAJOR                                 
SSTAT2   EQU   7                   STATION MINOR                                
STYP2    EQU   12                  SUBTYPE FOR BUDGET RECORDS                   
SPRIOR   EQU   13                                                               
SCURR    EQU   17                                                               
SALL$    EQU   21                  ALLOCATED DOLLARS DISPLACEMENT               
SMKTNM   EQU   25                                                               
SOFFNM   EQU   45                                                               
*                                                                               
SORTREC2 DS    0CL65               SAVED SORTKEY                                
SRECTYP2 DS    CL1                                                              
         DS    CL2                 NEW FIELD FOR SUB REP                        
         DS    CL9                                                              
SSUBTYP2 DS    CL1                                                              
SPRIDOL2 DS    CL4                 PRIOR YEAR ACTUALS                           
SCURDOL2 DS    CL4                 CURRENT YEAR BEST DOLLAR                     
SALLDOL2 DS    CL4                 ALLOCATED DOLLARS FROM BUDGET                
SMKTNAM2 DS    CL20                                                             
SOFFNAM2 DS    CL20                                                             
*                                                                               
RESETREC DS    CL65                                                             
ZZKEY    DS    CL10                                                             
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
SAVESTA  DC    CL5'     '          SAVED STATION CALLS                          
SAVEGRUP DC    CL2'  '             SAVED GROUP CODE                             
SAVEALL$ DC    XL4'0'              SAVED ALLOCATION DOLLARS                     
TOTLPRI$ DC    XL4'0'              TOTAL PRIOR DOLLARS                          
TOTLCUR$ DC    XL4'0'              TOTAL CURRENT DOLLARS                        
TOTLALL$ DC    XL4'0'              TOTAL ALLOCATION DOLLARS                     
TOTZZPRI DC    XL4'0'              TOTAL OFFICE ZZ PRIOR DOLLARS                
TOTZZCUR DC    XL4'0'              TOTAL OFFICE ZZ CURRENT DOLLARS              
TOTZZALL DC    XL4'0'              TOTAL OFFICE ZZ ALLOCATION DOLLARS           
TNOZZPRI DC    XL4'0'              TOTAL NOT OFFICE ZZ PRIOR DOLLARS            
TNOZZCUR DC    XL4'0'              TOTAL NOT OFFICE ZZ CURRENT DOLLARS          
TNOZZALL DC    XL4'0'              TOTAL NOT OFFICE ZZ ALLOC DOLLARS            
COMPPRI$ DC    XL4'0'              COMPANY PRIOR DOLLARS                        
COMPCUR$ DC    XL4'0'              COMPANY CURRENT DOLLARS                      
COMPALL$ DC    XL4'0'              COMPANY ALLOCATION DOLLARS                   
ZZPRI$   DC    XL4'0'              OFFICE ZZ PRIOR DOLLARS                      
ZZCUR$   DC    XL4'0'              OFFICE ZZ CURRENT DOLLARS                    
ZZALL$   DC    XL4'0'              OFFICE ZZ ALLOCATION DOLLARS                 
NOZZPRI$ DC    XL4'0'              NOT OFFICE ZZ PRIOR DOLLARS                  
NOZZCUR$ DC    XL4'0'              NOT OFFICE ZZ CURRENT DOLLARS                
NOZZALL$ DC    XL4'0'              NOT OFFICE ZZ ALLOCATION DOLLARS             
SAVEMKT  DS    CL20                SAVE MARKET NAME                             
SAVEOFF  DS    CL20                SAVE OFFICE NAME                             
STAACTIV DS    CL1                 STATION ACTIVE/INACTIVE FLAG                 
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
OFFFLAG  DC    C'N'                                                             
CO$FLAG  DC    C'N'                                                             
INCFLAG  DC    C'N'                INCLUDE ALLOCATION FLAG                      
QCONTYP2 DS    CL1                 ALTERNATE CONTRACT TYPE FLAG                 
TESTCNTR DS    XL1'00'             **TEST**                                     
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
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=65'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
INVBYTE  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023REREP1A02 05/01/02'                                      
         END                                                                    
