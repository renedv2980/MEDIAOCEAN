*          DATA SET REREPA002  AT LEVEL 183 AS OF 05/01/02                      
*PHASE REAO02C,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE REPVALM2                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPAO02 - BUDGET WORKSHEET/PRELIM/FINALS'                     
*********************************************************************           
*                                                                   *           
*        REREPAO02 --- REPPAK BUDGET WORKSHEET/PRELIM/FINALS        *           
*                                                                   *           
*  NOTE:  THIS IS A SKELETON FOR THE DEVELOPMENT OF REPORTS WHICH   *           
*        REQUIRE THAT EACH CONTRACT BE ANALYZED FOR MORE THAN ONE   *           
*        AS AT DATE.  ALL COMPONENTS ARE AVAILABLE TO:              *           
*        1.  DELIVER ONE CONTRACT AT A TIME, WITH AS-AT BUCKET      *           
*            DATA FROM THE REQUEST PERIOD + STANDARD AS-AT DATE     *           
*        2.  LOOP THROUGH A LIST OF SECONDARY AS-AT DATES, DEV-     *           
*            ELOPED FROM:                                           *           
*            A.  OPTION (REQUEST CARD 2, COLUMN 60)                 *           
*            B.  UP TO 3 DIRECTLY ENTERED AS-AT DATES (OPTION=' ')  *           
*            C.  REQUEST PERIOD DATES + OPTION:                     *           
*                1.  WEEKLY:  EACH MONDAY OF REQUEST PERIOD         *           
*                    (OPTION='W')                                   *           
*                2.  MONTHLY: FIRST MONDAY OF EACH MONTH IN REQUEST *           
*                    (OPTION='M')                                   *           
*                                                                   *           
*        3.  HOOKS FOR SORT GENERATION/RETRIEVAL ARE AVAILABLE.     *           
*                                                                   *           
*  THIS MODULE SHOULD BE COPIED TO A NEW BOOKNAME, AND DEVELOPED    *           
*        FOR EACH NEW APPLICATION.                                  *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* OCT20/97 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* FEB06/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
REAO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REAO02,R7,R9,RR=RE                                           
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
         XIT1                                                                   
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
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
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(REQDONE)    END OF REPORT                       
         DC    AL1(RUNLAST),AL3(RUNDONE)    END OF REPORT                       
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
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
***      GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC2,SORTREC2                                                
*                                                                               
         GOTO1 ASATDATS                                                         
*                                  SET AS-AT MONFORC ARRAYS                     
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
*                                                                               
         MVC   P+1(19),=C'PROCESSING CONTRACT'                                  
         GOTO1 HEXOUT,DMCB,RCONKCON,P+25,4,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
*   TEST                                                                        
         L     R3,AMONFORC         DISPLAY ORIGINAL VALU2 ARRAY                 
         BAS   RE,DISPARAY                                                      
*   TEST END                                                                    
*                                                                               
*                                                                               
         LA    R3,MONFARAY         A(FIRST 4 DATES IN MON FORCST)               
*                                     1ST DATE = CURRENT AS-AT DATE             
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
POST0020 EQU   *                                                                
*        L     R4,ANEWMON          A(MONTH TABLE)                               
*        A     R4,=F'4032'         DISPLACE TO 1997                             
*        LA    RF,800              LOAD DISPLAY LENGTH                          
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'2D'                
*                                                                               
         L     R4,ANEWMON          A(MONTH TABLE)                               
*                                                                               
*   FIND REQUEST START DATE IN TABLE.                                           
*                                                                               
POST0040 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0060            FOUND - BEGIN TO PULL FIGURES                
         BH    POST0140            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0040            GO BACK FOR NEXT                             
*                                                                               
POST0060 EQU   *                                                                
         CLI   0(R4),0             ANY TABLE ENTRY?                             
         BE    POST0140            NO  - EXIT                                   
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    POST0140            TABLE > END DATE - EXIT                      
*                                                                               
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         TM    FLAG6(R4),X'01'        ANY INVOICED $ IN BUCKET?                 
         BZ    POST0080               NO  - TAKE ORDERED $                      
         MVC   CONTOTS(4),CUASATIN(R6)    YES = INVOICED THIS YEAR              
         B     POST0100                                                         
*                                                                               
POST0080 EQU   *                                                                
         MVC   CONTOTS(4),TOTORD(R6)      NO  = ORDERED THIS YEAR               
*                                                                               
POST0100 EQU   *                                                                
         MVC   CONTOTS+4(4),PRTOTINV(R6)  INVOICED - LAST YEAR                  
         TM    6(R4),X'02'                TEST ANY PRIOR INVOICES               
         BO    POST0120                                                         
         MVC   CONTOTS+4(4),PRTOTORD(R6)  NO - USE PRI TOT EST                  
*                                                                               
POST0120 EQU   *                                                                
*                                                                               
*                                                                               
*   TEST PRINT OF DATA                                                          
         OC    CONTOTS(8),CONTOTS  ANY VALUE FOR MONTH?                         
         BZ    POST0130            NO  - DON'T DISPLAY                          
         MVC   P+1(04),=C'MOS:'                                                 
         GOTO1 DATCON,DMCB,(0,(R4)),(5,P+06)                                    
         MVC   P+18(04),=C'AOD:'                                                
         GOTO1 DATCON,DMCB,(2,(R3)),(5,P+23)                                    
         EDIT  (4,CONTOTS),(12,P+34)                                            
*                                  DISPLAY CURRENT BEST DOLLAR FIGURE           
         EDIT  (4,CONTOTS+4),(12,P+49)                                          
*                                  DISPLAY PRIOR BEST DOLLAR FIGURE             
         GOTO1 REPORT                                                           
POST0130 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST0060            SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
POST0140 EQU   *                                                                
         L     R4,ANEWMON          A(MONTH TABLE)                               
         L     R6,AMONINFO         A(MONTH INFO TABLE)                          
         LA    R3,16(R3)           BUMP TO NEXT SLOT IN AS-AT TABLE             
         OC    0(8,R3),0(R3)       CHECK FOR EMPTY                              
         BZ    POST0180            NO MORE DATES - FINISHED                     
*                                                                               
*   TEST DISPARAY                                                               
         BAS   RE,DISPARAY         QUICK DISPLAY OF DATES                       
*   TEST DISPARAY END                                                           
*                                                                               
***      MVI   DMCB+16,C'P'        INSERT 'PENNIES OPTION'                      
POST0160 EQU   *                                                                
         GOTO1 =V(VALU2NEW),DMCB,RCONREC,(R4),(R6),(R3)                         
         B     POST0020            GO BACK FOR NEXT SLOT                        
POST0180 EQU   *                                                                
*                                                                               
*   TEST U2NEW                                                                  
*        MVC   P+1(12),=C'MULTI U2NEWS'                                         
*        GOTO1 REPORT                                                           
*        DC    H'0'                                                             
*   TEST U2NEW END                                                              
*                                                                               
         LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
DISPARAY NTR1                                                                   
         MVC   P+1(05),=C'DATES:'                                               
         LA    R0,4                SET LOOP CONTROL                             
         LA    R2,P+10                                                          
DPAR0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,(R3)),(5,(R2))                                    
         LA    R2,12(R2)           BUMP TO NEXT PRINT SLOT                      
         LA    R3,2(R3)            BUMP TO NEXT DATE                            
         BCT   R0,DPAR0020                                                      
         GOTO1 REPORT                                                           
         XIT1                                                                   
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
RD001    EQU   *                                                                
*                                                                               
RD099    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RUNDONE:  DISPLAY MESSAGE                                                   
RUNDONE  NTR1                                                                   
         MVC   P+1(08),=C'RUN DONE'                                             
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
*   REQDONE:  DISPLAY MESSAGE                                                   
REQDONE  NTR1                                                                   
         MVC   P+1(08),=C'REQ DONE'                                             
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DATA RETURN AND REPORT GENERATOR SECTION:  EXPLANATION                      
*                                                                               
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*                                                                               
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*        MVC   P+5(07),=C'SORTGEN' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
         AP    SORTCTR,=P'3'              ADD TO SORTED RECORDS                 
         XIT1                                                                   
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1                                                                   
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GSOR0080            YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0080            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        CLI   SORTREC+STYP2,C'1'  **TEST**                                     
*        BNE   GSOR0080            **TEST**                                     
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GSOR0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         XIT1                                                                   
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
         XIT1                                                                   
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BNZ   DM020                                                            
         XIT1                                                                   
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
         XIT1                                                                   
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
*                                                                               
*   ASATDATS:  ALTERNATE DATES FOR THE VALU2NEW ROUTINE CALL, WHICH             
*        ANALYZES THE CONTRACT RECORD.  EACH ENTRY IN THE TABLE                 
*        CONSISTS OF 2 SETS OF FOUR DATES:  THE FIRST SET IS THE                
*        AS-AT DATES FOR THE PERIOD IN QUESTION.  THE SECOND SET                
*        IS THE CORRESPONDING "AS-AT TO" DATES IF A RANGE OF                    
*        AS-AT DATES IS SPECIFIED.                                              
ASATDATS NTR1                                                                   
*                                                                               
         XCEFL MONFARAY,LMONFARY   CLEAR OUT DATE ARRAY                         
*                                                                               
*                                  CHECK OPTION                                 
*                                                                               
         L     R1,ADCONLST         ADDRESS 2ND CARD OF REQUEST                  
         L     R4,MASTC-ADCONSD(R1)                                             
         LA    R5,MCIO-MASTD(,R4)                                               
         LA    R5,80(R5)           BUMP TO SECOND CARD                          
         USING QREC2D,R5                                                        
         CLI   QASATOPT,C' '       SPECIFIC DATES ENTERED?                      
         BNE   ADAT0020            NO                                           
         BAS   RE,ASATPROC         YES - SET UP AS-AT DATE ARRAY(S)             
                                                                                
         B     ADAT0800            FINISHED                                     
ADAT0020 EQU   *                                                                
         CLI   QASATOPT,C'W'       WEEKLY DATES NEEDED?                         
         BNE   ADAT0040            NO                                           
         BAS   RE,ASATWKLY         YES - SET UP AS-AT DATE ARRAY(S)             
                                                                                
         B     ADAT0800            FINISHED                                     
ADAT0040 EQU   *                                                                
         CLI   QASATOPT,C'M'       MONTHLY DATES NEEDED?                        
         BE    *+6                 YES                                          
         DC    H'0'                UNRECOGNIZED OPTION                          
         BAS   RE,ASATMNTH         YES - SET UP AS-AT DATE ARRAY(S)             
         B     ADAT0800            FINISHED                                     
ADAT0800 EQU   *                                                                
*                                                                               
*   TEST MULTI DATES                                                            
         MVC   P+1(17),=C'END AS-AT ROUTINE'                                    
         GOTO1 REPORT                                                           
*   TEST MULTI DATES END                                                        
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
ASATPROC NTR1                                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         CLC   QASAT,SPACES                                                     
         BE    *+10                                                             
         MVC   WORK(6),QASAT                                                    
         LA    R6,MONFARAY         A(AS-AT MONTH DATE TABLE)                    
*                                                                               
*   SET 'CURRENT AS-AT DATE' AND THREE DATES BASED ON IT                        
*                                                                               
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
*                                                                               
         LA    R6,16(R6)           BUMP TO NEXT SLOT                            
*                                                                               
         L     R1,ADCONLST         ADDRESS 2ND CARD OF REQUEST                  
         L     R4,MASTC-ADCONSD(R1)                                             
         LA    R5,MCIO-MASTD(,R4)                                               
         LA    R5,80(R5)           BUMP TO SECOND CARD                          
         USING QREC2D,R5                                                        
         CLC   QASAT1,SPACES       ANOTHER DATE?                                
         BE    APRO0400            NO  - FINISHED                               
         MVC   WORK(6),QASAT1                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
*   SET '1ST ALTERNATE AS-AT DATE' AND THREE DATES BASED ON IT                  
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
*                                                                               
         LA    R6,16(R6)           BUMP TO NEXT SLOT                            
*                                                                               
         L     R1,ADCONLST         ADDRESS 2ND CARD OF REQUEST                  
         L     R4,MASTC-ADCONSD(R1)                                             
         LA    R5,MCIO-MASTD(,R4)                                               
         LA    R5,80(R5)           BUMP TO SECOND CARD                          
         USING QREC2D,R5                                                        
         CLC   QASAT2,SPACES       ANOTHER DATE?                                
         BE    APRO0400            NO  - FINISHED                               
         MVC   WORK(6),QASAT2                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
*   SET '2ND ALTERNATE AS-AT DATE' AND THREE DATES BASED ON IT                  
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
*                                                                               
         LA    R6,16(R6)           BUMP TO NEXT SLOT                            
*                                                                               
         L     R1,ADCONLST         ADDRESS 2ND CARD OF REQUEST                  
         L     R4,MASTC-ADCONSD(R1)                                             
         LA    R5,MCIO-MASTD(,R4)                                               
         LA    R5,80(R5)           BUMP TO SECOND CARD                          
         USING QREC2D,R5                                                        
         CLC   QASAT3,SPACES       ANOTHER DATE?                                
         BE    APRO0400            NO  - FINISHED                               
         MVC   WORK(6),QASAT3                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
*   SET '3RD ALTERNATE AS-AT DATE' AND THREE DATES BASED ON IT                  
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
*                                                                               
*                                                                               
APRO0400 EQU   *                                                                
         CLC   =C'DUMPMONARAY',QUESTOR                                          
         BNE   APRO0500            NO ARRAY DISPLAY                             
         BAS   RE,DUMPARAY                                                      
APRO0500 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>>                                                                        
*                                                                               
*  THIS ROUTINE SETS THE SET OF FOUR DATES FOR AS-AT PROCESSING.                
*                                                                               
SET4DATS NTR1                                                                   
         L     R6,0(R1)            RESET A(TABLE ENTRIES)                       
*                                                                               
         GOTO1 GETMDAY,DMCB,WORK,(R6)    CURRENT AS-AT DATE                     
         LA    R6,2(R6)            A(PRIOR AS-AT DATE)                          
*                                                                               
* GET BROADCAST YEAR OF AS-AT DATE:  MAY NOT BE SAME - IE:                      
*    DEC30/91 IS BROADCAST YEAR 92.  (CHECK THE CALENDAR).                      
*    TAKE THE YEAR FROM THE TO-DATE PORTION, FOR SAME REASON.                   
*                                                                               
         GOTO1 GETBROAD,DMCB,WORK,WORK+12                                       
         MVC   WORK+6(2),WORK+18   CALCULATE START BDCST YEAR                   
         MVC   WORK+8(4),=C'0115'  SET JAN15 OF YEAR                            
         GOTO1 GETBROAD,DMCB,WORK+6,WORK+12                                     
*                                                                               
*  CALCULATE WEEK # FROM BDCAST YEAR START THRU CURR AS AT DATE                 
*                                                                               
         GOTO1 =V(PERVERT),DMCB,WORK+12,WORK                                    
         ZICM  RF,DMCB+12,2        SAVE WEEK# OF CURR AS AT DATE                
         OC    DMCB+10(2),DMCB+10  ANY PERVERT DAYS REMAINING?                  
         BNZ   S4DA0004            YES - DON'T ADJUST WEEK #                    
         BCTR  RF,0                NO  - DECREMENT WEEK #                       
S4DA0004 EQU   *                                                                
         STCM  RF,3,WORK+32        SAVE WEEK# OF CURR AS AT DATE                
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,DOUBLE)                                
         ZIC   RF,DOUBLE           EXTRACT YEAR FROM DATE                       
         BCTR  RF,0                SUBTRACT 1 FROM YEAR                         
         STC   RF,DOUBLE           PUT YEAR BACK                                
         GOTO1 DATCON,DMCB,(3,DOUBLE),(0,WORK+6)                                
         GOTO1 SETASAT,DMCB,(R6)                                                
         LA    R6,2(R6)            A(2YR PRIOR AS AT DATE)                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,DOUBLE)                                
         ZIC   RF,DOUBLE           EXTRACT YEAR FROM DATE                       
         BCTR  RF,0                SUBTRACT 1 FROM YEAR                         
         STC   RF,DOUBLE           PUT YEAR BACK                                
         GOTO1 DATCON,DMCB,(3,DOUBLE),(0,WORK+6)                                
         GOTO1 SETASAT,DMCB,(R6)                                                
         LA    R6,2(R6)            A(NEXT YEAR AS AT DATE)                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,DOUBLE)                                
         ZIC   RF,DOUBLE           EXTRACT YEAR FROM DATE                       
         LA    RF,3(RF)            ADD 3:  CALCULATE NEXT YEAR                  
         STC   RF,DOUBLE           PUT YEAR BACK                                
         GOTO1 DATCON,DMCB,(3,DOUBLE),(0,WORK+6)                                
         GOTO1 SETASAT,DMCB,(R6)                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  THIS ROUTINE CALCULATES THE MONDAY DATE OF THE NTH WEEK OF THE               
*     BROADCAST YEAR                                                            
*                                                                               
SETASAT  NTR1                                                                   
         L     R6,0(R1)            RESET A(MONFORC TABLE ENTRY)                 
         GOTO1 GETBROAD,DMCB,WORK+6,WORK+12                                     
         ZICM  R2,WORK+32,2        INSERT WEEK# OF CURRENT DATE                 
         CH    R2,=H'52'           IS CURRENT WEEK # 53?                        
*                                                                               
*   WEEK # IS UNDERSTATED BY 1.  DAYS 1-7 ARE WEEK 1, BUT PERVERT               
*    CALCULATION RETURNS 0.                                                     
*                                                                               
         BNE   SASA0004            NO                                           
         BCTR  R2,0                YES - SET IT TO 52 FOR OTHER YEARS           
SASA0004 EQU   *                                                                
         MH    R2,=H'7'            MULT BY 7 = # OF DAYS                        
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R2)                                  
         PRINT NOGEN                                                            
         GOTO1 GETMDAY,DMCB,WORK+18,(R6)                                        
*                                                                               
*   FINAL CALL INSERTS MONDAY DATE OF WEEK# INTO MONFOR TABLE                   
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**       ROUTINE RETURNS MONDAY DATE FOR WEEK GIVEN                             
*         INPUT = YYMMDD                                                        
*         OUTPUT= 2-BYTE BINARY                                                 
         SPACE 3                                                                
GETMDAY  NTR1                                                                   
         LM    R5,R6,0(R1)         R5=A(INPUT), R6=A(OUTPUT)                    
         CLC   0(6,R5),=C'940229' ****2YR PRIOR OF LEAP YR 96 ****              
         BE    GETM10                                                           
         CLC   0(6,R5),=C'950229' ****PRIOR YEAR FOR LEAP YR 96 ****            
         BE    GETM10                                                           
         CLC   0(6,R5),=C'970229' ****NEXT YEAR FOR LEAP YR 96 ****             
         BE    GETM10                                                           
         CLC   0(6,R5),=C'980229' ****2YR PRIOR OF LEAP YR 00 ****              
         BE    GETM10                                                           
         CLC   0(6,R5),=C'990229' ****PRIOR YEAR FOR LEAP YR 00 ****            
         BE    GETM10                                                           
         CLC   0(6,R5),=C'010229' ****NEXT YEAR FOR LEAP YR 00 ****             
         BE    GETM10                                                           
         CLC   0(6,R5),=C'020229' ****2YR PRIOR OF LEAP YR 04 ****              
         BE    GETM10                                                           
         CLC   0(6,R5),=C'030229' ****PRIOR YEAR FOR LEAP YR 04 ****            
         BE    GETM10                                                           
         CLC   0(6,R5),=C'050229' ****NEXT YEAR FOR LEAP YR 04 ****             
         BNE   GETM12                                                           
GETM10   EQU   *                                                                
         MVC   3(3,R5),=C'301'     ***PREVIOUS YEAR MUST BE MARCH 01***         
GETM12   EQU   *                                                                
         GOTO1 GETDAY,DMCB,(R5),FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB             DAY NUMBER                                   
         BCTR  RE,R0                                                            
         LNR   RE,RE                                                            
         ST    RE,DMCB+8           PUT IN NUMBER OF DAYS TO SUBTRACT            
*                                                                               
         GOTO1 ADDAY,DMCB,(R5),DUB                                              
*                                                                               
         GOTO1 DATCON,(R1),DUB,(2,(R6))                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ASATWKLY:  FROM QSTART TO QEND, AS-AT ARRAY IS SET TO EACH                  
*        MONDAY DATE.                                                           
ASATWKLY NTR1                                                                   
         XC    WORK,WORK           CLEAR WORK AREA                              
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'15'    INSERT MID-MONTH                             
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,ARAYSTRT)                              
*                                  BCST START DATE TO COMP                      
         MVC   WORK+48(6),WORK+6   SAVE EBCDIC BCST START DATE                  
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'15'    INSERT MID-MONTH                             
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,ARAYEND)                              
*                                  BCST END DATE TO COMP                        
         MVC   WORK+54(6),WORK+6   SAVE EBCDIC BCST END   DATE                  
         MVC   WORK(6),WORK+48     SET REQUEST BCST START DATE                  
         LA    R6,MONFARAY         A(AS-AT MONTH DATE TABLE)                    
*                                                                               
*   SET  AS-AT DATE AND THREE DATES BASED ON IT                                 
*                                                                               
ASWK0040 EQU   *                                                                
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
         LA    R6,16(R6)           BUMP TO NEXT ENTRY                           
         LA    R2,7                SET WEEK INCREMENT                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         MVC   WORK(6),WORK+6      LOAD FOR NEXT LOOP                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)                               
*                                  CONVERT TO COMPRESSED FORMAT                 
         CLC   ARAYEND,WORK+12                                                  
         BNL   ASWK0040            NOT AT END:  ADD TO ARRAY                    
*                                                                               
         CLC   =C'DUMPMONARAY',QUESTOR REQUEST TO DISPLAY TABLE?                
         BNE   ASWK0500            NO                                           
         BAS   RE,DUMPARAY         YES                                          
ASWK0500 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ASATMNTH:  FROM QSTART TO QEND, AS-AT ARRAY IS SET TO EACH                  
*        BROADCAST MONTH START DATE                                             
ASATMNTH NTR1                                                                   
         XC    WORK,WORK           CLEAR WORK AREA                              
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'15'    INSERT MID-MONTH                             
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,ARAYSTRT)                              
*                                  BCST START DATE TO COMP                      
         MVC   SAVEWORK,WORK+12    SAVE BCST START MONTH END DATE               
*                                                                               
         MVC   WORK+48(6),WORK+6   SAVE EBCDIC BCST START DATE                  
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'15'    INSERT MID-MONTH                             
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,ARAYEND)                               
*                                  BCST END DATE TO COMP                        
         MVC   WORK+54(6),WORK+6   SAVE EBCDIC BCST END   DATE                  
         MVC   WORK(6),WORK+48     SET REQUEST BCST START DATE                  
         LA    R6,MONFARAY         A(AS-AT MONTH DATE TABLE)                    
*                                                                               
*   SET  AS-AT DATE AND THREE DATES BASED ON IT                                 
*                                                                               
ASMN0040 EQU   *                                                                
*                                                                               
         GOTO1 SET4DATS,DMCB,(R6)                                               
         LA    R6,16(R6)           BUMP TO NEXT ENTRY                           
         GOTO1 DATCON,DMCB,(0,SAVEWORK),(3,WORK+24)                             
*                                  CONVERT DATE TO BINARY - USE BCST            
*                                     MONTH END DATE, NOT START DATE            
         CLI   WORK+25,12          IS MONTH DECEMBER (12TH)                     
         BNE   ASMN0060            NO                                           
         MVI   WORK+25,1           YES - SET TO JANUARY (01ST)                  
         ZIC   RF,WORK+24          BUMP YEAR                                    
         LA    RF,1(RF)                                                         
         STC   RF,WORK+24                                                       
         B     ASMN0080                                                         
ASMN0060 EQU   *                                                                
         ZIC   RF,WORK+25          STRIP MONTH                                  
         LA    RF,1(RF)            BUMP TO NEXT MONTH                           
         STC   RF,WORK+25          PUT IT BACK                                  
ASMN0080 EQU   *                                                                
         MVI   WORK+26,15          SET DAY TO MID-MONTH                         
         GOTO1 DATCON,DMCB,(3,WORK+24),(0,WORK)                                 
*                                  CONVERT NEXT MONTH TO EBCDIC                 
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         MVC   WORK(6),WORK+6      LOAD FOR NEXT LOOP                           
         MVC   SAVEWORK,WORK+12    SAVE BCST MONTH END DATE                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,WORK+12)                                 
*                                  CONVERT TO COMPRESSED FORMAT                 
         CLC   ARAYEND,WORK+12                                                  
         BNL   ASMN0040            NOT AT END:  ADD TO ARRAY                    
*                                                                               
         CLC   =C'DUMPMONARAY',QUESTOR REQUEST TO DISPLAY TABLE?                
         BNE   ASMN0500            NO                                           
         BAS   RE,DUMPARAY         YES                                          
ASMN0500 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DUMPARAY:  DISPLAY THE DATE SETUP IN THE ARRAY                              
*                                                                               
DUMPARAY NTR1                                                                   
         LA    R6,MONFARAY         SET A(ARRAY)                                 
DARA0420 EQU   *                                                                
         LA    R5,P                SET A(PRINT LINE)                            
         OC    0(8,R6),0(R6)       ANYTHING IN DATE SLOT?                       
         BZ    DARA0500            NO  - FINISHED                               
         LA    R0,4                SET LOOP CONTROL                             
DARA0440 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,(R6)),(5,(R5))                                    
         LA    R6,2(R6)            BUMP TO NEXT DATE IN SLOT                    
         LA    R5,12(R5)           BUMP TO NEXT DATE ON PRINT LINE              
         BCT   R0,DARA0440         GO BACK FOR NEXT                             
         GOTO1 REPORT              PRINT LINE OF 4 DATES                        
*                                  4 DATES PRINTED - CHECK NEXT SLOT            
         LA    R6,8(R6)            BUMP TO NEXT SLOT                            
         B     DARA0420            GO BACK FOR NEXT SLOT                        
DARA0500 EQU   *                                                                
*                                                                               
*   TEST                                                                        
****<<<  DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
****>>>>                                                                        
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
         EJECT                                                                  
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
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
SAVESTA  DC    CL5'     '          SAVED STATION CALLS                          
SAVEGRUP DC    CL2'  '             SAVED GROUP CODE                             
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
*                                                                               
SORTCTR  DC    PL4'0'              RECORDS RELEASED TO SORT                     
PROCCTR  DC    PL4'0'              RECORDS PROCESSED.                           
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
ACTIVE   DS    CL1                                                              
ARAYSTRT DS    CL2                 COMPRESSED ARRAY START DATE                  
ARAYEND  DS    CL2                 COMPRESSED ARRAY END DATE                    
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=65'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEWORK DS    CL6                                                              
MONFARAY DS    54CL16              54 SETS OF EIGHT DATES                       
*                                  MAX ONE YEAR, WEEKLY                         
LMONFARY EQU   *-MONFARAY                                                       
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
QREC2D   DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         DS    0F                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'183REREPA002 05/01/02'                                      
         END                                                                    
