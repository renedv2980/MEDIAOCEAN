*          DATA SET REREP3002S AT LEVEL 009 AS OF 08/16/02                      
*PHASE RE3002A,*                                                                
*INCLUDE REGENBUC                                                               
         TITLE 'RE3002 - REREP3002 - RADAR REPORT'                              
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP3002 --- REPPAK DAYPART ANALYSIS REPORT               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 16MAY91 (EFJ) --- INITIAL RELEASE                                 *           
*                                                                   *           
* 06AUG91 (EFJ) --- FIX BUG CAUSED DUMPING - R8 NOT ALWAYS SAVED    *           
*                                                                   *           
* 23SEP91 (EFJ) --- FIX BUG CAUSED DUMPING - R8 NOT ALWAYS RESTORED *           
*                                                                   *           
* SEP25/91 (MRR) --- DON'T DO BOXES WHEN YOU DON'T KNOW THE NUMBER  *           
*                     OF DAYPART COLUMNS                            *           
*                                                                   *           
* 30SEP91 (EFJ) --- PUT (000) ON TOTAL LINES PER KARI               *           
*                                                                   *           
* 07NOV91 (EFJ) --- FIX BUG IF NO BUY END TIME, OR START TIME       *           
*                   OF NONE OR VARY.                                *           
*               --- FIX PAGING PROBLEM                              *           
*                                                                   *           
* 11NOV91 (BU ) --- INSTALL NEW ADDRESSING FOR SPACEND, AND USE NEW *           
*                   SPACEND FORMATTING                              *           
*                                                                   *           
* JAN23/92 (MRR) --- >IF OPT1=T, ONLY PRINT TOTALS BOX              *           
*                    >IF OPT2=A, PRINT BOTH AND COST PER SPOT       *           
*                                                                   *           
* 29MAR93 (SKU) --- REP PROFILE SET IF START OF B'CAST DAY AT 6AM   *           
*                                                                   *           
* 15FEB94 (BU ) --- CORRECT EXCEPTIONALLY LONG PROCESSING PROBLEM   *           
*                                                                   *           
* 23SEP94 (SKU) --- ADD NEW OPTION TO PRINT SPOT LENGTH DETAILS     *           
*                                                                   *           
* 27JAN95 (SKU) --- ACCEPT MORE THAN 2 CARDS                        *           
*                                                                   *           
* 24MAR95 (SKU) --- COMBINE '+' DAY VALUES FOR THE SAME DAYPART     *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* JAN13/99 (RHV) --- HANDLE NON-DELETED CANCELLED BUYS              *           
*                                                                   *           
* SEP15/99 (BU ) --- ENHANCE 'BAD CATEGORY' ERROR MESSAGES          *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                   ***  END TOMBSTONE  ***                         *           
*********************************************************************           
*                                                                               
RE3002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3002,R9,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
* CHECK AND PROCESS MODE SETTINGS                                               
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
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(24),=C'>>> PROCESSING ERROR <<<'                               
         GOTO1 (RF),(R1),ERRORLIN                                               
         B     MAINBAD                                                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
* MAIN COMMON EXIT                                                              
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
* MODE/PROCESS ROUTINE TABLE                                                    
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
         DC    AL1(PROCCONT),AL3(PCON)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
* INITIAL --- PROCESS TO START THE REPORT                                       
*                                                                               
*        - READ FOR OFFICES AND BUILD OFFICE TABLES                             
*        - SET DATES FOR BUCKETING THE CONTRACT DOLLARS                         
*        - SET INITIAL REPORT VALUES                                            
*                                                                               
INITIAL  NTR1                                                                   
*        TIME  DEC                                                              
*                                  RETRIEVE TIME: RETURNED IN R0                
*                                     AS HH:MM:SS:TT                            
*        STCM  R0,15,P+50          INSERT TIME RETURNED                         
*        GOTO1 HEXOUT,DMCB,P+50,P+20,4,=C'TOG'                                  
*        MVC   P+1(14),=C'ENTER INITIAL:'                                       
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
**                                                                              
         LA    R0,HHOOK                                                         
         ST    R0,HEADHOOK                                                      
*                                                                               
         L     R0,=V(ACCUMC)                                                    
         A     R0,RELO                                                          
         ST    R0,ACCUMS           USE IT FOR ACCUMC                            
*                                                                               
         L     R0,=V(IOBUFF)                                                    
         A     R0,RELO                                                          
         ST    R0,ARADAR           USE IT FOR RADAR RECORD                      
*                                                                               
* GET SECOND REQUEST CARD                                                       
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         MVC   P+36(12),=C'MISSING CARD'                                        
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED AT LEAST TWO CARDS                      
         BL    INITBAD                                                          
*                                                                               
         L     R4,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R4)                                                     
         DROP  R5                                                               
*                                                                               
         MVI   RCSUBPRG,0          QOPTION1 DEFAULT IS 'D'                      
         CLI   QOPTION1,C'D'       SUMMARY?                                     
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ACCUMS                                                        
         GOTO1 ROLLER,DMCB,0,(R2),ACNUMLIN,ACNUMCOL                             
                                                                                
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   INIT0005                                                         
         GOTO1 ROLLER,DMCB,0,ACCUMSL,ACNMSLLN,ACNMSLCL                          
*                                                                               
* SET UP SORT PARAMETERS AT RUN TIME                                            
INIT0005 DS    0H                                                               
         LA    R1,1                                                             
*                                                                               
* IF OPTION 4=Y, SORT ON CATG (START SORT IN COL 1, NOT 3)                      
         CLI   Q2OPT4,C'Y'                                                      
         BE    *+8                                                              
         LA    R1,3                                                             
*                                                                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(1),DUB+7(1)                                          
*                                                                               
         LA    R1,SKEYLEN          SET L'SORT KEY IN SORTCARD                   
*                                                                               
* IF OPTION 4=Y, SORT ON CATG (DON'T REDUCE R1 BY 2)                            
         CLI   Q2OPT4,C'Y'                                                      
         BE    *+8                                                              
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
*                                                                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRECLEN          SET L'SORT RECORD IN RECCARD                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                         
         EJECT                                                                  
*                                                                               
* GET REP RECORD - CHECK IF PROFILE SET FOR 6A-559A B'CAST DAY INSTEAD          
*                  OF 5A-459A                                                   
*                                                                               
         MVC   P+36(11),=C'BAD REP REC'                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REP REC                                      
         MVC   KEY+25(2),QREP                                                   
         GOTO1 READ                                                             
         BNZ   INITBAD             REP REC NOT FOUND                            
         LA    RF,RREPREC                                                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         BNZ   INITBAD             BAD READ                                     
*                                                                               
         XC    REPPROF,REPPROF     INIT                                         
         MVC   STRTTIME,=H'0500'                                                
         CLI   RREPPROF+4,C'Y'     USE 6A-559A?                                 
         BNE   INIT0010                                                         
         OI    REPPROF,X'80'       YES, SET FLAG                                
         MVC   STRTTIME,=H'0600'                                                
         EJECT                                                                  
*                                                                               
* GET RADAR RECORD                                                              
INIT0010 DS    0H                                                               
         MVC   P+36(13),=C'BAD RADAR REC'                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'33'           RADAR REC                                    
         MVC   KEY+17(2),QREP                                                   
         CLC   QR30RDA,SPACES                                                   
         BNE   *+14                                                             
         MVC   KEY+19(8),=C'STANDARD' DEFAULT                                   
         B     *+10                                                             
         MVC   KEY+19(8),QR30RDA                                                
         GOTO1 READ                                                             
         BNZ   INITBAD             RADAR REC NOT FOUND                          
         MVC   AIOAREA,ARADAR                                                   
         GOTO1 GREC                                                             
         BNZ   INITBAD             BAD READ                                     
         L     R6,ARADAR                                                        
         USING RRDAREC,R6                                                       
         MVC   NUMDPTS,RRDANMCT                                                 
         DROP  R6                                                               
*                                                                               
* FILL IN ROUTINE BLOCK FOR REGENBUC                                            
         MVC   RBLOCK+00(4),GETBROAD                                            
         MVC   RBLOCK+04(4),GETDAY                                              
         MVC   RBLOCK+08(4),ADDAY                                               
         MVC   RBLOCK+12(4),DATCON                                              
*                                                                               
* GET REQ DATES IN BINARY                                                       
         GOTO1 DATCON,DMCB,(X'00',QSTART),(3,BSTART)                            
         GOTO1 (RF),(R1),(X'00',QEND),(3,BEND)                                  
*                                                                               
* CALCULATE START AND END ACTUAL DATES FROM REQUEST CARD                        
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'15'    DEFINATELY W/IN BDCAST MON                   
         GOTO1 GETBROAD,DMCB,WORK,REQSTBC,GETDAY,ADDAY                          
         MVC   WORK(4),QEND                                                     
         GOTO1 (RF),(R1),WORK,REQEDBC,GETDAY,ADDAY                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,REQSTBC),(3,FLTSTART)                             
         GOTO1 DATCON,DMCB,(0,REQEDBC+6),(3,FLTEND)                             
*                                                                               
* GET EXPANDED TARGET AGENCY NAME FROM AGY REC                                  
         MVC   P+36(14),=C'BAD AGENCY REC'                                      
         LA    R1,RAGYREC                                                       
         ST    R1,AIOAREA                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),QR30AGY                                                
         MVC   KEY+23(2),SPACES                                                 
         MVC   KEY+25(2),QREP                                                   
         GOTO1 HIGH                                                             
         BNZ   INITBAD                                                          
         GOTO1 GREC                                                             
         MVC   TARGAGY,RAGYNAM2                                                 
*                                                                               
INITGOOD EQU   *                                                                
*        TIME  DEC                                                              
*                                  RETRIEVE TIME: RETURNED IN R0                
*                                     AS HH:MM:SS:TT                            
*        STCM  R0,15,P+50          INSERT TIME RETURNED                         
*        GOTO1 HEXOUT,DMCB,P+50,P+20,4,=C'TOG'                                  
*        MVC   P+1(14),=C'START PCON:   '                                       
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
**                                                                              
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
INITBAD  EQU   *                                                                
         MVC   P(35),=C'>>> ERROR IN INITIALIZING PROGRAM -'                    
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
* PCON --- PROCESS A CONTRACT                                                   
PCON     NTR1                                                                   
*                                                                               
* CHECK 1ST TIME SWITCH TO SET ADDRESS                                          
*                                                                               
         CLI   CATFOUND,C'Y'       CATEGORY ADDR SET?                           
         BE    PCON0050            YES                                          
         MVI   CATFOUND,C'Y'       NO  - SET IT                                 
*                                                                               
* GET ADDR OF 1ST CATEGORY ENTRY IN SPACEND TABLE - SET IT                      
*     THE FIRST TIME THROUGH TO ISOLATE THE TIME SPENT                          
*     SEARCHING SPACEND.                                                        
*                                                                               
         L     R1,ASPACND4         A(1ST ENTRY IN SPACEND TABLE)                
PCON0020 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                NO  - MUST BE SOME ENTRIES                   
         CLI   0(R1),X'0F'         CATEGORY ENTRY?                              
         BE    PCON0040            YES                                          
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ENTRY                     
         AR    R1,R0                                                            
         B     PCON0020            GO BACK FOR NEXT                             
PCON0040 EQU   *                                                                
         ST    R1,ACATSPAC         SAVE A(1ST CATEGORY ENTRY)                   
PCON0050 EQU   *                                                                
         CLC   RCONDATE(3),FLTEND  FLIGHT AFTER REQUEST DATES?                  
         BH    PCONGOOD            YES - IGNORE CONTRACT                        
         CLC   FLTSTART,RCONDATE+3 FLIGHT BEFORE REQUEST DATES?                 
         BH    PCONGOOD            YES - IGNORE CONTRACT                        
         L     RF,CONCOUNT         INCREMENT CONTRACT COUNTER                   
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
*                                                                               
         CLC   RCONKAGY,QR30AGY    THIS K FROM TARG AGY?                        
         BE    PCONGOOD            YES, IGNORE IT...                            
*                                                                               
         LA    R3,SORTREC                                                       
         USING SORTD,R3                                                         
*                                                                               
* BUILD SORT KEY                                                                
         MVC   SCAT,RCONCTGY                                                    
         MVC   SADVNM,RADVNAME                                                  
         MVC   SPRDNM,RPRDNAME                                                  
         MVC   SAGYNM,RAGYNAM1                                                  
         MVC   SSTANM,RCONKSTA                                                  
         MVC   SCONNUM,RCONKCON                                                 
         MVC   SRDATES,RCONDATE                                                 
*                                                                               
* BUILD SORT RECORD                                                             
         MVC   SRADV,RCONKADV                                                   
         MVC   SRPRD,RCONPRD                                                    
         MVC   SRAGY,RCONKAGY                                                   
*                                                                               
* GET TIME ZONE FROM STATION RECORD                                             
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        EXTRA DESC ELEM                              
         BAS   RE,GETEL                                                         
         USING RSTAXXEL,R6                                                      
         BE    *+12                                                             
         MVI   SRSTATZ,0                                                        
         B     *+10                                                             
         MVC   SRSTATZ,RSTATZ                                                   
         DROP  R6                                                               
*                                                                               
* GO GET CATEGORY REC FROM SPACEND                                              
         L     R4,ACATSPAC         A(1ST CATEGORY ENTRY IN TABLE)               
         SR    R0,R0                                                            
         CLC   RCONCTGY,SPACES     ANY CATEGORY CODE IN CONTRACT                
         BNH   PCON0080            NO  - LOAD DEFAULT                           
*                                                                               
PCON0060 CLI  0(R4),0                                                           
         BE    BADCTG                                                           
         CLI   0(R4),X'0F'         CATG REC                                     
         BNE   *+14                                                             
         CLC   2(2,R4),RCONCTGY                                                 
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PCON0060                                                         
         MVC   SRCATNM,6(R4)                                                    
         B     PCON0100                                                         
PCON0080 EQU   *                                                                
         MVC   SRCATNM(24),=C'NO CATEGORY CODE ENTERED'                         
PCON0100 EQU   *                                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
* RELEASE RECORD TO SORTER                                                      
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         L     RF,SRTCTOUT         INCREMENT SORT COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,SRTCTOUT                                                      
*                                                                               
* PCON EXIT                                                                     
PCONGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
BADCTG   EQU   *                                                                
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(40),=C'>>> ERROR - CATEGORY NOT IN SPACEND? <<<'               
         GOTO1 (RF),(R1),ERRORLIN                                               
         MVC   P+12(09),=C'CONTRACT='                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+32,4,=C'TOG'                              
         GOTO1 LOCALREP,DMCB,ERRORLIN                                           
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* RPTDONE: REPORT DONE - GET BUYS FROM SORTER, CK FOR DAYPART FIT     *         
*          AND BUILD PRINT LINES...                                   *         
*                                                                     *         
* REG USAGE:                                                          *         
* ----------                                                          *         
* R5 --- DAYPART DEFN ELEM (X'02' FROM RADAR RECORD)                  *         
* R6 --- GETEL                                                        *         
* R7 --- SORT RECORD                                                  *         
*                                                                     *         
***********************************************************************         
RPTDONE  NTR1                                                                   
*        TIME  DEC                                                              
*                                  RETRIEVE TIME: RETURNED IN R0                
*                                     AS HH:MM:SS:TT                            
*        STCM  R0,15,P+50          INSERT TIME RETURNED                         
*        GOTO1 HEXOUT,DMCB,P+50,P+20,4,=C'TOG'                                  
*        MVC   P+1(14),=C'ENDED PCON:   '                                       
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
*        MVC   P+1(14),=C'CONTRACTS =   '                                       
*        L     RF,CONCOUNT                                                      
*        EDIT  (RF),(10,P+20),ZERO=NOBLANK                                      
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
*        MVC   P+1(14),=C'SORTRECS OUT= '                                       
*        L     RF,SRTCTOUT                                                      
*        EDIT  (RF),(10,P+20),ZERO=NOBLANK                                      
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
**                                                                              
         MVI   BREAKF,0                                                         
         XC    LADV,LADV           CLEAR BREAK CONTROLS                         
         XC    LPRDNM,LPRDNM                                                    
         XC    LAGY,LAGY                                                        
         XC    LCAT,LCAT                                                        
         MVI   WEEKS,0             RESET #WEEKS                                 
         XC    PERTAB,PERTAB       ZERO WEEK TABLE                              
         XC    PERTABSL(212),PERTABSL ZERO WEEK TABLE FOR SPOT LENGTHS          
         MVC   SPTLENS,SPACES                                                   
         MVC   SPLNWKS,SPACES                                                   
RD10A    DS    0H                                                               
         MVC   SORTREC,0(R7)       SAVE OFF LAST REC (NOT 1ST TIME)             
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R7,15,4(R1)                                                      
         BNZ   *+12                                                             
         OI    BREAKF,EOSORT                                                    
         B     NEXTK                                                            
*                                                                               
         ST    R7,SAVER7                                                        
         USING SORTD,R7                                                         
*                                                                               
         OC    LADV,LADV                                                        
         BNZ   RD10C                                                            
         MVC   LADV,SRADV          INITIALIZE BREAKS                            
         MVC   LPRDNM,SPRDNM                                                    
         MVC   LAGY,SRAGY                                                       
         MVC   LCAT,SCAT                                                        
*                                                                               
* CHECK FOR BREAKS                                                              
RD10C    DS    0H                                                               
         CLI   Q2OPT4,C'Y'         IF SORTED ON CAT CD, GIVE CAT BREAKS         
         BNE   RD10D                                                            
         CLC   LCAT,SCAT                                                        
         BE    RD10D                                                            
         OI    BREAKF,EOCAT                                                     
         B     NEXTK                                                            
*                                                                               
RD10D    CLI   QOPTION1,C'D'                                                    
         BE    RD10B                                                            
*                                                                               
         CLC   LADV,SRADV                                                       
         BE    *+12                                                             
         OI    BREAKF,EOADV                                                     
         B     NEXTK               BREAK                                        
*                                                                               
         CLC   LAGY,SRAGY                                                       
         BE    *+12                                                             
         OI    BREAKF,EOAGY                                                     
         B     NEXTK               BREAK                                        
*                                                                               
         CLC   LPRDNM,SPRDNM                                                    
         BE    *+12                                                             
         OI    BREAKF,EOPRD                                                     
         B     NEXTK               BREAK                                        
*                                                                               
RD10B    DS    0H                                                               
         GOTO1 =A(DOBUYS),DMCB,(RC),RR=RELO                                     
         BNZ   BUYERR                                                           
*                                                                               
* PRINT DETAILS AND ROLL TOTALS TO NEXT LEVEL                                   
*                                                                               
NEXTK    DS    0H                                                               
         TM    BREAKF,EOSORT                                                    
         BNZ   NEXTK10             ALWAYS ON END OF SORT                        
*                                                                               
         TM    BREAKF,EOCAT        CATG BREAK? (ALWAYS ALLOWED)                 
         BZ    *+12                                                             
         LA    R7,SORTREC                                                       
         B     NEXTK10                                                          
*                                                                               
         CLI   QOPTION1,C'D'       DETAIL OPTION?                               
         BE    NEXTK10             PRINT DETAILS                                
*                                                                               
* IF NOT DETAIL OPTION, CHECK FOR ADV OR AGY OR PRD BREAK                       
         TM    BREAKF,EOADV+EOAGY+EOPRD                                         
         BZ    RD10A                                                            
         LA    R7,SORTREC          USE LAST REC'S DATA                          
*                                                                               
NEXTK10  DS    0H                                                               
         TM    BREAKF,EOSORT                                                    
         BZ    *+16                                                             
         LA    R7,SORTREC                                                       
         CLI   QOPTION1,C'D'                                                    
         BE    RD70                ALL DETAIL LINES PRINTED                     
*                                                                               
         BAS   RE,PRTDAT                                                        
         BZ    NEXTK20                                                          
*                                                                               
         TM    BREAKF,EOCAT+CDATA                                               
         BO    RD60                IF BREAK & DATA                              
         CLI   QOPTION1,C'D'                                                    
         BNE   NEXTK15                                                          
         TM    BREAKF,EOCAT                                                     
         BZ    RD10A                                                            
         L     R7,SAVER7                                                        
         MVC   LCAT,SCAT                                                        
         NI    BREAKF,X'FF'-(EOCAT+CDATA)                                       
         B     RD10B               ALREADY HAVE NEXT K IF WAS EOCAT             
*                                                                               
NEXTK15  TM    BREAKF,EOSORT                                                    
         BO    RD60                ONLY CONTINUE IF END OF SORT                 
         TM    BREAKF,EOADV+ADATA                                               
         BO    RD60                OR IF (BREAK & DATA)                         
         TM    BREAKF,EOAGY+ADATA                                               
         BO    RD60                OR IF (BREAK & DATA)                         
         L     R7,SAVER7                                                        
         NI    BREAKF,X'FF'-(EOADV+EOPRD+EOAGY+EOCAT)                           
         MVC   LPRDNM,SPRDNM                                                    
         MVC   LADV,SRADV                                                       
         MVC   LAGY,SRAGY                                                       
         MVC   LCAT,SCAT                                                        
         B     RD10B               ALREADY HAVE NEXT K                          
*                                                                               
* ROLL TOTALS FOR K INTO TOTALS FOR CATG AND CLEAR K TOTALS                     
NEXTK20  DS    0H                                                               
         GOTO1 ROLLER,DMCB,4,ACCUMC,1,3                                         
         GOTO1 (RF),(R1),4,ACCUMC,2,4                                           
         GOTO1 (RF),(R1),2,ACCUMC,1                                             
         GOTO1 (RF),(R1),2,ACCUMC,2                                             
*                                                                               
         CLI   QOPTION1,C'T'                                                    
         BE    RD60                                                             
*                                                                               
         ZIC   R4,LINE                                                          
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         DROP  RF                                                               
         CLI   LINE,46                                                          
         BL    RD50                                                             
         MVI   0(R4),C'B'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVI   LINE,99                                                          
         B     RD60                                                             
RD50     MVI   0(R4),C'M'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
RD60     DS    0H                                                               
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   RD61                                                             
*                                                                               
* INITIALIZE ROLLER FOR SPOT LENGTH BREAK OUTS                                  
*                                                                               
         MVC   SPLNWKS,SPACES                                                   
         XC    PERTABSL(212),PERTABSL ZERO WEEK TABLE FOR SPOT LENGTHS          
         GOTO1 ROLLER,DMCB,0,ACCUMSL,ACNMSLLN,ACNMSLCL                          
*                                                                               
RD61     DS    0H                                                               
         TM    BREAKF,EOCAT                                                     
         BZ    RD62                                                             
         BAS   RE,CATBRK                                                        
         NI    BREAKF,X'FF'-(CDATA+ADATA)                                       
         L     R7,SAVER7                                                        
         MVC   LCAT,SCAT                                                        
         B     RD65                                                             
*                                                                               
RD62     CLI   QOPTION1,C'D'                                                    
         BE    RD10A                                                            
*                                                                               
         TM    BREAKF,EOSORT                                                    
         BNZ   RD70                                                             
*                                                                               
         TM    BREAKF,EOADV        IF ADV BRK, PRINT TOTALS                     
         BZ    RD65                                                             
         BAS   RE,ADVBRK                                                        
         NI    BREAKF,X'FF'-ADATA  TURN OFF DATA FLAG AFTER ADV BRK             
RD65     NI    BREAKF,X'FF'-(EOPRD+EOADV+EOAGY+EOCAT)                           
         L     R7,SAVER7                                                        
         MVC   LPRDNM,SPRDNM       SET CURRENT PROD                             
         MVC   LADV,SRADV                                                       
         MVC   LAGY,SRAGY                                                       
         B     RD10B                                                            
*                                                                               
RD70     DS    0H                                                               
         CLI   Q2OPT4,C'Y'         CATG BREAKS?                                 
         BNE   RDX                                                              
         TM    BREAKF,CDATA                                                     
         BZ    RDX                                                              
         BAS   RE,CATBRK                                                        
*                                                                               
RDX      EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
*                                                                               
* PRINT TOTALS                                                                  
         LA    R8,3                TOTALS FOR ALL K'S                           
         BAS   RE,TOTALS                                                        
*                                                                               
         ZIC   R4,LINE                                                          
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         DROP  RF                                                               
         MVI   0(R4),C'B'                                                       
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
RDEXIT   EQU   *                                                                
*        TIME  DEC                                                              
*                                  RETRIEVE TIME: RETURNED IN R0                
*                                     AS HH:MM:SS:TT                            
*        STCM  R0,15,P+50          INSERT TIME RETURNED                         
*        GOTO1 HEXOUT,DMCB,P+50,P+20,4,=C'TOG'                                  
*        MVC   P+1(14),=C'ENDED RPTDONE:'                                       
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
*        MVC   P+1(14),=C'TOTAL BUYS =  '                                       
*        L     RF,BUYCOUNT                                                      
*        EDIT  (RF),(10,P+20),ZERO=NOBLANK                                      
*        GOTO1 LOCALREP,DMCB,ERRORLIN                                           
**                                                                              
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
BUYERR   EQU   *                                                                
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P(32),=C'>>> ERROR READING BUY RECORD <<<'                       
         GOTO1 (RF),(R1),ERRORLIN                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* PRTDAT : PRINT DETAIL LINES 1-4 (AT LEAST 1 & 2)                    *         
*          EXIT CC <> 0 IF NO SPOTS FIT DAYPARTS                      *         
*          ASSUMES R7 STILL AT SORT REC                               *         
*                                                                     *         
***********************************************************************         
PRTDAT   NTR1                                                                   
* DETAIL LINE 1 (ALWAYS)                                                        
* CROSS-CAST #SPOTS LINE TO GET TOTAL SPOTS & GET TOTAL SPOTS                   
         GOTO1 ROLLER,DMCB,5,ACCUMC,1                                           
         GOTO1 (RF),(R1),1,ACCUMC,1                                             
         CLI   QOPTION1,C'T'                                                    
         BE    PD20A                                                            
         L     RE,0(R1)                                                         
         LA    RE,(ACNUMCOL-1)*4(RE)      TOTAL COL                             
         ICM   R0,15,0(RE)                                                      
         BZ    PDBAD                                                            
         ST    R0,SPOTS            SAVE TOTAL SPOTS                             
         SR    R1,R1                                                            
         SRDA  R0,31               2X #SPOTS IN R0 & R1                         
         ZIC   RF,WEEKS                                                         
         LTR   RF,RF               ANY WEEKS?                                   
         BNZ   PD10                YES                                          
         LA    R1,0                NO  - SET COUNT TO ZERO                      
         B     PD15                                                             
PD10     EQU   *                                                                
         DR    R0,RF                                                            
         LTR   R1,R1               SEE IF NEGATIVE (SHOULDN'T BE)               
         BM    *+8                 IF SO, DON'T NEED TO ADD 1                   
         AH    R1,=H'1'                                                         
         SRA   R1,1                DIVIDE BY 2                                  
*                                                                               
PD15     EQU   *                                                                
         EDIT  (R1),(3,P+43)       # SPOTS                                      
         MVC   P+1(L'SADVNM),SADVNM    ADVERTISER NAME                          
         CLI   RCSUBPRG,1          NO STA NAME FOR SUMMARY                      
         BE    PD20                AGENCY NM INSTEAD OF STA NM & DATES          
         MVC   P+22(L'SSTANM),SSTANM   STATION NAME                             
         GOTO1 DATCON,DMCB,(3,SRDATES),(4,P+31)  FLIGHT DATES                   
         GOTO1 (RF),(R1),(3,SRDATES+3),(4,P+37)                                 
         MVI   P+36,C'-'                                                        
         B     *+10                                                             
PD20     MVC   P+22(L'SAGYNM),SAGYNM                                            
PD20A    EQU   *                                                                
* DETAIL LINE 2 (ALWAYS)                                                        
* CROSS-CAST DOLLARS LINE TO GET TOTAL DOLLARS &  GET TOTAL DOLLARS             
         GOTO1 ROLLER,DMCB,5,ACCUMC,2                                           
         GOTO1 (RF),(R1),1,ACCUMC,2                                             
         L     RE,0(R1)                                                         
         LA    RE,(ACNUMCOL-1)*4(RE)      TOTAL COL                             
         MVC   DOLLARS,0(RE)       SAVE TOTAL DOLLARS                           
         CLI   QOPTION1,C'T'                                                    
         BE    PDGOOD                                                           
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   PD22                NO #WEEKS FOR SUMMARY                        
         ZIC   R1,WEEKS                                                         
         LA    R2,PSECOND                                                       
         EDIT  (R1),(3,43(R2))           # WEEKS                                
PD22     MVC   PSECOND+1(20),SPRDNM      PRODUCT NAME                           
         CLI   RCSUBPRG,1          NO K# FOR SUMMARY                            
         BE    PD25                                                             
         ZAP   WORK(5),=P'0'       CONTRACT NUMBER                              
         MVO   WORK(5),SCONNUM                                                  
         EDIT  (P5,WORK),(8,22(R2)),ALIGN=LEFT                                  
         MVC   PSECOND+31(11),SPTLENS    SPOT LENS FROM BUYS                    
         B     *+10                                                             
PD25     MVC   PSECOND+22(11),SPTLENS    SPOT LENS FROM BUYS                    
*                                                                               
         LA    R2,P                                                             
         CLI   QOPTION2,C'D'       DOLLARS ONLY?                                
         BE    PD30                                                             
         CLI   QOPTION2,C'N'       NEITHER (DOLLARS/SPOTS)                      
         BE    PD30                                                             
         LA    R8,1                PRINT SPOT COUNTERS                          
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMC                                    
         LA    R2,PSECOND                                                       
*                                                                               
* CHECK ON % WANTED...                                                          
PD30     DS    0H                                                               
         CLI   QOPTION3,C'D'       PERCENTS ON DOLLARS ONLY?                    
         BE    PD60                                                             
         CLI   QOPTION3,C'N'       NO PERCENTS?                                 
         BE    PD60                                                             
*                                                                               
* READ SPOT LINE, DIVIDE DAYPART SPOTS BY TOTAL SPOTS TO GET %.                 
         GOTO1 ROLLER,DMCB,1,ACCUMC,1                                           
         L     R3,0(R1)            A(LINE)                                      
         GOTO1 PRTPCT,DMCB,46(R2),(R3),SPOTS                                    
*                                                                               
* FIND OUT WHICH LINE TO USE                                                    
PD60     DS    0H                                                               
         CLC   P+47(L'P-47),SPACES                                              
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     PD65                                                             
         CLC   PSECOND+47(L'PSECOND-47),SPACES                                  
         BNE   *+12                                                             
         LA    R2,PSECOND                                                       
         B     PD65                                                             
         LA    R2,PTHIRD                                                        
PD65     DS    0H                                                               
         CLI   QOPTION2,C'S'       SPOTS ONLY?                                  
         BE    PD70                                                             
         CLI   QOPTION2,C'N'       NEITHER (DOLLARS OR SPOTS)                   
         BE    PD70                                                             
         LA    R8,2                PRINT DOLLAR COUNTERS                        
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMC                                    
PD70     DS    0H                                                               
*                                                                               
* CHECK ON % WANTED...                                                          
         CLI   QOPTION3,C'S'       PERCENTS ON SPOTS ONLY?                      
         BE    PD110                                                            
         CLI   QOPTION3,C'N'       NO PERCENTS?                                 
         BE    PD110                                                            
*                                                                               
* FIND OUT WHICH LINE TO USE                                                    
         CLC   P+47(L'P-47),SPACES                                              
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     PD85                                                             
         CLC   PSECOND+47(L'PSECOND-47),SPACES                                  
         BNE   *+12                                                             
         LA    R2,PSECOND                                                       
         B     PD85                                                             
         CLC   PTHIRD+47(L'PTHIRD-47),SPACES                                    
         BNE   *+12                                                             
         LA    R2,PTHIRD                                                        
         B     PD85                                                             
         LA    R2,PFOURTH                                                       
*                                                                               
* READ DOLLAR LINE, DIVIDE DAYPART DOLLARS BY TOTAL DOLLARS TO GET %.           
PD85     GOTO1 ROLLER,DMCB,1,ACCUMC,2                                           
         L     R3,0(R1)            A(LINE)                                      
         GOTO1 PRTPCT,DMCB,46(R2),(R3),DOLLARS                                  
                                                                                
PD110    GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
         CLI   QOPTION2,C'A'       INCLUDE COST-PER-SPOT?                       
         BNE   PD120                                                            
         LA    R2,P                                                             
         LA    R7,1                                                             
         LR    R8,R7                                                            
         GOTO1 COSTSPOT,DMCB,1,ACCUMC DO WITH PENNIES                           
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
PD120    EQU   *                                                                
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   PDGOOD                                                           
                                                                                
         CLI   SPLNWKS+3,C' '                                                   
         BE    PDGOOD                                                           
         GOTO1 =A(PRTSPLN),DMCB,(RC),RR=RELO                                    
*                                                                               
PDGOOD   DS    0H                                                               
         MVI   WEEKS,0             RESET #WEEKS                                 
         XC    PERTAB,PERTAB       ZERO WEEK TABLE                              
         MVC   SPTLENS,SPACES                                                   
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
*                                                                               
* NO SPOTS - CLEAR TOTALS LINES                                                 
PDBAD    DS    0H                                                               
         MVI   WEEKS,0             RESET #WEEKS                                 
         XC    PERTAB,PERTAB       ZERO WEEK TABLE                              
         MVC   SPTLENS,SPACES                                                   
         GOTO1 ROLLER,DMCB,2,ACCUMC,1                                           
         GOTO1 (RF),(R1),2,ACCUMC,2                                             
         MVC   P+1(L'P-1),SPACES                                                
         MVC   PSECOND+1(L'PSECOND-1),SPACES                                    
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* CATBRK: PRINT TOTALS FOR CATEGORY                                   *         
*                                                                     *         
***********************************************************************         
CATBRK   NTR1                                                                   
         CLI   QOPTION1,C'D'                                                    
         BE    CB10                                                             
         TM    BREAKF,ADATA                                                     
         BZ    CB10                                                             
         BAS   RE,ADVBRK                                                        
*                                                                               
CB10     EQU   *                                                                
         CLI   QOPTION1,C'T'                                                    
         BE    CB90                                                             
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         MVI   BOXCOLS+21,C' '     AFTER ADVERTISER/PRODUCT                     
         MVI   BOXCOLS+30,C' '     AFTER STATION/CONTRACT                       
         MVI   BOXCOLS+42,C' '     FLIGHT/SPOT LENS                             
         MVI   BOXCOLS+46,C' '     AFTER SPW/#W                                 
         ZIC   R4,LINE                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         MVI   0(R4),C'M'                                                       
         DROP  RF                                                               
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
*                                                                               
         MVC   P+10(30),SRCATNM                                                 
*                                                                               
* PUT (000) AFTER CAT NAME                                                      
         LA    RF,P+40                                                          
*                                                                               
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         MVC   2(5,RF),=C'(000)'                                                
*                                                                               
         LA    R8,7                                                             
         BAS   RE,TOTALS                                                        
         ZIC   R4,LINE                                                          
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         DROP  RF                                                               
         MVI   0(R4),C'B'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVI   LINE,99                                                          
* CLEAR TOTALS                                                                  
CB90     EQU   *                                                                
         GOTO1 ROLLER,DMCB,2,ACCUMC,7                                           
         GOTO1 (RF),(R1),2,ACCUMC,8                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ADVBRK: PRINT TOTALS FOR ADVERTISER                                 *         
*                                                                     *         
***********************************************************************         
ADVBRK   NTR1                                                                   
         CLI   QOPTION1,C'T'                                                    
         BE    ABRK10                                                           
         GOTO1 LOCALREP,DMCB,BLANKLIN                                           
         MVC   P+22(20),=C'ADVERTISER TTL (000)'                                
         LA    R8,5                                                             
         BAS   RE,TOTALS                                                        
*                                                                               
         ZIC   R4,LINE                                                          
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         DROP  RF                                                               
         MVI   0(R4),C'M'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
* CLEAR TOTALS                                                                  
ABRK10   EQU   *                                                                
         GOTO1 ROLLER,DMCB,2,ACCUMC,5                                           
         GOTO1 (RF),(R1),2,ACCUMC,6                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* TOTALS:  PRINT TOTALS.  PRINT DOLLARS IN THOUSANDS IF TROLL ON.     *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
* ---------------                                                     *         
* R8 = TOTAL LINE (FOR SPOTS - 3, 5, OR 7)                            *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
TOTALS   NTR1                                                                   
         LR    R7,R8               SAVE OFF PARAM                               
         CLI   QOPTION2,C'S'       PRINT SPOTS?                                 
         BE    TTOT10                                                           
         CLI   QOPTION2,C'A'                                                    
         BE    TTOT10                                                           
         CLI   QOPTION2,C'B'                                                    
         BNE   TTOT20                                                           
TTOT10   EQU   *                                                                
         LA    R2,P                                                             
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMC                                    
         MVC   P+2(14),=C'TOTALS (SPOTS)'                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
TTOT20   DS    0H                                                               
         CLI   QOPTION3,C'S'       PRINT %/SPOTS?                               
         BE    TTOT25                                                           
         CLI   QOPTION3,C'B'                                                    
         BNE   TTOT30                                                           
TTOT25   EQU   *                                                                
         GOTO1 ROLLER,DMCB,5,ACCUMC,(R8)   CROSS CAST                           
         GOTO1 (RF),(R1),1,ACCUMC,(R8)     GET A(LINE)                          
         L     R3,0(R1)                                                         
         LA    RE,(ACNUMCOL-1)*4(R3)       GET A(TOTAL COL)                     
         LA    R8,0(RE)                                                         
         LA    R2,P+46                                                          
         GOTO1 PRTPCT,DMCB,(R2),(R3),(R8)                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
TTOT30   DS    0H                                                               
         LA    R8,1(R7)            DOLL LN ALWAYS 1 MORE THAN SPOT LN           
         CLI   QOPTION2,C'D'       PRINT DOLLARS?                               
         BE    TTOT35                                                           
         CLI   QOPTION2,C'A'                                                    
         BE    TTOT35                                                           
         CLI   QOPTION2,C'B'                                                    
         BNE   TTOT40                                                           
TTOT35   EQU   *                                                                
         LA    R2,P                                                             
         OI    BREAKF,TROLL                                                     
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMC                                    
         MVC   P+2(12),=C'TOTALS (000)'                                         
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
* READ DOLLAR LINE, DIVIDE DAYPART DOLLARS BY TOTAL DOLLARS TO GET %.           
TTOT40   EQU   *                                                                
         CLI   QOPTION3,C'D'       PRINT %/DOLLARS?                             
         BE    TTOT45                                                           
         CLI   QOPTION3,C'B'                                                    
         BNE   TTOT50                                                           
TTOT45   EQU   *                                                                
         GOTO1 ROLLER,DMCB,5,ACCUMC,(R8)   CROSS CAST                           
         GOTO1 (RF),(R1),1,ACCUMC,(R8)     GET A(LINE)                          
         L     R3,0(R1)                                                         
         LA    RE,(ACNUMCOL-1)*4(R3)       GET A(TOTAL COL)                     
         LA    R8,0(RE)                                                         
         LA    R2,P+46                                                          
         GOTO1 PRTPCT,DMCB,(R2),(R3),(R8)                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
TTOT50   EQU   *                                                                
         CLI   QOPTION2,C'A'                                                    
         BNE   TTOT60                                                           
         LR    R8,R7                                                            
         LA    R2,P                                                             
         GOTO1 COSTSPOT,DMCB,0,ACCUMC    DO W/O PENNIES                         
         MVC   P+2(17),=C'AVERAGE UNIT RATE'                                    
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
TTOT60   EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* PRTPCT:  PRINT PERCENTAGES                                          *         
*                                                                     *         
* PARAMTER USAGE:                                                     *         
* ---------------                                                     *         
* P1 = A(PRINT LOCATION)            INPUT                             *         
* P2 = A(1ST TOTAL BUCKET)          INPUT                             *         
* P3 = TOTAL # OF DOLLARS/SPOTS     INPUT                             *         
*                                                                     *         
***********************************************************************         
PRTPCT   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R1,8(R1)                                                         
         L     R8,0(R1)                                                         
                                                                                
         LTR   R8,R8               SKIP IF NO TOTAL SPOT/DOLLARS                
         BZ    PPX                                                              
         ZIC   R4,NUMDPTS                                                       
PP10     ICM   R1,15,0(R3)         ANY SPOTS/DOLLARS?                           
         BZ    PP20                                                             
         SR    R0,R0               ZERO HIGH WORD                               
         LA    RF,100              MULT BY 100 S.T. % IS WHOLE NUMBER           
         MR    R0,RF                                                            
*                                                                               
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         DR    R0,R8                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(8,0(R2))                                                   
         MVI   8(R2),C'%'                                                       
*                                                                               
PP20     LA    R3,4(R3)                                                         
         LA    R2,9(R2)                                                         
         BCT   R4,PP10                                                          
*                                                                               
PPX      DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROLLOUT: OUTPUT ROLLER LINE TO PRINT LINE.                          *         
*          P1 MUST HAVE A(PRTLINE) (IE: A(P), A(PSECOND))             *         
*          P2 MUST HAVE LINE NUMBER (ODD LINE #'S ARE SPOTS)          *         
*          P3 MUST HAVE A(ACCUMULATOR)                                *         
***********************************************************************         
ROLLOUT  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R8,4(R1)                                                         
         L     R5,8(R1)                                                         
                                                                                
         GOTO1 ROLLER,DMCB,1,(R5),(R8)                                          
         STC   R8,DMCB+8                                                        
         L     R3,0(R1)            A(LINE)                                      
         ZIC   R4,NUMDPTS          # DAYPARTS                                   
         LA    R2,47(R2)                                                        
RO10     DS    0H                                                               
         TM    DMCB+8,X'01'        IF ODD, SPOTS, ELSE DOLLARS                  
         BZ    RODOLL                                                           
         EDIT  (4,(R3)),(8,0(R2))                                               
         B     RO20                                                             
RODOLL   DS    0H                  DOLLARS ONLY - NO CENTS                      
         ICM   R1,15,0(R3)         DOLLARS IN R1                                
         BZ    RO20                SKIP IF NO $$$                               
         TM    BREAKF,TROLL                                                     
         BZ    RO15                                                             
* PRINT IN THOUSANDS                                                            
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31               THIS WILL PUT 2X DOLLARS IN R0 & R1          
         D     R0,=F'1000'                                                      
         LTR   R1,R1               SEE IF RESULT IS NEG                         
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'            NOT AN LA!!!                                 
         SRA   R1,1                DIVIDE RESULT BY 2                           
RO15     EDIT  (R1),(8,0(R2)),FLOAT=$                                           
RO20     LA    R3,4(R3)                                                         
         LA    R2,9(R2)                                                         
         BCT   R4,RO10                                                          
*                                                                               
         SR    R0,R0                                                            
         NI    BREAKF,X'FF'-TROLL  RESET TOTAL FLAG                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* COSTSPOT:   OUTPUT THE COST-PER-SPOT TO THE PRINT LINE              *         
*             R2 MUST HAVE A(PRTLINE) (IE: A(P), A(PSECOND))          *         
*             R7 MUST HAVE SPOT LINE NUMBER                           *         
*                                                                     *         
*                                                                     *         
*             P1 = 0 DO AS DOLLARS                                    *         
*                  NON-0 DO AS DOLLARS.CENTS                          *         
*             P2 = A(ACCUMULATOR)                                     *         
*                                                                     *         
***********************************************************************         
COSTSPOT NTR1                                                                   
         L     R6,0(R1)            PUT P1 IN R6                                 
         L     R4,4(R1)            PUT P2 IN R4                                 
                                                                                
         GOTO1 ROLLER,DMCB,1,(R4),(R7)                                          
         L     R3,0(R1)            A(SPOT LINE)                                 
         LA    R8,1(R7)                                                         
         GOTO1 ROLLER,DMCB,1,(R4),(R8)                                          
         L     R4,0(R1)            A(DOLLAR LINE)                               
         ZIC   R5,NUMDPTS          # DAYPARTS                                   
         LA    R2,47(R2)                                                        
CSPT10   EQU   *                                                                
         ICM   R1,15,0(R4)         DOLLARS IN R1                                
         BZ    CSPT20              SKIP IF NO $$$                               
         LTR   R6,R6                                                            
         BZ    CSPT12                                                           
         MH    R1,=H'100'                                                       
*                                                                               
CSPT12   EQU   *                                                                
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31               THIS WILL PUT 2X DOLLARS IN R0 & R1          
         D     R0,0(R3)            DIVIDE BY NUMBER OF SPOTS                    
         LTR   R1,R1               SEE IF RESULT IS NEG                         
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'            NOT AN LA!!!                                 
         SRA   R1,1                DIVIDE RESYLT BY 2                           
         LTR   R6,R6                                                            
         BZ    CSPT15                                                           
         EDIT  (R1),(8,0(R2)),2,FLOAT=$                                         
         B     CSPT20                                                           
CSPT15   EQU   *                                                                
         EDIT  (R1),(8,0(R2)),FLOAT=$                                           
CSPT20   EQU   *                                                                
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R2,9(R2)                                                         
         BCT   R5,CSPT10                                                        
*                                                                               
         SR    R0,R0                                                            
         NI    BREAKF,X'FF'-TROLL  RESET TOTAL FLAG                             
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* HHOOK --- HEADHOOK ROUTINE                                          *         
*                                                                     *         
***********************************************************************         
HHOOK    NTR1                                                                   
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS                                                      
         DROP  RF                                                               
         LM    R2,RC,0(R1)                                                      
*                                                                               
* SET UP BOXES                                                                  
         BAS   RE,BOXES                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(6,HEAD2+58)                              
         MVI   HEAD2+64,C'-'                                                    
         GOTO1 (RF),(R1),(0,QEND),(6,HEAD2+65)                                  
*                                                                               
         CLC   QR30AGY,SPACES      IS THERE A TARG AGY?                         
         BE    HH03                                                             
         MVC   HEAD3+1(13),=C'TARGET AGENCY'                                    
         MVC   HEAD3+20(6),QR30AGY                                              
         MVC   HEAD3+28(33),TARGAGY                                             
*                                                                               
HH03     DS    0H                                                               
         MVC   HEAD3+99(07),=C'PRINT: '                                         
         CLI   QOPTION2,C'S'                                                    
         BNE   *+14                                                             
         MVC   HEAD3+106(13),=C'SPOTS ONLY   '                                  
         B     HH05                                                             
         CLI   QOPTION2,C'D'                                                    
         BNE   *+14                                                             
         MVC   HEAD3+106(13),=C'DOLLARS ONLY '                                  
         B     HH05                                                             
         CLI   QOPTION2,C'N'                                                    
         BNE   *+14                                                             
         MVC   HEAD3+106(13),=C'NONE         '                                  
         B     HH05                                                             
         CLI   QOPTION2,C'B'                                                    
         BNE   *+14                                                             
         MVC   HEAD3+106(13),=C'SPOTS/DOLLARS'                                  
         B     HH05                                                             
         MVC   HEAD3+106(13),=C'W/ AVG RATE  '                                  
*                                                                               
HH05     DS    0H                                                               
         MVC   HEAD4+99(26),=C'PRINT PERCENT OF DAYPART: '                      
         CLI   QOPTION3,C'S'                                                    
         BNE   *+14                                                             
         MVC   HEAD4+125(7),=C'SPOTS  '                                         
         B     HH07                                                             
         CLI   QOPTION3,C'D'                                                    
         BNE   *+14                                                             
         MVC   HEAD4+125(7),=C'DOLLARS'                                         
         B     HH07                                                             
         CLI   QOPTION3,C'N'                                                    
         BNE   *+14                                                             
         MVC   HEAD4+125(7),=C'NONE   '                                         
         B     HH07                                                             
         MVC   HEAD4+125(7),=C'SPT/DOL'                                         
*                                                                               
HH07     DS    0H                                                               
         LA    R4,HEAD7+47                                                      
         L     R6,ARADAR                                                        
         CLI   0(R6),0             RECORD THERE?                                
         BZ    HHOOKEXT            NOPE                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST 1                           
         USING RRDADPEL,R6                                                      
*                                                                               
HH10     DS    0H                                                               
         CLC   RRDADPTX,=C'+       '                                            
         BE    HH20                NO COL FOR CONT. CARD                        
         MVC   0(L'RRDADPTX,R4),RRDADPTX              HEAD7                     
         LA    R4,9(R4)                                                         
HH20     BAS   RE,NEXTEL                                                        
         BE    HH10                                                             
*                                                                               
HHOOKEXT EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* BOXES: SET UP BOXES FOR NEAT OUTPUT                                 *         
*                                                                     *         
***********************************************************************         
BOXES    NTR1                                                                   
         L     R4,ABOX             A(BOX DSECT)                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+8,C'M'      UNDER HEADINGS                               
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   BOXCOLS,C'L'        LEFT BORDER                                  
         MVI   BOXCOLS+21,C'C'     AFTER ADVERTISER/PRODUCT                     
         CLI   RCSUBPRG,0                                                       
         BNE   *+8                                                              
         MVI   BOXCOLS+30,C'C'     AFTER STATION/CONTRACT                       
         MVI   BOXCOLS+42,C'C'     FLIGHT/SPOT LENS                             
         MVI   BOXCOLS+46,C'C'     AFTER SPW/#W                                 
*                                                                               
         ZIC   R5,NUMDPTS                                                       
         LA    R6,BOXCOLS+46                                                    
         LTR   R5,R5                                                            
         BZ    BOXES50                                                          
*                                                                               
         LA    R6,9(R6)                                                         
         MVI   0(R6),C'C'                                                       
         BCT   R5,*-8                                                           
BOXES50  DS    0H                                                               
         MVI   0(R6),C'R'          AFTER LAST DAYPART                           
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
BOXES99  DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* LOCALREP --- LOCAL REPORT INTERFACE                                 *         
*                                                                     *         
***********************************************************************         
LOCALREP NTR1                                                                   
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
LREP10   EQU   *                                                                
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LREP20                                                           
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LREP10                                                           
LREP20   EQU   *                                                                
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
*                                                                               
LREPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 3                                                                
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
*                                                                               
* COMMON ROUTINES FOR SAVING CODE SPACE                                         
         SPACE 3                                                                
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
* PROGRAM LITERAL AREA                                                          
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SORT DEFNS                                                                    
SORTCARD DC    CL80'SORT FIELDS=(X,XXX,A),FORMAT=CH,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXX'                                   
*                                                                               
* WORK AREA                                                                     
RELO     DS    A                                                                
SAVEREGS DS    11F                                                              
* DATAMGR STUFF                                                                 
COMMAND  DS    CL8                                                              
AIOAREA  DS    F                                                                
ARADAR   DS    A                   A(IOBUFF) - RADAR RECORD                     
ACCUMS   DS    A                   A(ACCUMC)                                    
ACATSPAC DS    A                   A(1ST CATEGORY ENTRY)                        
* BREAK (SUMMARY OR TOTAL) CONTROL DATA                                         
LADV     DS    CL4                 LAST ADVERTISER CODE                         
LPRDNM   DS    CL20                LAST PRODUCT NAME                            
LAGY     DS    CL4                 LAST AGY CODE                                
LCAT     DS    CL2                 LAST CATGORY NAME                            
*                                                                               
CATFOUND DS    CL1                                                              
SPTLENS  DS    CL11                SPOT LENGTHS                                 
SPLNWKS  DS    CL12                SPOT LENGTHS AND # OF WEEKS                  
*                                  2 BYTES - SPOT LENGTH                        
*                                  1 BYTE  - # OF WEEKS                         
WEEKS    DS    X                   COUNT OF WEEKS BUY IS IN REQ PER             
PERTAB   DS    CL53                WEEKLY PERIODS...                            
PERTABSL DS    4CL53               WEEKLY PERIODS FOR SPOT LENGTHS              
*                                                                               
DATE     DS    CL6                                                              
EDATE    DS    CL6                                                              
REQSTBC  DS    CL12                QSTART BC MON ST & ED DATES                  
REQEDBC  DS    CL12                QEND BC MON ST & ED DATES                    
FLTSTART DS    CL3                 FLIGHT START: YMD BINARY                     
FLTEND   DS    CL3                 FLIGHT END  : YMD BINARY                     
BUYSTDT  DS    CL6                 BUY START DATE (YYMMDD EBCDIC)               
BUYEDDT  DS    CL6                 BUY END DATE (YYMMDD EBCDIC)                 
BSTART   DS    XL3                 BINARY START DATE (FM REQ CARD)              
BEND     DS    XL3                 BINARY END DATE (FM REQ CARD)                
*                                                                               
DAYPART  DS    X                   DAYPART DEFN CURRENTLY CHECKING(1-9)         
NUMDPTS  DS    X                   NUMBER OF DAYPARTS(1-9)                      
MASK     DS    X                   FOR CHECKING DAY FIT                         
SPOTS    DS    F                   TOTAL SPOTS FOR K                            
DOLLARS  DS    F                   TOTSL DOLLARS FOR K                          
TARGAGY  DS    CL33                EXPANDED TARGET AGY NAME                     
*                                                                               
SVDPDY   DS    X                   DAYPARTS CHECKED (FOR + DEFINITIONS)         
CONDAYDF DS    X                   HAS CONTINUING DAYPART DEFINITIONS           
*                                  X'80' = YES, HAS CONT DP DEFS                
*                                  X'40' = NO, NO CONT DP DEFS                  
*                                  X'20' = NO MATCH, DON'T BOTHER WITH          
*                                      REST OF CONTINUING DAYPART DEFS          
*                                                                               
BREAKF   DS    X                   BREAK FLAGS                                  
EOSORT   EQU   X'80'               END OF SORT                                  
EOPRD    EQU   X'40'               END OF PRODUCT                               
EOADV    EQU   X'20'               END OF ADVERTISER                            
EOAGY    EQU   X'10'               END OF AGENCY                                
EOCAT    EQU   X'08'               END OF CATEGORY                              
CDATA    EQU   X'04'               FLAG FOR CATG DATA                           
ADATA    EQU   X'02'               FLAG FOR ADV DATA                            
TROLL    EQU   X'01'               FLAG TO ROLLOUT TO PRT DATA IN 000'S         
*                                                                               
DUB2     DS    D                                                                
ELCODE   DS    CL1                 ELEMENT CODE                                 
SAVER7   DS    F                                                                
SORTREC  DS    CL(SRECLEN)                                                      
CARD2    DS    CL80                                                             
BUCKETS  DS    CL200               200 BYTE OUTPUT AREA FOR REGENBUC            
RBLOCK   DS    4F                  ROUTINE BLOCK FOR REGENBUC                   
REPPROF  DS    X                   REP PROFILE                                  
*                                  X'80'=USE 6A-559A START/END TIME             
STRTTIME DS    H                   START TIME, DEFAULTS TO =H'0500'             
CONCOUNT DS    F                   CONTRACT COUNTER                             
BUYCOUNT DS    F                   BUY      COUNTER                             
SRTCTOUT DS    F                   SORT     COUNTER OUT                         
SRTCTIN  DS    F                   SORT     COUNTER IN                          
*                                                                               
         ORG   CARD2                                                            
       ++INCLUDE REGENREQ2                                                      
         ORG                                                                    
         EJECT                                                                  
*                                                                               
** A SCHEMATIC OF THE ACCUMULATORS FOR ROLLER FOLLOWS-                          
*                                                                               
*       ______________________________________________________                  
*       |           |DAYPART1|DAYPART2|...|DAYPART9|DP TOTALS|                  
*       |-----------|--------|--------|---|--------|---------|                  
* LVL 0 | SPOTS  (1)|        |        |...|        |         |                  
*       | DOLLARS(2)|        |        |...|        |         |                  
*       |-----------|--------|--------|---|--------|---------|                  
*     1 | SPOTS  (3)|        |        |...|        |         |                  
*       | DOLLARS(4)|        |        |...|        |         |                  
*       |-----------|--------|--------|---|--------|---------|                  
*     . |     .     |   .    |   .    | . |   .    |    .    |                  
*     . |     .     |   .    |   .    | . |   .    |    .    |                  
*       |-----------|--------|--------|---|--------|---------|                  
*     4 | SPOTS  (9)|        |        |...|        |         |                  
*       | DOLLARS 10|        |        |...|        |         |                  
*       ------------------------------------------------------                  
*                                                                               
* --->   ACCUMULATOR SIZE = (LINE*COL*4)+8 = (10*8*4)+8 = 328 BYTES             
*                                                                               
* --->   LVL 0 SUMMARY FOR THIS K/PRODUCT  (ROWS 1 & 2)                         
*            1 TOTALS FOR ALL K'S          (ROWS 3 & 4)                         
*            3 TOTALS FOR THIS ADVERTISER  (ROWS 5 & 6)                         
*            4 TOTALS FOR THIS CATG        (ROWS 7 & 8)                         
*                                                                               
ACNUMCOL EQU   10                                                               
ACNUMLIN EQU   08                                                               
         ENTRY ACCUMC                                                           
ACCUMC   DS    0D                                                               
         DS    ((ACNUMCOL*ACNUMLIN*4)+8)C                                       
*                                                                               
*       ______________________________________________________                  
*       |           |DAYPART1|DAYPART2|...|DAYPART9|DP TOTALS|                  
*       |-----------|--------|--------|---|--------|---------|                  
* SPOT  | SPOTS     |        |        |...|        |         |                  
* LEN 1 | DOLLARS   |        |        |...|        |         |                  
*       |-----------|--------|--------|---|--------|---------|                  
* SPOT  | SPOTS     |        |        |...|        |         |                  
* LEN 2 | DOLLARS   |        |        |...|        |         |                  
*       |-----------|--------|--------|---|--------|---------|                  
* SPOT  | SPOTS     |        |        |...|        |         |                  
* LEN 3 | DOLLARS   |        |        |...|        |         |                  
*       |-----------|--------|--------|---|--------|---------|                  
* SPOT  | SPOTS     |        |        |...|        |         |                  
* LEN 4 | DOLLARS   |        |        |...|        |         |                  
*       ------------------------------------------------------                  
*                                                                               
* --->   ACCUMULATOR SIZE = (LINE*COL*4)+8 = (10*8*4)+8 = 328 BYTES             
*                                                                               
ACNMSLCL EQU   10                                                               
ACNMSLLN EQU   08                                                               
         ENTRY ACCUMSL                                                          
ACCUMSL  DS    0D                                                               
         DS    ((ACNMSLCL*ACNMSLLN*4)+8)C                                       
*                                                                               
         ENTRY IOBUFF                                                           
IOBUFF   DS    0D                                                               
         DC    1008X'00'           WORK FILE BUFFER                             
         EJECT                                                                  
DOBUYS   NMOD1 0,*DOBUYS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
* GO READ BUYS FOR THIS K                                                       
         LA    R1,RBUYREC                                                       
         ST    R1,AIOAREA                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),QREP                                                   
* GET R9'S K NUM                                                                
         ZAP   WORK(8),=P'0'                                                    
         MVO   WORK(8),SCONNUM                                                  
         ZAP   DUB,=P'99999999'                                                 
         SP    DUB,WORK(8)                                                      
         UNPK  WORK(8),DUB                                                      
         OI    WORK+7,X'F0'                                                     
         PACK  DUB(5),WORK(9)                                                   
         MVC   KEY+18(4),DUB                                                    
         PACK  KEY+18(1),DUB+3(1)       REVERSE THE COMPLIMENT                  
         PACK  KEY+19(1),DUB+2(1)                                               
         PACK  KEY+20(1),DUB+1(1)                                               
         PACK  KEY+21(1),DUB(1)                                                 
         GOTO1 HIGH                                                             
         BNZ   TESTEXIT                                                         
         B     DBUY30                                                           
DBUY20   EQU   *                                                                
         GOTO1 SEQ                                                              
DBUY30   EQU   *                                                                
         CLC   KEY(22),KEYSAVE                                                  
         BE    DBUY40                                                           
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
DBUY40   EQU   *                                                                
         CLC   =X'FFFF',KEY+25     SKIP PLANS...                                
         BE    DBUY20                                                           
DBUY50   EQU   *                                                                
         GOTO1 GREC                                                             
         BNZ   TESTEXIT                                                         
         CLI   RBUYCHGI,C'C'                                                    
         BE    DBUY20              SKIP CANCELLED LINES                         
*                                                                               
         L     RF,BUYCOUNT         INCREMENT BUY COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCOUNT                                                      
*                                                                               
* INITIALIZE DAYPART COUNTER AND GET 1ST DAYPART DEFN ELEM                      
*                                                                               
         MVI   DAYPART,1           FOR ROLLER                                   
         L     R6,ARADAR                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST 1                           
         LR    R5,R6                                                            
                                                                                
DBUY60   DS    0H                                                               
         XC    SVDPDY,SVDPDY       CLEAR SAVED DAYPARTS FOR CONT'D DEF          
         MVI   CONDAYDF,X'40'      DEFAULT ONE DAYPART DEFINITION               
                                                                                
         LR    RF,R5                                                            
         ZIC   R1,1(RF)            GET NEXT DAYPART DEFN EL                     
         AR    RF,R1                                                            
         CLI   0(RF),X'02'                                                      
         BNE   DBUY70                                                           
         USING RRDADPEL,RF                                                      
         CLC   RRDADPTX,=C'+       '                                            
         BNE   DBUY70              CONT. CARDS NOT NEW DAYPART DEFN             
         MVI   CONDAYDF,X'80'      SET MORE THAN ONE DAYPART DEFINITION         
         DROP  RF                                                               
*                                                                               
DBUY70   BAS   RE,CKDT                                                          
         BZ    DBUY73              NO MATCH ON D/T - CHECK NEXT DEFN            
         OI    CONDAYDF,X'20'                                                   
         B     NEXTDPT             NO MATCH ON D/T - SKIP ALL CONT DEFS         
                                                                                
DBUY73   DS    0H                                                               
         TM    CONDAYDF,X'40'                                                   
         BO    DBUY75                                                           
         LA    R6,RBUYREC          R6 WILL HAVE BUY X'02'                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
         CLC   RBUYDAYS,SVDPDY                                                  
         BNE   NEXTDPT                                                          
         DROP  R6                                                               
*                                                                               
* BUILD BUCKETS FOR BUY                                                         
DBUY75   DS    0H                                                               
         GOTO1 =V(REGENBUC),DMCB,RBUYREC,BUCKETS,RBLOCK                         
*                                                                               
* SEE IF ANY ACTIVITY DURING REQUEST PERIOD AND ADD TOTALS FOR BUY              
         BAS   RE,CKACTV                                                        
         B     NEXTBUY                                                          
*                                                                               
NEXTDPT  DS    0H                                                               
         ZIC   R1,1(R5)            GET NEXT DAYPART DEFN EL                     
         AR    R5,R1                                                            
         CLI   0(R5),X'02'                                                      
         BNE   NEXTBUY                                                          
         USING RRDADPEL,R5                                                      
         CLC   RRDADPTX,=C'+       '                                            
         BNE   DBUY100             CONT. CARDS NOT NEW DAYPART DEFN             
         TM    CONDAYDF,X'20'      IF DEF IS CONT DAYPART, AND                  
         BO    NEXTDPT             ALREADY THERE'S NO MATCH, LOOKING            
*                                  FOR NEXT NON-CONT DAYPART DEF                
         B     DBUY70                                                           
         DROP  R5                                                               
                                                                                
DBUY100  DS    0H                                                               
         ZIC   R1,DAYPART          SET NEXT COLUMN FOR ROLLER                   
         LA    R1,1(R1)                                                         
         STC   R1,DAYPART                                                       
         B     DBUY60                                                           
*                                                                               
NEXTBUY  DS    0H                                                               
         MVI   DAYPART,1                                                        
         B     DBUY20                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* CKDT:  SEE IF BUY FITS INTO DAYPART DEFN ADDRESSED BY R5.           *         
*        EXIT CC=0 IF GOOD                                            *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
* ---------------                                                     *         
* R0 = BUY START TIME                                                 *         
* R1 = BUY END TIME                                                   *         
* R5 = RDA RECORD                                                     *         
* R6 = BUY X'02' ELEM (DATE ELEM)                                     *         
* R7 = ASSUMED TO STILL BE AT SORT REC                                *         
* RE = DAYPART DEFN START TIME                                        *         
* RF = DAYPART DEFN END TIME                                          *         
*                                                                     *         
***********************************************************************         
CKDT     NTR1                                                                   
         USING RRDADPEL,R5         R5 WILL HAVE DAYPART DEFN EL                 
         CLI   RRDADPD1,X'00'      DAY/TIME = *?                                
         BE    MATCH               EVERYTHING CATEGORY                          
         LA    R6,RBUYREC          R6 WILL HAVE BUY X'02'                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   NOMATCH                                                          
         USING RBUYDYEL,R6                                                      
*                                                                               
* SET UP BUY TIMES                                                              
         CLC   =C'NONE',RBUYDYT1                                                
         BE    NOMATCH                                                          
         CLC   =C'VARY',RBUYDYT1                                                
         BE    NOMATCH                                                          
*                                                                               
         ZICM  R0,RBUYDYT1,2                                                    
*                                                                               
         CH    R0,STRTTIME                                                      
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
*                                                                               
         ZICM  R1,RBUYDYT2,2                                                    
         BZ    *+16                DON'T ADD IF NO END TIME                     
         CH    R1,STRTTIME                                                      
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
*                                                                               
* SEE IF BUY FITS INTO DAY/TIME 1                                               
         CLI   SRSTATZ,0           STATION TZ?                                  
         BE    CK01                                                             
* RDA ONLY SUPPORTS TZ'S E & C                                                  
         CLI   SRSTATZ,C'P'        E=P                                          
         BNE   *+12                                                             
         MVI   SRSTATZ,C'E'                                                     
         B     *+16                                                             
*                                                                               
         CLI   SRSTATZ,C'M'        C=M                                          
         BNE   *+8                                                              
         MVI   SRSTATZ,C'C'                                                     
*                                                                               
         CLI   RRDADPT1,0          RDA TZ?                                      
         BE    CK01                                                             
         CLC   SRSTATZ,RRDADPT1    SAME TZ?                                     
         BNE   CK10                                                             
*                                                                               
CK01     MVC   MASK,RRDADPD1                                                    
         NC    MASK,RBUYDAYS                                                    
         TM    CONDAYDF,X'40'      MULTI-DEFINITION DAYPART, (IE:'+')?          
         BO    CK03                                                             
         OC    MASK,MASK           YES, ANY MATCH ON DAYS?                      
         BZ    CK10                                                             
         OC    SVDPDY,MASK         SAVE DAYS CHECKED                            
         B     CK04                                                             
                                                                                
CK03     DS    0H                                                               
         CLC   MASK,RBUYDAYS                                                    
         BNE   CK10                NO MATCH ON DAYS                             
*                                                                               
CK04     DS    0H                                                               
         ZICM  RE,RRDADPD1+2,2                                                  
         ZICM  RF,RRDADPD1+4,2                                                  
*                                                                               
         CH    RE,STRTTIME                                                      
         BNL   *+8                                                              
         AH    RE,=H'2400'                                                      
         CH    RF,STRTTIME                                                      
         BNL   *+8                                                              
         AH    RF,=H'2400'                                                      
*                                                                               
         CR    R0,RE               BUY START LT DAYPT START?                    
         BL    CK05                SEE IF END TIME GT START TIME                
         CR    R0,RF               IS BUY START LT DAYPT END?                   
         BL    MATCH                                                            
         BH    CK10                                                             
         OC    RBUYDYT2,RBUYDYT2   SINGLE TIME?                                 
         BZ    MATCH               FITS DAYPART DEFN                            
         B     CK10                                                             
CK05     CR    R1,RE               END TIME GT START TIME?                      
         BH    MATCH                                                            
*                                                                               
* SEE IF BUY FITS INTO DAY/TIME 2                                               
CK10     DS    0H                                                               
         OC    RRDADPD2,RRDADPD2                                                
         BZ    NOMATCH                                                          
*                                                                               
         CLI   SRSTATZ,0           STATION TZ???                                
         BE    CK15                                                             
*                                                                               
         CLI   RRDADPT2,0          RDA TZ?                                      
         BE    CK15                                                             
         CLC   SRSTATZ,RRDADPT2    SAME TZ?                                     
         BNE   NOMATCH                                                          
*                                                                               
CK15     MVC   MASK,RRDADPD2                                                    
         NC    MASK,RBUYDAYS                                                    
         TM    CONDAYDF,X'40'      MULTI-DEFINITION DAYPART, (IE:'+')?          
         BO    CK18                                                             
         OC    MASK,MASK           YES, ANY MATCH ON DAYS?                      
         BZ    MATCH                                                            
         OC    SVDPDY,MASK         SAVE DAYS CHECKED                            
         B     CK19                                                             
                                                                                
CK18     DS    0H                                                               
         CLC   MASK,RBUYDAYS                                                    
         BNE   NOMATCH                                                          
*                                                                               
CK19     DS    0H                                                               
         ZICM  RE,RRDADPD2+2,2                                                  
         ZICM  RF,RRDADPD2+4,2                                                  
*                                                                               
         CH    RE,STRTTIME                                                      
         BNL   *+8                                                              
         AH    RE,=H'2400'                                                      
         CH    RF,STRTTIME                                                      
         BNL   *+8                                                              
         AH    RF,=H'2400'                                                      
*                                                                               
         CR    R0,RE               BUY ST LT DAYPT ST?                          
         BL    CK20                SEE IF END TIME GT START TIME                
         CR    R0,RF               BUY ST LT DAYPT END?                         
         BL    MATCH                                                            
         BH    NOMATCH                                                          
         OC    RBUYDYT2,RBUYDYT2   SINGLE TIME?                                 
         BZ    MATCH               FITS DAYPART DEFN                            
         B     NOMATCH                                                          
CK20     CR    R1,RE               BUY END GT DAYPT ST?                         
         BNH   NOMATCH                                                          
MATCH    DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
NOMATCH  DS    0H                                                               
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* CKACTV: SEE IF ANY ACTIVITY FOR BUY DURING REQUEST PERIOD AND       *         
*         FILL ROLLER TOTALS WITH DOLLARS FOR DOLLARS/SPOTS           *         
*         PICK UP SPOT LENGTHS...                                     *         
*                                                                     *         
***********************************************************************         
CKACTV   NTR1                                                                   
         LA    R2,BUCKETS+2                                                     
         SR    R3,R3                                                            
CA10     CLC   2(2,R2),BSTART                                                   
         BL    NEXTBUC                                                          
         CLC   2(2,R2),BEND                                                     
         BH    CA15                                                             
*                                                                               
* BUCKET FALLS WITHIN REQUEST PERIOD                                            
         IC    R3,DAYPART                                                       
         ICM   R4,15,10(R2)                                                     
         BZ    CA10A                                                            
         OI    BREAKF,CDATA+ADATA  SET TOTAL DATA FLAG                          
                                                                                
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   CA10AA                                                           
         GOTO1 EXSPTLEN,DMCB,(C'S',0),(R3),(R4)                                 
                                                                                
CA10AA   DS    0H                                                               
         GOTO1 ROLLER,DMCB,3,ACCUMC,(R4),1,(R3)   SPOTS FOR K/SUMMARY           
         GOTO1 (RF),(R1),3,ACCUMC,(R4),5,(R3)     SPOTS FOR ADV                 
         GOTO1 (RF),(R1),3,ACCUMC,(R4),7,(R3)     SPOTS FOR CAT                 
*                                                                               
CA10A    ICM   R0,15,6(R2)                                                      
         BZ    NEXTBUC                                                          
         OI    BREAKF,CDATA+ADATA  SET TOTAL DATA FLAG                          
* STORE IN DOLLARS (R0 IN PENIES)                                               
         SR    R1,R1                                                            
         SRDA  R0,31               THIS WILL PUT 2X DOLLARS IN R0 & R1          
         D     R0,=F'100'                                                       
         LTR   R1,R1               SEE IF RESULT IS NEG                         
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'            NOT AN LA!!!                                 
         SRA   R1,1                DIVIDE RESYLT BY 2                           
         LR    R4,R1               ROUNDED ANSWER IN R4                         
*                                                                               
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   CA13                                                             
         GOTO1 EXSPTLEN,DMCB,(C'D',0),(R3),(R4)                                 
                                                                                
CA13     DS    0H                                                               
         GOTO1 ROLLER,DMCB,3,ACCUMC,(R4),2,(R3)   DOLLARS FOR K/SUMMARY         
         GOTO1 (RF),(R1),3,ACCUMC,(R4),6,(R3)     DOLLARS FOR ADV               
         GOTO1 (RF),(R1),3,ACCUMC,(R4),8,(R3)     DOLLARS FOR CAT               
*                                                                               
NEXTBUC  DS    0H                                                               
         LA    R2,14(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   CA10                                                             
*                                                                               
* PICK UP SPOT LENGTHS                                                          
CA15     DS    0H                                                               
         OR    R3,R3               IF R3=0, NO BUX FELL WITHIN REQ PER          
         BZ    CA70                                                             
         MVC   DUB2,SPACES                                                      
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,DUB2),ALIGN=LEFT                                     
         LR    RE,R0                                                            
         LR    RF,R0               RF = SPACE NEEDED FOR OUTPUT                 
         LA    RE,DUB2(RE)                                                      
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    *+12                                                             
         MVI   0(RE),C'M'                                                       
         LA    RF,1(RF)            ADD 1 TO OUTPUT LENGTH FOR M                 
         LA    RF,1(RF)            AND 1 FOR COMMA                              
*                                                                               
         SR    R3,R3                                                            
         LA    R3,SPTLENS          A(OUTPUT AREA)                               
         SR    R4,R4               COUNTER OF SPACES USED                       
CA20     CLI   0(R3),C' '          LOOK FOR 1ST BLANK SPACE                     
         BE    CA30                                                             
* SEE IF THIS NUMBER ALREADY IN TABLE                                           
         LR    RE,RF                                                            
         BCTR  RE,0                FOR COMMA                                    
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),DUB2                                                     
         BE    CA50                                                             
*                                                                               
         CLI   0(R3),C'*'          NO MORE ROOM IF *                            
         BE    CA50                                                             
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'11'                                                        
         BNE   CA20                                                             
*                                                                               
CA30     AR    RF,R4               IF SPACES USED + SPACE NEEDED > 11           
         CH    RF,=H'11'            THEN NO MORE ROOM                           
         BNH   CA40                                                             
         BCTR  R3,0                BACK UP TO LAST LEN, AND PUT *               
         CLI   0(R3),C','                                                       
         BE    *+12                                                             
         MVI   0(R3),C' '                                                       
         B     *-14                                                             
         MVI   0(R3),C'*'                                                       
         B     CA50                                                             
*                                                                               
CA40     DS    0H                                                               
         SR    RE,RE                                                            
         LA    RE,SPTLENS                                                       
         CR    RE,R3               CHECK IF FIRST SPOT LEN...                   
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         SR    RF,R4               LENGTH OF NEW DATA                           
         BCTR  RF,0                DEC 1 (COMMA ALREADY OUT)                    
         BCTR  RF,0                DEC 1 FOR EX                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DUB2                                                     
*                                                                               
* COUNT NUMBER OF WEEKS THAT BUY IS IN REQ PERIOD                               
CA50     DS    0H                                                               
* CONVERT BUY START & END DATES INTO EBCDIC                                     
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO BUY EFFECTIVE DATE ELEM                   
         USING RBUYDTEL,R6                                                      
CA55     DS    0H                                                               
         MVC   DATE,REQSTBC                                                     
         GOTO1 ADDAY,DMCB,DATE,EDATE,6                                          
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,BUYSTDT)                             
         GOTO1 (RF),(R1),(3,RBUYDTED),(0,BUYEDDT)                               
         DROP  R6                                                               
         LA    R4,PERTAB                                                        
         GOTO1 EXSPTLEN,DMCB,(C'P',0),0,0                                       
*                                                                               
* R5 POINTS TO CORRESPONDING OFFSET INTO PERTABSL                               
*                                                                               
CKWEEK   DS    0H                                                               
         CLC   DATE,BUYSTDT                                                     
         BNL   *+18                                                             
         CLC   EDATE,BUYSTDT                                                    
         BNL   *+18                                                             
         B     NEXTDT                                                           
*                                                                               
         CLC   DATE,BUYEDDT                                                     
         BH    CA60                                                             
         MVI   0(R4),1                                                          
         MVI   0(R5),1                                                          
*                                                                               
NEXTDT   DS    0H                                                               
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         GOTO1 ADDAY,DMCB,DATE,DATE,7                                           
         GOTO1 ADDAY,DMCB,DATE,EDATE,6                                          
         CLC   DATE,REQEDBC+6    BC MON END                                     
         BH    CA60                                                             
         CLC   DATE,BUYEDDT                                                     
         BH    CA60                                                             
         B     CKWEEK                                                           
*                                                                               
* GET NEXT BUY EFFECTIVE DATE ELEM                                              
CA60     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    CA55                                                             
* NOW COUNT NUMBER OF 1 WEEK PERIODS                                            
         LA    R0,53                                                            
         LA    R4,PERTAB                                                        
         SR    R1,R1                                                            
*                                                                               
         CLI   0(R4),1                                                          
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         STC   R1,WEEKS                                                         
*                                                                               
         CLI   QR30SPLN,C'Y'       BREAK OUT SPOT LENGTHS?                      
         BNE   CA70                                                             
         GOTO1 EXSPTLEN,DMCB,(C'P',0),0,0                                       
*                                                                               
* R5 POINTS TO CORRESPONDING OFFSET INTO PERTABSL                               
*                                                                               
* NOW COUNT NUMBER OF 1 WEEK PERIODS                                            
         LA    R0,53                                                            
         SR    RF,RF                                                            
*                                                                               
         CLI   0(R5),1                                                          
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         GOTO1 EXSPTLEN,DMCB,(C'W',0),0,(RF)                                    
*                                                                               
CA70     DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* EXSPTLEN: BREAKS OUT INDIVIDUAL SPOT LENGTHS AND KEEPS TRACK OF     *         
*           INDIVIDUAL SPOTS AND DOLLARS                              *         
*                                                                     *         
* P1 = BYTE 1 : P = GET PERTABSL FOR CORRESPONDING SPOT LENGTH TABLE  *         
*                   RETURNS POINTER IN R5                             *         
*               W = RECORD # WEEKS FOR THIS SPOT LENGTH               *         
*               D = ADD P3 TO DOLLAR ACCUMULATOR                      *         
*               S = ADD P3 TO SPOTS ACCUMULATOR                       *         
*                                                                     *         
* P2 = COLUMN IN ACCUMULATOR                                          *         
*                                                                     *         
* P3 = # SPOTS OR DOLLARS (SEE P1)                                    *         
*                                                                     *         
***********************************************************************         
EXSPTLEN NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         L     R3,4(R1)            COLUMN                                       
         L     R4,8(R1)            # OF SPOTS                                   
                                                                                
         LA    R8,2                POINT TO DOLLAR ROW FIRST                    
         LA    R5,SPLNWKS                                                       
         LA    R6,PERTABSL         UP TO 4 TABLES OF CL53 EACH                  
                                                                                
EXSL10   DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BNE   EXSL20                                                           
         MVC   0(2,R5),RBUYDUR                                                  
         B     EXSL100                                                          
                                                                                
EXSL20   DS    0H                                                               
         CLC   RBUYDUR,0(R5)                                                    
         BE    EXSL100                                                          
         CH    R8,=H'8'                                                         
         BL    EXSL30                                                           
         MVI   0(R5),C'*'          MORE THAN ONE SPOT LENGTH IN THE 4TH         
         B     EXSL100             SPOT LENGTH ACCUMULATOR                      
                                                                                
EXSL30   DS    0H                                                               
         LA    R5,3(R5)                                                         
         LA    R8,2(R8)                                                         
         LA    R6,L'PERTABSL(R6)                                                
         B     EXSL10                                                           
                                                                                
EXSL100  DS    0H                                                               
         CLI   BYTE,C'P'                                                        
         BNE   EXSL110                                                          
         LR    R5,R6                                                            
         XIT1  REGS=(R5)           PASS BACK POINTER TO APPROPRIATE             
*                                  PERTAB TABLE                                 
EXSL110  DS    0H                                                               
         CLI   BYTE,C'W'                                                        
         BNE   EXSL120                                                          
         STC   R4,2(R5)                                                         
         B     EXSLX                                                            
                                                                                
EXSL120  DS    0H                                                               
         CLI   BYTE,C'D'           # SPOTS OR DOLLARS?                          
         BE    *+6                                                              
         BCTR  R8,0                DECREMENT TO #SPOT ROW                       
                                                                                
* ADD SPOTS OR DOLLARS TO THIS SPOT LENGTH IN ACCUMULATOR                       
         GOTO1 ROLLER,DMCB,3,ACCUMSL,(R4),(R8),(R3)                             
                                                                                
EXSLX    DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* PRTSPLN: PRINT DETAIL LINES 1-4 (AT LEAST 1 & 2) FOR EACH SPOT LEN  *         
* NOTE THAT ONLY MAX OF 4 SPOT LENGTH BREAK OUTS ARE SUPPORTED. IF    *         
* THERE ARE MORE THAN 4, THE 4TH AND SUBSEQUENT SPOT LENGTHS WILL BE  *         
* LUMPED TOGETHER AS '*'                                              *         
*                                                                     *         
***********************************************************************         
         DS    0H                                                               
PRTSPLN  NMOD1 0,*PRTSPL*                                                       
         L     RC,0(R1)                                                         
                                                                                
         LA    R8,1                                                             
         LA    R3,SPLNWKS          2-BYTE SPOT LENGTH, 1-BYTE # WEEKS           
*                                                                               
PSL10    DS    0H                                                               
         ZIC   R4,LINE                                                          
         L     RF,ABOX             A(BOX DSECT)                                 
         USING BOXD,RF                                                          
         LA    R4,BOXROWS-1(R4)                                                 
         DROP  RF                                                               
         CLI   LINE,46                                                          
         BL    PSL15                                                            
         MVI   0(R4),C'B'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         MVI   LINE,99                                                          
         B     PSL18                                                            
                                                                                
PSL15    DS    0H                                                               
         MVI   0(R4),C'M'                                                       
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
*                                                                               
* DETAIL LINE 1 (ALWAYS)                                                        
*                                                                               
PSL18    DS    0H                                                               
         MVC   DUB2,SPACES         CHECK IF MORE THAN 4 SPOT LENGTHS            
         CLI   0(R3),C'*'                                                       
         BNE   PSL20                                                            
         MVI   DUB2,C'*'                                                        
         B     PSL30                                                            
                                                                                
PSL20    DS    0H                                                               
         MVC   HALF,0(R3)                                                       
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,DUB2),ALIGN=LEFT                                     
         LR    RE,R0                                                            
         LA    RE,DUB2(RE)                                                      
         TM    0(R3),X'80'         MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
                                                                                
PSL30    DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BE    PSL40                                                            
         MVC   P+31(4),DUB2        SPOT LENS FROM BUYS                          
         B     *+10                                                             
PSL40    MVC   P+22(4),DUB2        SPOT LENS FROM BUYS                          
*                                                                               
* CROSS-CAST #SPOTS LINE TO GET TOTAL SPOTS & GET TOTAL SPOTS                   
*                                                                               
         GOTO1 ROLLER,DMCB,5,ACCUMSL,(R8)                                       
         GOTO1 (RF),(R1),1,ACCUMSL,(R8)                                         
         L     RE,0(R1)                                                         
         LA    RE,(ACNMSLCL-1)*4(RE)      TOTAL COL                             
         ICM   R0,15,0(RE)                                                      
         SR    R1,R1                                                            
         SRDA  R0,31               2X #SPOTS IN R0 & R1                         
         ZIC   RF,2(R3)                                                         
         DR    R0,RF                                                            
         LTR   R1,R1               SEE IF NEGATIVE (SHOULDN'T BE)               
         BM    *+8                 IF SO, DON'T NEED TO ADD 1                   
         AH    R1,=H'1'                                                         
         SRA   R1,1                DIVIDE BY 2                                  
         EDIT  (R1),(3,P+43)       # SPOTS                                      
*                                                                               
* DETAIL LINE 2 (ALWAYS)                                                        
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   PSL50               NO #WEEKS FOR SUMMARY                        
         ZIC   R1,2(R3)                                                         
         LA    R2,PSECOND                                                       
         EDIT  (R1),(3,43(R2))     # WEEKS                                      
                                                                                
PSL50    DS    0H                                                               
         LA    R2,P                                                             
         CLI   QOPTION2,C'D'       DOLLARS ONLY?                                
         BE    PSL60                                                            
         CLI   QOPTION2,C'N'       NEITHER (DOLLARS/SPOTS)                      
         BE    PSL60                                                            
*                                  PRINT SPOT COUNTERS                          
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMSL                                   
         LA    R2,PSECOND                                                       
*                                                                               
* CHECK ON % WANTED...                                                          
PSL60    DS    0H                                                               
         CLI   QOPTION3,C'D'       PERCENTS ON DOLLARS ONLY?                    
         BE    PSL70                                                            
         CLI   QOPTION3,C'N'       NO PERCENTS?                                 
         BE    PSL70                                                            
*                                                                               
* READ SPOT LINE, DIVIDE DAYPART SPOTS BY TOTAL SPOTS TO GET %.                 
         GOTO1 ROLLER,DMCB,1,ACCUMSL,(R8)                                       
         L     RF,0(R1)            A(LINE)                                      
         GOTO1 PRTPCT,DMCB,46(R2),(RF),SPOTS                                    
*                                                                               
* FIND OUT WHICH LINE TO USE                                                    
PSL70    DS    0H                                                               
         CLC   P+47(L'P-47),SPACES                                              
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     PSL80                                                            
         CLC   PSECOND+47(L'PSECOND-47),SPACES                                  
         BNE   *+12                                                             
         LA    R2,PSECOND                                                       
         B     PSL80                                                            
         LA    R2,PTHIRD                                                        
PSL80    DS    0H                                                               
         LA    R8,1(R8)                                                         
         CLI   QOPTION2,C'S'       SPOTS ONLY?                                  
         BE    PSL90                                                            
         CLI   QOPTION2,C'N'       NEITHER (DOLLARS OR SPOTS)                   
         BE    PSL90                                                            
*                                  PRINT DOLLAR COUNTERS                        
         GOTO1 ROLLOUT,DMCB,(R2),(R8),ACCUMSL                                   
*                                                                               
* CHECK ON % WANTED...                                                          
*                                                                               
PSL90    DS    0H                                                               
         CLI   QOPTION3,C'S'       PERCENTS ON SPOTS ONLY?                      
         BE    PSL110                                                           
         CLI   QOPTION3,C'N'       NO PERCENTS?                                 
         BE    PSL110                                                           
*                                                                               
* FIND OUT WHICH LINE TO USE                                                    
*                                                                               
         CLC   P+47(L'P-47),SPACES                                              
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     PSL100                                                           
         CLC   PSECOND+47(L'PSECOND-47),SPACES                                  
         BNE   *+12                                                             
         LA    R2,PSECOND                                                       
         B     PSL100                                                           
         CLC   PTHIRD+47(L'PTHIRD-47),SPACES                                    
         BNE   *+12                                                             
         LA    R2,PTHIRD                                                        
         B     PSL100                                                           
         LA    R2,PFOURTH                                                       
*                                                                               
* READ DOLLAR LINE, DIVIDE DAYPART DOLLARS BY TOTAL DOLLARS TO GET %.           
*                                                                               
PSL100   DS    0H                                                               
         GOTO1 ROLLER,DMCB,1,ACCUMSL,(R8)                                       
         L     RF,0(R1)            A(LINE)                                      
         GOTO1 PRTPCT,DMCB,46(R2),(RF),DOLLARS                                  
                                                                                
PSL110   GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         CLI   QOPTION2,C'A'       INCLUDE COST-PER-SPOT?                       
         BNE   PSL130                                                           
         LA    R2,P                                                             
         BCTR  R8,0                                                             
PSL120   LR    R7,R8                                                            
         GOTO1 COSTSPOT,DMCB,1,ACCUMSL DO WITH PENNIES                          
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
         LA    R8,1(R8)                                                         
                                                                                
PSL130   EQU   *                                                                
         LA    R8,1(R8)                                                         
         CH    R8,=H'8'                                                         
         BH    PSLX                                                             
         LA    R3,3(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   PSL10                                                            
                                                                                
PSLX     DS    0H                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REGENRDA                                                       
         EJECT                                                                  
*                                                                               
SORTD    DSECT                                                                  
*                                                                               
SCAT     DS    CL2                 CATEGORY CODE                                
SADVNM   DS    CL20                EXPANDED ADVERTISER NAME                     
SPRDNM   DS    CL20                EXPANDED PRODUCT NAME                        
SAGYNM   DS    CL20                EXPANDED AGY NAME                            
SSTANM   DS    CL5                 STATION NAME                                 
SCONNUM  DS    CL4                 CONTRACT NUMBER (PACKED)                     
         DS    CL1                 SPARE FOR ALIGNMENT                          
SKEYLEN  EQU   *-SORTD                                                          
*                                                                               
* DATA IN SORT RECORD                                                           
SRCATNM  DS    CL30                EXPANDED CATEGORY NAME                       
SRADV    DS    CL4                 ADVERTISER CODE                              
SRPRD    DS    CL3                 PRODUCT CODE                                 
SRAGY    DS    CL4                 AGY CODE                                     
SRSTATZ  DS    CL1                 STATION TIME ZONE                            
SRDATES  DS    CL6                 CONTRACT START-END DATES                     
         DS    CL8                 SPARE FOR ALIGNMENT                          
SRECLEN  EQU   *-SORTD                                                          
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP3002S08/16/02'                                      
         END                                                                    
