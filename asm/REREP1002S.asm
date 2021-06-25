*          DATA SET REREP1002S AT LEVEL 155 AS OF 06/18/02                      
*PHASE RE1002B,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE REGENBUF                                                               
*INCLUDE REGENPBY                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE REGENSTC                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE UNBOOK                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE STXITER                                                                
         PRINT NOGEN                                                            
         TITLE 'REREP1002 - RE1002 - CONTRACT PRINTING MODULE'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP1002 --- CONTRACT OFFLINE PRINTING MODULE             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 15MAY96 RHV HISTORY LOST                                                      
*                                                                   *           
* 15MAY96 RHV CONTYPE RECORD CONTROLLED CONTRACT FORMATTING                     
*         SKU PROPOSAL FLAG CHECK BUG FIX. EXPAND EST NUM.          *           
*                                                                   *           
* 06JUN96 WSB DISP NON-DARE HIATUS INFO IF CON NOT LINKED TO DARE   *           
*         WSB FIX BUG IN HIATUS ROUTINE WHERE 'USING' ON R5 W/O LR  *           
*                                                                   *           
* 12SEP96 RHV SKIP PROCESSING OF DELETED & UNCONFIRMED CONTRACTS    *           
*                                                                   *           
* 08JAN97 RHV REVISE PROCESSING CONTROL OF CONTRACTS                *           
*             ENABLE ORDER SORTING                                  *           
*                                                                   *           
* 05MAY97 RHV SUPPORT 'OTHER' TYPE CONTRACTS                        *           
*                                                                   *           
* 28MAY97 RHV DISPLAY CFC COMMENTS ON CONFIRMATION                  *           
*                                                                   *           
* 20NOV97 BU  PAPERWORK FOR ORDERS WITH NO BUYS.                    *           
*                                                                   *           
* 19DEC97 JRD CARE OF AGENCIES                                      *           
*                                                                   *           
* 20FEB98 JRD 4K CONTRACTS                                          *           
*                                                                   *           
* 29APR98 RHV COVERSHEET                                            *           
*                                                                   *           
* 05JAN99 RHV NON-DELETED CANCELLED BUYS                            *           
*                                                                   *           
* 09SEP99 RHV PRINT $0 CONTRACTS                                    *           
*                                                                   *           
* 17DEC99 MLB ADDING POINT PERSON NAME                              *           
*                                                                   *           
* 06JUL00 BU  SOFTEN 'MISSING S/P RECORD' END                       *           
*                                                                   *           
* 21AUG00 SKU INCREASE COMBO BUFFER AREA                            *           
*                                                                   *           
* 14NOV00 BU  IF COVERSHEET NOT FOUND, SKIP IT                      *           
*                                                                   *           
* 27NOV00 BU  UPGRADE TOTALS FOR TRADE                              *           
*                                                                   *           
* 06JUN02 BU  SOFTEN MISSING PRODUCT RECORD                         *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE1002   CSECT                                                                  
         ENTRY SORTA                                                            
         NMOD1 STOREX-STORED,**RE1002,R7                                        
         USING STORED,RC                                                        
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
*                                                                               
***********************************************************************         
* MODE - RUNFRST - PERFORMS GENERAL MODULE SETUP                      *         
*                - READ REPREC PROFILE BITS                           *         
*                - INITIALIZE SORTER (REGARDLESS OF WHETHER WE        *         
*                  ACTUALLY SORT)                                     *         
***********************************************************************         
         CLI   MODE,RUNFRST        RELOCATE VCONS AT RUNFRST                    
         BNE   REQF00                                                           
*                                                                               
         RELOC                                                                  
         ST    RE,RELO                                                          
         LA    R0,MODULES          COUNTER                                      
         LA    R1,MODTAB           R1 POINTS TO TABLE                           
         L     RF,0(R1)            V(ROUTINE)                                   
         AR    RF,RE               RELOCATE IT                                  
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)            NEXT VCON                                    
         BCT   R0,*-14                                                          
*                                                                               
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   VREPFACS,4(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
         OC    VREPFACS,VREPFACS                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOX (RF),(R1),=CL8'T00AE0',0                                         
         MVC   VDEMOCON,4(R1)      V(DEMOCON)                                   
         OC    VDEMOCON,VDEMOCON                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    R0,=H'4000'         4K COVERSHEET IO AREA                        
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ACOVAREA                                                      
*                                                                               
         LA    RF,SORTA            ESTABLISH COMBO PRINT LIST ADDRESS           
         AH    RF,=H'2000'         BUMP PAST SORTA                              
         ST    RF,ACLIST           THIS IS THE COMBO PRINT LIST AREA            
*                                                                               
         L     RF,=A(BUCKS)        BUCKET AREAS                                 
         A     RF,RELO                                                          
         ST    RF,ABUCKS                                                        
         L     RF,=A(TBUCKS)                                                    
         A     RF,RELO                                                          
         ST    RF,ATBUCKS                                                       
         L     RF,=A(GRAND)                                                     
         A     RF,RELO                                                          
         ST    RF,AGRAND                                                        
         L     RF,=A(TGRAND)                                                    
         A     RF,RELO                                                          
         ST    RF,ATGRAND                                                       
*                                                                               
         MVC   REPID,RREPKREP      SAVE REP ID FOR THIS RUN                     
*                                                                               
* GET REP PROGRAM PROFILES (SPECIFICALLY CONTRACT PROFILES - CAN BE             
* FOUND IN RECNTPROF).                                                          
         XC    PROFDATA,PROFDATA   START CLEAN                                  
         LA    R6,RREPREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   RUNF30              NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
RUNF10   CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    RUNF20                                                           
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,RUNF10                                                        
         B     RUNF30              CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
RUNF20   MVC   PROFDATA,RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
RUNF30   DS    0H                  INITIALIZE SORTER                            
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         XC    REQNUM,REQNUM       ZERO REQUEST NUMBER COUNTER                  
*                                                                               
*                                  PRINT TEST PATTERNS                          
         GOTO1 =A(PATTERN),DMCB,(RC),(RA),(R9),RR=Y                             
         B     CPEXT                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(30,2,A,5,20,A),FORMAT=BI,WORK=1'               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=31'                                    
*                                                                               
***********************************************************************         
* MODE - REQFRST - IF THIS IS A 10 REQUEST, ASSIGN A SEQUENCE NUMBER  *         
*                  TO EACH REQUEST TO PRESERVE THE ORDER OF THE REQ'S *         
***********************************************************************         
REQF00   DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PROC00                                                           
*                                                                               
         CLI   FULL-1,C'Y'         DON'T DUMP OPTION SET?                       
         BE    REQF0100            YES                                          
         MVC   FULL-1(1),QOPTION2  SAVE DUMP OPTION                             
REQF0100 EQU   *                                                                
*                                                                               
*   TEST OPT2 - FIRST END                                                       
*                                                                               
         CLC   QPROG,=C'10'                                                     
         BNE   CPEXT                                                            
*                                                                               
         L     R2,ACLIST           A(COMBO K PRINTED LIST)                      
         MVI   0(R2),X'FF'         INITIALIZE                                   
*                                                                               
         XC    COMBOSC,COMBOSC                                                  
*                                                                               
         LH    R2,REQNUM                                                        
         LA    R2,1(R2)                                                         
         STH   R2,REQNUM                                                        
         B     CPEXT                                                            
*                                                                               
***********************************************************************         
* MODE - PROCCONT                                                     *         
*  RECEIVE CONTRACT FROM REPORTER AND PASS K NUMBER & MARKET NAME TO  *         
*  SORTER                                                             *         
***********************************************************************         
PROC00   DS    0H                                                               
         CLI   MODE,PROCCONT                                                    
         BNE   RUNL00                                                           
*                                                                               
PROC0010 EQU   *                                                                
         MVI   HALF2,0             CLEAR 'TRADE' FLAG                           
*                                     DON'T STEP ON HALF2/BYTE1                 
*                                                                               
         TM    RCONCNTL,X'80'           CONTRACT DELETED?                       
         BO    CPEXT                    SKIP IT                                 
*                                                                               
* FOR R10'S MAKE SURE WE DON'T DOUBLE PRINT COMBO MEMBERS                       
*                                                                               
         CLC   QPROG,=C'10'        10 REQUEST?                                  
         BNE   CP08                                                             
         BAS   RE,PREVCMB          COMBO PREVIOUSLY PROCESSED?                  
         BE    CPEXT               YES - DON'T PROCESS AGAIN                    
*                                                                               
CP08     DS    0H                                                               
         XC    WORK,WORK           BUILT SORT RECORD IN WORK                    
         MVC   WORK(L'RCONKCON),RCONKCON    PASS K NUMBER                       
         MVC   WORK+24(2),QREQOFF           PASS REQUESTING OFFICE              
         MVC   WORK+26(2),QPROG             PASS PROGRAM REQUESTED              
         MVC   WORK+28(1),QOPTION1          PASS OPTION1                        
         MVC   WORK+29(2),REQNUM            PASS REQUEST SEQUENCE NUM           
*                                                                               
         CLC   QPROG,=C'11'                 TURNAROUND REQUEST?                 
         BNE   CP10                                                             
         TM    PROFILES+CNTSRT1B,CNTSRT1A   SORT TURNAROUNDS?                   
         BO    CP20                         YES - PASS MKT NAME                 
         B     CP30                                                             
*                                                                               
CP10     DS    0H                                                               
         CLC   QPROG,=C'10'                 TURNAROUND REQUEST?                 
         BE    *+6                                                              
         DC    H'0'                         HAS TO BE 10 OR 11 REQ!             
         TM    PROFILES+CNTSRT0B,CNTSRT0A   SORT CONTRACTS?                     
         BZ    CP30                         NO - SKIP PASSING MKT NAME          
*                                                                               
CP20     DS    0H                                                               
         XC    KEY,KEY                      READ STATION RECORD                 
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPID                                                  
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         MVC   WORK+4(L'RSTAMKT),RSTAMKT   MARKET NAME TO SORTER                
         DROP  R6                                                               
*                                                                               
         MVC   KEY,RCONKEY         RESTORE SEQ LOOP FOR REPORTER                
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP30     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',WORK                                     
         B     CPEXT                                                            
***********************************************************************         
* MODE - RUNLAST                                                      *         
***********************************************************************         
* THIS IS A VERY IMPORTANT ROUTINE! IT GETS THE SORTED (OPTIONALLY)   *         
* CONTRACT NUMBERS BACK FROM SORTER AND THEN MIMICS HOW REPORTER      *         
* USED TO CONTROL THIS MODULE BY SETTING THE MODE, AND CALLING THE    *         
* REST OF THIS MODULE. THIS ROUTINE IS THE CONTROL FOR THE REST OF    *         
* THE MODULE. THE VARIOUS MODE SETTINGS THAT ARE USED ARE CONTROLLED  *         
* HERE.                                                               *         
***********************************************************************         
RUNL00   DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   RUNL100                                                          
*                                                                               
RUNL20   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'   GET NEXT CONTRACT FROM SORTER          
         OC    DMCB+4(4),DMCB+4          NO MORE CONTRACTS?                     
         BZ    CPEXT                     ALL DONE - EXIT                        
*                                                                               
         ZICM  R2,DMCB+4,4                                                      
         ZICM  R3,0(R2),4                GET K NUMBER                           
         MVC   REQOFF,24(R2)       GET REQUESTING OFFICE                        
         MVC   PROG,26(R2)         GET PROGRAM REQUESTED                        
         MVC   OPTION1,28(R2)      GET OPTION 1 (AGY COPY?)                     
*                                                                               
* LOOKUP CONTRACT REC                                                           
*                                                                               
         L     R1,=X'99999999'           GET 9'S COMPLIMENT OF K NUMBER         
         SR    R1,R3                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),RREPKREP                                               
         STCM  R1,15,KEY+23                                                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RCONREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         TM    RCONMODR+1,X'80'+X'40'  ACE OR GRAPHNET?                         
         BZ    RUNL25                  NO - SKIP CONFIRM CHECK                  
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   RUNL20                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'           CONFIRMED NOW?                          
         BZ    RUNL20                   NO - SKIP IT                            
         DROP  R6                                                               
*                                                                               
RUNL25   MVI   HAVEBUYS,0          CLEAR HAVE BUYS FLAG                         
*                                                                               
         TM    RCONMODR+1,X'20'      MON DATA?                                  
         BO    RUNL60                YES - SKIP BUY READS                       
*                                                                               
* READ FIRST BUY                                                                
*                                                                               
RUNL30   XC    KEY,KEY                 BUILD BUYREC KEY                         
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),RREPKREP                                               
         MVC   FULL,RCONKCON           GET 9'S COMP REVERSED                    
         L     R0,=X'99999999'                                                  
         S     R0,FULL                                                          
         STCM  R0,15,FULL                                                       
         PACK  KEY+18(1),FULL+3(1)                                              
         PACK  KEY+19(1),FULL+2(1)                                              
         PACK  KEY+20(1),FULL+1(1)                                              
         PACK  KEY+21(1),FULL+0(1)                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(22),KEYSAVE                                                  
         BNE   RUNL60              ***> ALWAYS PRINT CONTRACT ***>              
         OI    HAVEBUYS,X'80'       SET FLAG - HAVE AT LEAST 1 BUY              
        GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,RBUYREC,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
RUNL60   DS    0H                                                               
         XC    CMBSTAT,CMBSTAT     CLEAR COMBO FLAG AT FIRST PASS               
         XC    COMBONUM,COMBONUM                                                
         GOTO1 =A(CHKCOMBO),DMCB,(RC),(RA),(R9),RR=Y                            
*                                                                               
* READ STATION, AGENCY, & ADVERTISER RECS                                       
*                                                                               
         MVC   SVBUYKEY,RBUYREC    SO WE CAN RESTORE BUY READ ORDER             
         MVI   OPT2A,0             SET 'PRODUCT NOT FOUND' FLAG                 
         GOTO1 =A(READRECS),DMCB,(RC),RR=Y                                      
*                                                                               
         MVI   MODE,PROCCONT            SET MODE = PROCCONT                     
         BAS   RE,DOCONF                CALL REST OF MODULE                     
*                                                                               
         MVI   MODE,CONTFRST                                                    
         BAS   RE,DOCONF                                                        
*                                                                               
         TM    HAVEBUYS,X'80'      HAVE ANY BUYS?                               
         BZ    RUNL90              NO - SKIP LOOP & SEQ RESTORE                 
*                                                                               
         MVC   KEY,SVBUYKEY        RE-ESTABLISH SEQ ORDER                       
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
* LOOP & READ REST OF BUYRECS FOR THE CURRENT CONTRACT                          
*                                                                               
RUNL70   DS    0H                                                               
         MVI   MODE,PROCBUY                                                     
         BAS   RE,DOCONF                                                        
*                                                                               
         TM    CMBSTAT,CMBDONE     IF DONE FLAG SET                             
         BO    RUNL20              EXIT!                                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(22),KEYSAVE                                                  
         BNE   RUNL90              NEXT CONTRACT                                
         GOTO1 (RF),(R1),(X'08',GETREC),REPFILE,KEY+28,RBUYREC,DMWORK           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         B     RUNL70              BACK FOR NEXT BUY                            
*                                                                               
RUNL90   DS    0H                                                               
         XC    SPOTS,SPOTS                                                      
         MVI   MODE,CONTLAST                                                    
         BAS   RE,DOCONF                                                        
         B     RUNL20              BACK FOR NEXT CONTRACT                       
***********************************************************************         
* NO OTHER REPORTER MODES ARE SUPPORTED IN THIS PROGRAM               *         
***********************************************************************         
RUNL100  DS    0H                                                               
         B     CPEXT                                                            
***********************************************************************         
* DOCONF - DO CONFIRMATION - MAIN REPORT GENERATING ROUTINE           *         
***********************************************************************         
* THIS ROUTINE IS WHAT'S LEFT OF THE ORIGINAL MODULE. BE AWARE....    *         
* THE ONLY WAY ANY CODE FROM HERE DOWN GETS CALLED IS WHEN 'RUNL'     *         
* ROUTINE MAKES A CALL TO HERE. THE MODES THAT ARE RESPONDED TO FROM  *         
* NOW ON ARE SET BY 'RUNL' - NOT BY REPORTER.                         *         
***********************************************************************         
DOCONF   NTR1                                                                   
         CLI   MODE,PROCCONT                                                    
         BNE   CP150                                                            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   MAXLINES,80                                                      
         L     R1,ABUCKS                                                        
         XC    0(L'BUCKS,R1),0(R1)                                              
         MVC   0(2,R1),=H'2'                                                    
         L     R1,ATBUCKS                                                       
         XC    0(L'TBUCKS,R1),0(R1)                                             
         MVC   0(2,R1),=H'2'                                                    
         XC    FLTFLDS,FLTFLDS                                                  
         MVC   POINTER,VSORTA      INIT PTR TO FLIGHT ENTRIES                   
         MVC   SVCONNUM,RCONKCON                                                
*                                                                               
         GOTO1 =A(FMT),DMCB,(RC),(RA),(R9),RR=YES                               
         B     CPEXT                                                            
*                                                                               
CP150    DS    0H                  CHECK IF CONTRACT IS A COMBO ORDER           
         CLI   MODE,PROCBUY                                                     
         BNE   CP170                                                            
*                                                                               
         TM    CMBSTAT,CMBDONE     IF DONE FLAG SET                             
         BO    CPEXT               EXIT!                                        
*                                                                               
CP160    DS    0H                                                               
         OC    COMBONUM,COMBONUM                                                
         BZ    CP180                                                            
*                                                                               
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
*                                                                               
         OI    CMBSTAT,CMBCMT      PRINT COMBO CONTRACT COMMENT                 
         GOTO1 =A(BUYPRT),DMCB,(RC),(RA),(R9),RR=Y                              
         NI    CMBSTAT,X'FF'-CMBCMT                                             
*                                                                               
         GOTO1 =A(PCOMBO),DMCB,(RC),(RA),(R9),RR=Y                              
         B     CPEXT                                                            
*                                                                               
CP170    DS    0H                                                               
         CLI   MODE,CONTFRST       CHECK IF COMBO MON ORDER                     
         BNE   CP180                                                            
         TM    RCONMODR+1,X'20'                                                 
         BO    CP160                                                            
*                                                                               
CP180    DS    0H                                                               
         TM    CMBSTAT,CMBDONE     IF DONE FLAG SET                             
         BO    CPEXT               EXIT!                                        
         GOTO1 =A(BUYPRT),DMCB,(RC),(RA),(R9),RR=Y                              
CPEXT    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FORCE EVERYTHING FROM HERE DOWN TO BE OFF 2ND BASE REG.                       
***********************************************************************         
         DS    XL2100                                                           
*                                                                               
***********************************************************************         
* ROUTINE TO PRINT CONTRACT COMMENTS                                            
***********************************************************************         
PRTKCMT  NTR1                                                                   
* HARD CODED COMMENTS                                                           
         MVC   SAVKEY,KEY          SAVE KEY TO KEEP CONTRLR SEQUENCE            
         GOTO1 =A(PRTCOV),DMCB,(RC),RR=Y                                        
         BAS   RE,PRTEICDS         PRINT EI CODES                               
                                                                                
         MVI   STASW,FALSE         SET SWITCH FOR CON COMM. LITERAL             
*                                                                               
         GOTO1 =A(PRTCFC),DMCB,(RC),RR=Y  PRINT CFC COMMENT                     
*                                                                               
PKCMT10  DS    0H                  PRINT STANDARD COMMENT ATTACHED TO           
         CLI   RCONTYPE,0          TYPE, IF ANY                                 
         BE    STACMT                                                           
         GOTO1 =A(TYPECMT),DMCB,(RC),(RA),(R9),RR=Y                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* STATION COMMENTS                                                              
***********************************************************************         
STACMT   DS    0H                                                               
*                                                                               
         MVI   STALIAB,0           LOOK FOR STATION LIABILITY POSITION          
         LA    R6,RSTAREC                                                       
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   STACMT10                                                         
         MVC   STALIAB,RSTALIAB                                                 
         DROP  R6                                                               
*                                                                               
STACMT10 LA    R6,RSTAREC                                                       
         USING RSTACEL,R6                                                       
         MVI   ELCODE,3            LOOK FOR STATION COMMENT                     
         BAS   RE,GETEL                                                         
         BNE   KCMT                                                             
*                                                                               
         CLI   RSTACTYP,C'L'       LIBRARY REFERENCE                            
         BE    STACMT30                                                         
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
STACMT20 XC    WORK,WORK           STATION STORED COMMENTS                      
         ZIC   R1,RSTACLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RSTACCMT                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,WORK),(RC),(RA),(R9),RR=Y                     
         B     KCMT                PROCESS CONTRACT COMMENTS                    
*                                                                               
STACMT30 DS    0H                  LIBRARY REFERENCE LOGIC                      
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD AN INVENTORY KEY                       
         USING RINVKEY,R4                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,RCONKREP                                                
         MVC   RINVKSTA,=CL5'ZZZZZ'                                             
         MVI   RINVKSRC,X'FF'                                                   
         MVC   RINVKTXT,RSTACNUM                                                
         DROP  R4                                                               
*                                                                               
         MVC   TEXTKEY,KEY         SAVE KEY FOR LATER COMPARE                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),TEXTKEY                                                  
         BNE   KCMT                DID NOT FIND LIBRARY TEXT                    
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         USING RINVTEL,R6          POINT TO FIRST TEXT ELEMENT                  
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   KCMT                                                             
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
STACMT40 LA    R2,12               COUNTER                                      
*                                                                               
STACMT50 DS    0H                  LOOP THROUGH TEXT ELEMENTS                   
         CLI   RINVTLEN,7                                                       
         BL    STACMT60                                                         
         ZIC   R1,RINVTLEN         ELEMENT LENGTH                               
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),RINVTEXT                                                 
         BAS   RE,PRTREP                                                        
*                                                                               
STACMT60 BAS   RE,NEXTEL                                                        
         BNE   KCMT                                                             
         BCT   R2,STACMT50                                                      
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CONTRACT COMMENTS                                                             
***********************************************************************         
KCMT     LA    R6,RCONREC                                                       
*                                                                               
         MVI   ELCODE,X'25'        ANY AGENCY HIATUS DATES?                     
         BAS   RE,GETEL                                                         
         BE    KCMT20              YES, PRINT HEADER                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'        ANY HIATUS COMMENTS?                         
         BAS   RE,GETEL                                                         
         BNE   KCMT50              NO, NO HIATUS INFO                           
                                                                                
KCMT20   DS    0H                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   KCMT45              NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      YES, IS CONTR LINKED TO AGY ORDER?           
         BNO   KCMT45              NO, CONTINUE                                 
         DROP  R6                                                               
*                                                                               
*                                  PRINT HIATUS DATES                           
         GOTO1 =A(HIATUS),DMCB,(RC),RR=Y                                        
         B     KCMT50                                                           
*                                                                               
KCMT45   DS    0H                                                               
         BAS   RE,NODAHIAT         PRINT NON-DARE HIATUS INFO                   
*                                                                               
KCMT50   DS    0H                                                               
         LA    R6,RCONREC                                                       
         USING RCONCMEL,R6                                                      
*                                                                               
         MVI   ELCODE,2            ANY CONTRACT COMMENTS??                      
         BAS   RE,GETEL                                                         
         BNE   AGYCMT                                                           
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                  FIRST COMMENT LINE                           
         GOTO1 =A(PSTCMT),DMCB,(2,RCONCMEL),(RC),(RA),(R9),RR=Y                 
*                                                                               
         BAS   RE,NEXTEL           BUMP FORWARD TO GET NEXT COMMENT             
         BNE   AGYCMT                                                           
*                                  SECOND COMMENT LINE                          
         GOTO1 =A(PSTCMT),DMCB,(2,RCONCMEL),(RC),(RA),(R9),RR=Y                 
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* AGENCY COMMENTS                                                               
*********************************************************************           
AGYCMT   DS    0H                                                               
         GOTO1 =A(DOAGYCMT),DMCB,(RC),RR=Y                                      
*                                                                               
***********************************************************************         
* STATION LIABILITY COMMENTS                                                    
***********************************************************************         
SLCMT    CLI   STALIAB,0                                                        
         BE    SLCMT10                                                          
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(4,STALIAB),(RC),(RA),(R9),RR=Y                  
*                                                                               
SLCMT10  MVC   KEY,SAVKEY                                                       
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'        READ AGAIN TO RE-ESTABLISH                   
         BZ    XIT                 SEQUENTIAL READ IN CONTROLLER                
         DC    H'0'                                                             
         EJECT                                                                  
*********************************************************************           
* PRINT NON-DARE HIATUS DATES AND COMMENTS IF ANY                               
*********************************************************************           
* TAKEN FROM RECNT17                                                            
NODAHIAT NTR1                                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2   CLEAR OUT WORK2                       
*                                                                               
*              PRINT EFFECTIVE DATES                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        HIATUS DATES ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   NDHI205             PRINT COMMENTS                               
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+13(16),=C'*HIATUS DATE(S)*'                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,WORK2            OUTPUT                                       
         LA    R5,WORK2+60         OUTPUT END                                   
         ZIC   R4,1(R6)                                                         
         AR    R4,R6               R4 = ELEMENT END                             
         LA    R6,2(R6)            R6 = POSITION IN ELEMENT                     
*                                                                               
PRINDTES LA    R3,WORK+20          BUILD AREA                                   
*              PRINT DATES                                                      
* 3 BYTE DATE ENTRIES: 2 BYTE COMPRESSED START DATE, 1 BYTE NUM OF              
* DAYS THAT HIATUS LASTS FROM START DATE                                        
         GOTO1 DATCON,DMCB,(2,(R6)),(4,(R3))     START DATE                     
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI100                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI100  AR    R3,RE                                                            
*                                                                               
         CLI   2(R6),0             NON-ZERO NUM OF DAYS FROM START DAT?         
         BE    NDHI160             NO                                           
*                                                                               
         MVI   0(R3),C'-'          YES, THERE ARE SOME DAYS                     
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R6)),WORK2+200   START DATE FOR ADDAY            
         ZIC   RE,2(R6)                         GET NUMBER OF DAYS              
         ST    RE,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK2+200,WORK2+206,  END DATE IN ADDAY FORM          
*                                                                               
         GOTO1 DATCON,DMCB,WORK2+206,(4,(R3))   END DATE IN PRINT FORM          
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI120                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI120  AR    R3,RE                                                            
*                                                                               
NDHI160  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM PRINT LEN                           
         LR    RF,R2                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R5               WORK2+60                                     
         BNH   NDHI164                                                          
* FIRST LINE EXCEEDED - START AT SECOND LINE                                    
         LA    R5,500(R5)          ELIM. FIRST TEST                             
         LA    R2,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,0          DELIMITER?                                   
         BNE   NDHI164                                                          
         LA    R2,1(R2)                                                         
*                                                                               
NDHI164  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK+20                                                  
         MVC   WORK+20(20),SPACES                                               
         LA    R2,1(R3,R2)         OUTPUT PTR                                   
*                                                                               
         LA    RE,WORK2+170                                                     
         CR    R2,RE               SECOND LINE EXCEEDED?                        
         BNH   *+12                                                             
         MVI   WORK2+169,C'>'      DOESN'T FIT                                  
         B     NDHI200                                                          
*                                                                               
         LA    R6,3(R6)                                                         
         CR    R6,R4               END OF ELEMENT?                              
         BNL   NDHI200             YES                                          
*                                                                               
         MVI   0(R2),0             DELIMITER                                    
         LA    R2,1(R2)                                                         
         B     PRINDTES                                                         
*                                                                               
NDHI200  MVC   P+13(60),WORK2       DATES                                       
         GOTO1 REPORT                                                           
         MVC   P+13(110),WORK2+60  MOVE 2D LINE (PLUS EXTRA FOR OVERFL)         
         GOTO1 REPORT                                                           
*                                                                               
* PRINT COMMENTS                                                                
NDHI205  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL            ANY COMMENT ELEMENTS?                        
         BNE   NDHIX               NO, GO OUT                                   
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+13(19),=C'*HIATUS COMMENT(S)*'                                 
         GOTO1 REPORT                                                           
*                                                                               
         USING RCONHCEL,R6                                                      
NDHI220  ZIC   R1,RCONHCLN         ELEMENT LENGTH                               
         LA    R4,2                                                             
         SR    R1,R4               SUBTRACT OVERHEAD                            
         LTR   R1,R1               ZERO LENGTH?                                 
         BZ    NDHI230             YES, DON'T PRINT ANYTHING                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),RCONHCCM    MOVE TO PRINT LINE                           
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
NDHI230  BAS   RE,NEXTEL           ANY MORE ELEMENTS?                           
         BE    NDHI220             YES                                          
*                                                                               
NDHIX    DS    0H                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* PRINT EI CODES                                                                
*********************************************************************           
PRTEICDS NTR1                                                                   
         LA    R6,RCONREC                                                       
         USING RCONIEL,R6                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTEICDX                                                         
         MVC   P+13(EIHEADLQ),EIHEADER                                          
         MVC   P+31(4),RCONIADV                                                 
         MVC   P+41(4),RCONIPRD                                                 
         MVC   P+51(4),RCONIPR2                                                 
         MVC   P+61(10),RCONXEST                                                
         OC    P+61(10),SPACES                                                  
         CLC   P+61(10),SPACES                                                  
         BNE   *+10                                                             
         MVC   P+61(4),RCONIEST                                                 
                                                                                
PRTEICDX DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         DROP  R6                                                               
                                                                                
EIHEADER DS    0C                                                               
         DC    C'AGENCY CODES  CLT:      PRD:      PTR:      EST:'              
EIHEADLQ EQU   *-EIHEADER                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT HEADER 'FLIGHT N' FOR EACH CHANGE IN FLT NUM                 
***********************************************************************         
FLTHEAD  ST    RE,FULL                                                          
         ZIC   R1,LINE             WILL HEADER FIT ON PAGE                      
         LA    R1,6(R1)            INCREMENT LINE CNT BY SIZE OF HEAD           
         CLM   R1,1,MAXLINES                                                    
         BL    FLTHEAD5            IT FITS                                      
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
*        BAS   RE,HEADLINE         NO-PUT OUT TOP OF FORM FIRST                 
*                                                                               
FLTHEAD5 MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
         MVI   SPACING,1                                                        
         MVC   P+10(7),=C'FLIGHT '                                              
         EDIT  (1,RBUYFLT),(3,P+17),ALIGN=LEFT                                  
         LR    R1,R0               SWITCH NUMBER LENGTH TO R1                   
         LA    R1,7(R1)            FIND LENGTH OF DASHES                        
         SPACE                                                                  
         GOTO1 REPORT                                                           
         MVI   SPACING,1                                                        
         MVI   P+10,DASH                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+11(0),P+10        PROPAGATE DASHES                             
         GOTO1 REPORT                                                           
         MVI   SPACING,1           SKIP A LINE AFTER HEADER                     
         GOTO1 REPORT                                                           
         NI    CONTROL,X'FF'-FLTH  TURN OFF INDICATOR                           
         OI    CONTROL,FLTWT                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO SORT FLIGHT ENTRIES                                            
***********************************************************************         
SORTFLT  NTR1                                                                   
         OI    CONTROL,FLT+FIRST   TURN ON INDICATORS FOR FIGHT MODE            
         MVC   POINTER,VSORTA      RE-INIT FLT ENTRY POINTER                    
         L     R2,VSORTA           PASS ENTRY ADR TO XSORT                      
         ZIC   R6,COUNT                                                         
         XC    P5,P5                                                            
         GOTO1 XSORT,DMCB,(R2),(R6),L'ENTREC,3,,                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE WEEKLY BUCKET TOTALS                                                   
***********************************************************************         
UPWKTOT  ST    RE,FULL                                                          
         TM    RBUYCNTL,X'80'      DO NOT INCLUDE DELETED LINE                  
         BO    UPWKEX                                                           
         TM    RBUYCOMB,X'80'      DO NOT INCLUDE N/A LINES                     
         BO    UPWKEX                                                           
         CLI   RBUYCHGI,C'C'                                                    
         BE    UPWKEX              OR CANCELLED LINES                           
         ZIC   RE,RBUYNW           NUMBER PER WEEK                              
         LR    R0,RE                                                            
         AH    R0,SPOTS            UPDATE SPOTS/WK BUCKET                       
         STH   R0,SPOTS                                                         
         ICM   RF,15,RBUYCOS                                                    
         MR    RE,RE               COST/SPT X SPTS/WK                           
         A     RF,DOLLARS          UPDATE COST/WEEK BUCKET                      
         ST    RF,DOLLARS                                                       
UPWKEX   L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT WEEKLY TOTAL LINE                                            
***********************************************************************         
WKTOT    ST    RE,FULL                                                          
         SPACE                                                                  
         ZIC   RE,LINE             TEST FOR FIT OF TOTALS LINE ON PAGE.         
         LA    RE,1(RE)                                                         
         CLM   RE,1,MAXLINES                                                    
         BL    WKTOT1              YES                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
*        BAS   RE,HEADLINE         NO-PUT OUT HEADLINE                          
*                                                                               
WKTOT1   MVI   SPACING,1                                                        
         MVC   P+39(24),=C'**WEEKLY FLIGHT TOTALS**'                            
         SPACE                                                                  
         EDIT  (2,SPOTS),(3,P+64)                                               
         CLI   OPTION1,C'N'                                                     
         BNE   WKTOT2                                                           
         MVI   P+68,ASTER                                                       
         MVC   P+69(9),P+68        FILL COST WITH ASTERISKS                     
         B     WKTOT4                                                           
         SPACE                                                                  
WKTOT2   EDIT  (4,DOLLARS),(10,P+68),2,FLOAT=-                                  
         B     WKTOT4                                                           
         SPACE                                                                  
WKTOT4   GOTO1 REPORT                                                           
         XC    SPOTS,SPOTS         CLEAR BUCKETS                                
         XC    DOLLARS,DOLLARS                                                  
         SPACE                                                                  
WKTOTEX  DS    0H                                                               
         NI    CONTROL,X'FF'-FLTWT RESET WEEK TOTALS                            
         NI    CONTROL,X'FF'-FIRST TURN OFF INDICATOR                           
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* YES, NO, XIT                                                                  
***********************************************************************         
         ANSR                      YES, NO, XIT                                 
         EJECT                                                                  
***********************************************************************         
* ELEMENT SEARCH ROUTINE                                                        
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* PRINT ROUTINE FOR STORED COMMENTS                                             
***********************************************************************         
PRTREP   NTR1                                                                   
         CLC   LINE,MAXLINES       CHECK IF STORED COMMENTS FIT ON PAGE         
         BL    PRTREP5                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
PRTREP5  GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT *** CONTRACT COMMENT ***                                     
***********************************************************************         
PRTHEAD  NTR1                                                                   
         CLI   STASW,TRUE                                                       
         BE    XIT                                                              
         MVC   P+13(24),=C'*** CONTRACT COMMENT ***'                            
         MVI   STASW,TRUE                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRTREP                                                        
         B     XIT                                                              
***********************************************************************         
* PREVCMB - CHECKS IF K IS COMBO AND IF OTHER MEMBER OF COMBO ALREADY           
*           PROCESSED. IF NOT ALREADY PROCESSED, ADDS K NUMBER TO LIST          
***********************************************************************         
PREVCMB  NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
*                                                                               
         ZIC   R1,1(R6)            ELEM LEN                                     
         SH    R1,=H'2'                                                         
         SR    R0,R0                                                            
         D     R0,=F'9'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,7(R6)                                                         
PREVC10  DS    0H                                                               
         L     R4,ACLIST                                                        
         LR    R5,R4                                                            
         A     R5,=F'16000'                                                     
PREVC15  DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    PREVC20                                                          
         CLC   0(4,R6),0(R4)                                                    
         BE    YES                                                              
         LA    R4,4(R4)                                                         
         CR    R4,R5                                                            
         BNH   PREVC15                                                          
         DC    H'0'                                                             
PREVC20  DS    0H                                                               
         LA    R6,9(R6)                                                         
         BCT   R1,PREVC10                                                       
         MVC   0(4,R4),RCONKCON                                                 
         MVI   4(R4),X'FF'         NEW END OF LIST                              
         B     NO                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* EQUATES                                                                       
*                                                                               
ASTER    EQU   C'*'                                                             
DASH     EQU   C'-'                                                             
TRUE     EQU   C'T'                                                             
FALSE    EQU   C'F'                                                             
FLT      EQU   X'80'               READING FLIGHT BUYS                          
FLTH     EQU   X'40'               PRINT FLIGHT HEADER                          
FIRST    EQU   X'20'               FIRST FLIGHT BUY READ                        
FLTWT    EQU   X'10'               PRINT FLIGHT WEEK TOTALS                     
         SPACE 2                                                                
*                                                                               
* EXTERNAL ROUTINE TABLE                                                        
*                                                                               
MODTAB   DS    0F                                                               
OUTDAY   DC    V(OUTDAY)                                                        
RECUP    DC    V(RECUP)                                                         
REGENBUF DC    V(REGENBUF)                                                      
REGENPBY DC    V(REGENPBY)                                                      
REGENTL2 DC    V(REGENTL2)                                                      
VSORTA   DC    V(SORTA)                                                         
MODULES  EQU   (*-MODTAB)/4                                                     
         EJECT                                                                  
         SPACE 2                                                                
RTGTBL   DS    0CL3                                                             
         DC    C'ARBNSISRCBIRTRCMTDRAM',X'004040',X'FF'                         
MONTBL   DS    0CL4                                                             
         DC    X'01',C'JAN'                                                     
         DC    X'02',C'FEB'                                                     
         DC    X'03',C'MAR'                                                     
         DC    X'04',C'APR'                                                     
         DC    X'05',C'MAY'                                                     
         DC    X'06',C'JUN'                                                     
         DC    X'07',C'JUL'                                                     
         DC    X'08',C'AUG'                                                     
         DC    X'09',C'SEP'                                                     
         DC    X'0A',C'OCT'                                                     
         DC    X'0B',C'NOV'                                                     
         DC    X'0C',C'DEC'                                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
       ++INCLUDE REPINTLIST                                                     
         SPACE                                                                  
* STORAGE AREAS FOR BETWEEN I/O DATA                                            
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
VREPFACS DS    A                                                                
VDEMOCON DS    A                                                                
ACOVAREA DS    A                                                                
ABUCKS   DS    A                                                                
ATBUCKS  DS    A                                                                
AGRAND   DS    A                                                                
ATGRAND  DS    A                                                                
WPCOVFLG DS    X                                                                
YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                         
BYEAR    DS    X                   BINARY YEAR                                  
CFULL    DS    F                                                                
FLTFLDS  DS    0CL20               ADDRESS LABEL FOR FIELDS TO POINTER          
CONTROL  DS    X                                                                
FLIGHT   DS    B                                                                
COUNT    DS    C                                                                
SPOTS    DS    H                                                                
DOLLARS  DS    F                                                                
POINTER  DS    V                   ADDRESS OF NEXT XSORT ENTRY                  
SAVER7   DS    F                                                                
DFLDH    DS    CL8                 DUMMY FIELD HEADER                           
DFLDD    DS    CL8                 DUMMY FIELD DATA                             
TEMP     DS    6F                                                               
REQOFF   DS    CL2                 REQUESTING OFFICE                            
REPID    DS    CL2                 REP CODE                                     
PROG     DS    CL2                 PROGRAM REQUEST (10 OR 11)                   
OPTION1  DS    C                   OPTION 1 AGY NO RATE COPY                    
REQNUM   DS    H                   REQUEST NUMBER                               
PRINTED  DS    C                   Y = REPORTS PRINTED                          
HAVEBUYS DS    X                   HAVE BUYS FLAG (NEED TO RESTORE SEQ)         
ACLIST   DS    A                   A(COMBO K PRINT LIST)                        
*                                                                               
PROFDATA DS    0CL10               PROFILE DATA FROM REP REC                    
         DS    X                   NOT USED                                     
         DS    X                   NOT USED                                     
PROFILES DS    CL8                 REP PROFILES                                 
*                                                                               
COMBONUM DS    X         B         0 = NOT A COMBO ORDER                        
*                                  N = COMBO ORDER WITH N STATIONS              
CMBSTAT  DS    X                   COMBO STATUS FLAG                            
CMBDONE  EQU   X'80'               FF = WE ARE FINISHED                         
CMBBUY   EQU   X'40'               COMBO ORDER HAS BUY                          
CMBCMT   EQU   X'20'               PRINT COMBO CONTRACT COMMENT                 
*                                                                               
SVCONNUM DS    XL4                                                              
*                                                                               
COMBOSC  DS    0CL36                                                            
COMBOS1  DS    CL5       A         READ FROM X'17' ELEMENT, COMBO STA 1         
COMBOC1  DS    XL4       P         STA 1 COMBO K #                              
COMBOS2  DS    CL5       A         READ FROM X'17' ELEMENT, COMBO STA 2         
COMBOC2  DS    XL4       P         STA 2 COMBO K #                              
COMBOS3  DS    CL5       A         READ FROM X'17' ELEMENT, COMBO STA 3         
COMBOC3  DS    XL4       P         STA 3 COMBO K #                              
COMBOS4  DS    CL5       A         READ FROM X'17' ELEMENT, COMBO STA 4         
COMBOC4  DS    XL4       P         STA 4 COMBO K #                              
*                                                                               
SVCTDES  DS    CL20                CONTRACT TYPE DESCRIPTION                    
SVDTDES  DS    CL20                DEVELOPEMENTAL TYPE DESCRIPTION              
SVPTCDE  DS    CL3                 POINT PERSON CODE                            
SVPTNAM  DS    CL20                POINT PERSON NAME                            
SVPTTEL  DS    CL20                POINT PERSON TEL                             
*                                                                               
FMTFLK   DS    CL1                      FORMAT RECORD OPTION FLAGS              
PRTFMTFL DS    CL1                      PRINTING & FORMAT FLAGS                 
PRTRNAM  DS    CL33                     PRINTED REP NAME                        
PRTRAD1  DS    CL20                     PRINTED REP ADDRESS 1                   
PRTRAD2  DS    CL34                     PRINTED REP ADDRESS 2                   
PRTRRNAM DS    CL33                     PRINTED REAL REP NAME FIELD             
PRTANAM  DS    CL33                     PRINTED AGENCY NAME                     
PRTBUYER DS    CL20                     PRINTED BUYER NAME                      
PRTAAD1  DS    CL34                     PRINTED AGENCY ADDRRESS 1               
PRTAAD2  DS    CL36                     PRINTED AGENCY ADDRESS 2                
PRTAAD3  DS    CL36                     PRINTED AGENCY ADDREESS 3               
PRTSALCD DS    CL3                      PRINTED SALESPERSON CODE                
PRTSALNM DS    CL20                     PRINTED SALESPERSON NAME                
PRTSALPH DS    CL20                     PRINTED SALESPERSON PHONE               
PRTADVNM DS    CL30                     PRINTED ADVERTISER NAME                 
*                                                                               
WORK2    DS    CL240                                                            
*                                                                               
* AREA FOR XSORT ENTRIES OF FLIGHTED BUYS                                       
*                                                                               
SORTA    DS    0D                                                               
         DS    2000C                                                            
***********************************************************************         
**** WATCH OUT HERE!!!!!! *****                                                 
*******************************                                                 
ACLAREA  DS    4000XL4                                                          
*                                                                               
* THE AREA SORTA+2000 IS ADDRESSED BY ACLIST, AND IS USED TO BUFFER THE         
* NUMBERS OF COMBO K'S PRINTED SO FAR.                                          
*                                                                               
BUCKS    DS    100H                     CASH BUCKETS AREA                       
TBUCKS   DS    100H                     TRADE BUCKETS AREA                      
GRAND    DS    100H                     COMBO CASH GRAND TOTAL AREA             
TGRAND   DS    100H                     COMBO TRADE GRAND TOTAL AREA            
TEMPBUCK DS    100H                     TEMPORARY BUCKET AREA                   
*                                                                               
*  COVAREA IS USED AS A COVERSHEET RECORD 4K IO AREA                            
*                                                                               
*                                                                               
***********************************************************************         
STORED   DSECT                                                                  
STASW    DS    C                                                                
ELCODE   DS    C                                                                
SOPTIONS DS    X                   STATION OPTION                               
STAOPT7  EQU   X'80'               PRINT AGY, ADV AND TRAF #                    
STALIAB  DS    X                   STATION LIABILITY POSITION                   
STCMODE  DS    X                   STORED COMMENT ROUTINE MODE                  
SAVKEY   DS    CL32                                                             
TEXTKEY  DS    CL32                                                             
SVBUYKEY DS    CL32                                                             
SVBUYK2  DS    CL32                                                             
SVCONKEY DS    CL32                                                             
MYSVKEY  DS    CL32                                                             
MYSVKEY2 DS    CL32                                                             
PRTCMBPT DS    X         B         COMBO INDEX                                  
PSAVE    DS    CL132               P SAVE AREA                                  
OPT2     DS    CL1                                                              
OPT2A    DS    CL1                                                              
*                                                                               
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
         DS    0D                                                               
BLOCK    DS    3000C                                                            
STOREX   EQU   *                                                                
*              FILE CONTROL AND WORKD DSECTS                                    
* DSECT TO COVER FLIGHTED BUY ENTRIES                                           
*                                                                               
ENTD     DSECT                                                                  
ENTREC   DS    0CL7                                                             
ENTFLT   DS    X                                                                
ENTMAS   DS    X                                                                
ENTLIN   DS    X                                                                
ENTDA    DS    XL4                                                              
         SPACE 2                                                                
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         SPACE 2                                                                
       ++INCLUDE REGENDCT                                                       
* INVENTORY RECORD DSECT FOR LIBRARY TEXT AND REGENPBY OUTPUT DSECT             
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENPBYD                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
* CONTRACT PROGRAM PROFILE EQU'S                                                
       ++INCLUDE RECNTPROF                                                      
         EJECT                                                                  
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE REGENCOV                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REPFACSQ                                                       
***********************************************************************         
* PROCESS AND PRINT BUY LINES                                                   
***********************************************************************         
BUYPRT   CSECT                                                                  
         NMOD1 0,**BPRT**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         TM    CMBSTAT,CMBCMT      PRINT COMBO CONTRACT COMMENT?                
         BZ    BPRT05                                                           
         BAS   RE,PRTKCMT          CHECK FOR CONTRACT COMMENTS                  
         B     BPRTX                                                            
*                                                                               
BPRT05   DS    0H                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   BPRT160                                                          
*                                                                               
         CLI   RBUYFLT,0                                                        
         BE    FLTSTART                                                         
         BAS   RE,BLDENTRY                                                      
         B     BPRTX                                                            
FLTSTART DS    0H                                                               
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   BPRT10                                                           
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         BAS   RE,PRTKCMT          CHECK FOR CONTRACT COMMENTS                  
         EJECT                                                                  
BPRT10   TM    RBUYCNTL,X'80'      DELETED?                                     
         BO    BPRT70                                                           
         TM    RBUYCOMB,X'80'      N/A?                                         
         BO    BPRT70                                                           
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BPRT70                                                           
* BUILD BUCKETS FROM BUYS TO CHECK CONTRACT BUCKETS                             
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         GOTOX (RFGENBUC,VREPFACS),DMCB,RBUYREC,BLOCK,WORK                      
* ADD BUCKETS TO RUNNING BUCKETS                                                
         LA    R2,BLOCK                                                         
***>     TM    RBUYFLG2,X'02'      TRADE BUY?                                   
***>     BNO   BPRT15              NO  - DON'T SET FLAG                         
***>     MVI   HALF2,1             TURN ON 'TRADE FOUND' FLAG                   
BPRT15   EQU   *                                                                
         CLC   BLOCK(2),=H'2'      NONE?                                        
         BE    BPRT70                                                           
         LA    R8,BLOCK+2          1ST BUY BUCKET                               
         LH    R3,BLOCK                                                         
         LA    R3,BLOCK-1(R3)                                                   
*                                                                               
BPRT20   DS    0H                                                               
         L     R6,ABUCKS                                                        
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BZ    *+8                                                              
         L     R6,ATBUCKS          USE TRADE BUCKET AREA                        
         LR    R2,R6                                                            
         LH    R1,0(R6)                                                         
         LA    R5,0(R1,R6)                                                      
         AHI   R5,-1                                                            
         AHI   R6,-12              1ST BUCKET-14                                
         LA    R4,14                                                            
BPRT30   BXLE  R6,R4,BPRT60        NEXT TOTAL BUCKET                            
BPRT40   XC    4(2,R8),4(R8)                                                    
         GOTO1 RECUP,DMCB,(X'FF',(R2)),(R8),(R6)                                
*                                                                               
BPRT50   LA    R8,14(R8)           INCREMENT INDEX                              
         CR    R8,R3                                                            
         BNH   BPRT20                                                           
         B     BPRT70                                                           
*                                                                               
BPRT60   CLC   2(2,R8),2(R6)       SAME YR-MONTH?                               
         BH    BPRT30                                                           
         BL    BPRT40                                                           
* ADD BUY BUCKET TO TOTAL BUCKET                                                
* ADD DOLLARS AND SPOTS                                                         
         MVC   DUB(8),6(R8)        $ AND SPOTS                                  
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R6)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R6),DUB                                                      
         B     BPRT50                                                           
* DISPLAY BUY                                                                   
BPRT70   MVC   WORK(4),OUTDAY      PASS ROUTINE ADDRESSES IN WORK               
         MVC   WORK+4(4),UNTIME                                                 
         MVC   WORK+8(4),DATCON                                                 
         MVC   WORK+12(4),ADDAY                                                 
         MVC   WORK+16(4),VREPFACS                                              
         GOTO1 REGENPBY,DMCB,RBUYREC,(28,BLOCK),WORK,RCONREC,PROFILES           
         SR    R2,R2               CAN WE FIT ON THE PAGE                       
         IC    R2,DMCB+4                                                        
         ST    R2,CFULL                                                         
         LTR   R2,R2               NULL LINE?                                   
         BP    *+16                                                             
         TM    CONTROL,FLT         EXIT IF IN NON-FLIGHTED LOGIC                
         BZ    BPRTX                                                            
         B     BPRT140                                                          
         CLI   OPTION1,C'N'        AGENCY COPY?                                 
         BNE   BPRT90                                                           
BPRT80   MVI   BLOCK+65,C'*'       ELIMINATE COST                               
         MVC   BLOCK+66(9),BLOCK+65                                             
         SPACE 1                                                                
BPRT90   EQU   *                                                                
*                                                                               
         TM    RBUYCOMB,X'80'      SKIP IF BUYLINE IS A COMBO                   
         BO    BPRT140             PLACE HOLDER (IE N/A RATE)                   
*                                                                               
         TM    CONTROL,FLTH        PUT OUT FLIGHT HEADER BEFORE FIRST           
         BZ    *+8                 BUY LINE FOR FLIGHT.                         
         BAS   RE,FLTHEAD                                                       
         SR    R6,R6                                                            
         IC    R6,LINE                                                          
         AR    R2,R6                                                            
         LA    R2,1(R2)                                                         
         IC    R6,MAXLINES                                                      
         CR    R2,R6                                                            
         BL    BPRT100                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         SPACE 2                                                                
BPRT100  LA    R2,BLOCK            PRINT THE BLOCK                              
         L     R6,CFULL                                                         
         SPACE 2                                                                
BPRT110  CH    R6,=H'1'                                                         
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
*                                                                               
         CLC   =C'P=',13(R2)                                                    
         BNE   BPRT120                                                          
         MVC   P+16(12),=C'PROGRAMMING='                                        
         MVC   P+28(55),15(R2)     NOTE: REST OF LINE IS CHOPPED OFF            
         B     BPRT130                                                          
*                                                                               
BPRT120  MVC   P+3(80),0(R2)       SKIPS BUYTYP                                 
BPRT130  GOTO1 REPORT                                                           
         LA    R2,L'PRTLN(R2)                                                   
         BCT   R6,BPRT110                                                       
BPRT140  TM    CONTROL,FLT                                                      
         BZ    BPRTX                                                            
         ZIC   R1,COUNT                                                         
         SH    R1,=H'1'            DECREMENT FLIGHT COUNTER                     
         BZ    BPRT150             AT ZERO-PREPARE TO WRAP UP                   
         STC   R1,COUNT            REPLACE COUNTER                              
         BAS   RE,FLTREAD          READ NEXT FLIGHTED BUY                       
         B     FLTSTART            GO BACK AND FORMAT IT                        
         SPACE                                                                  
BPRT150  TM    CONTROL,FLTWT                                                    
         BZ    *+8                                                              
         BAS   RE,WKTOT            PUT OUT WKLY TOTS FOR LAST FLT               
         B     BPRT170                                                          
         SPACE 2                                                                
BPRT160  CLI   MODE,CONTLAST                                                    
         BNE   BPRTX                                                            
         CLI   COUNT,0                                                          
         BE    BPRT170             NO FLIGHT BUYS                               
         BAS   RE,SORTFLT          SORT THE FLIGHT BUY ENTRIES                  
         BAS   RE,FLTREAD          READ THE FIRST BUY AND                       
         B     FLTSTART            FORMAT IT.                                   
         SPACE                                                                  
BPRT170  CLI   FORCEHED,C'Y'                                                    
         BNE   BPRT175                                                          
* NO BUYS FOR CONTRACT                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         BAS   RE,PRTKCMT          PRINT CONTRACT COMMENTS                      
BPRT175  TM    RCONCNTL,X'80'                                                   
         BZ    BPRT180                                                          
         MVC   P+10(24),=C'*** CONTRACT DELETED ***'                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
BPRT180  DS    0H                                                               
* 14JAN91  *** START ***                                                        
         TM    RCONMODR+1,X'20'    MON???                                       
         BZ    BPRT190                                                          
*                                                                               
*******************************************************************             
*                                                                 *             
* WORK2 IS THE STARTING BUCKET TOTAL   (BUILT IN DISBUCK)         *             
*    AREA+0(2)=YYMM                                               *             
*    AREA+2(4)=DOLLARS                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         GOTO1 REPORT                                                           
* BUILD TOTAL BUCKETS                                                           
BLDBUCK  DS    0H                                                               
         LA    R5,WORK2                                                         
         XCEF  (R5),240                                                         
         XR    R3,R3               KEEP TOTAL $ COUNTER                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   BBX                 NO BUCKETS YET                               
BB4      MVC   0(2,R5),2(R6)       SAVE BUCK YYMM                               
BB5      L     R1,6(R6)            R1=BUCK AMT                                  
         AR    R3,R1               TOTAL TOTAL (ALL MOS)                        
         A     R1,2(R5)            ADD RUNNING TOTAL                            
         ST    R1,2(R5)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BBX                                                              
         CLC   0(2,R5),2(R6)       SAME BDCST MON?                              
         BE    BB5                                                              
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     BB4                                                              
BBX      DS    0H                                                               
         ST    R3,SAVER7                                                        
*                                                                               
DISBUCK  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)      K START DATE              
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)  K END DATE                
* WORK HAS EBCDIC START AND WORK+6 HAS END DATE                                 
         GOTO1 GETBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 GETBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
* WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END                  
* BROADCAST MONTH AND YEAR (YMD - BINARY)                                       
*                                                                               
         LA    R5,WORK2                                                         
         MVC   YEAR,TEMP+6         K START YEAR                                 
         MVC   BYEAR,WORK          SAVE BIN YEAR                                
         ZIC   R4,WORK+1           START MON                                    
         ZIC   R3,WORK+4           END MON                                      
         CLC   WORK(1),WORK+3      SAME YEAR?                                   
         BE    *+8                                                              
         LA    R3,12(R3)           ADJUST FOR NEXT YEAR                         
         SR    R3,R4                                                            
         LA    R3,1(R3)            NUM MONTHS                                   
         LR    R1,R3               GET NUM OF LINES                             
         SRL   R1,2                DIV BY 4, FORGET THE REMAINDER               
         LA    R1,1(R1)                                                         
         LR    R0,R1               CAN'T 'LA' R0...                             
         BCTR  R4,0                MONTHS ARE 0 RELATIVE                        
         MH    R4,=Y(L'MONTBL)     R4 NOW AT STARTING MONTH                     
         LA    R4,MONTBL(R4)       IT HELPS TO POINT IT AT THE TABLE            
         LA    R2,BLOCK            OUTPUT AREA                                  
*                                                                               
         XCEF  (R2),3000           CLEAR FOR COMBO MULTIPLE USE                 
*                                                                               
DB10     DS    0H                                                               
         MVC   0(3,R2),1(R4)       MMM                                          
         MVC   3(2,R2),YEAR        YY                                           
         LA    R2,7(R2)                                                         
*                                                                               
* DISPLAY $ (IF ANY)                                                            
         CLC   0(1,R5),BYEAR       THIS YEAR?                                   
         BNE   DB20                                                             
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   DB20                                                             
         LR    R1,R0               EDIT KILLS R0                                
         EDIT  (4,2(R5)),(10,0(R2)),2                                           
         LR    R0,R1                                                            
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
DB20     LA    R2,13(R2)                                                        
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   DB30                NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   YEAR,TEMP+18        K END YEAR (EBCDIC)                          
         ZIC   R1,BYEAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYEAR                                                         
DB30     BCT   R3,DB10                                                          
         LR    R5,R0               EDIT KILLS R0                                
         MVC   0(05,R2),=C'TOTAL'                                               
         EDIT  (4,SAVER7),(10,7(R2)),2,FLOAT=$                                  
         LR    R2,R5               FOR BCT LATER                                
         LA    R6,BLOCK                                                         
DB35     DS    0H                  CASH TOTALS                                  
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,DB35                                                          
         B     BPRTX                                                            
* 14JAN91  *** END ***                                                          
BPRT190  DS    0H                                                               
* DISPLAY CONTRACT TOTALS FROM BUY BUCKETS                                      
         GOTO1 REGENTL2,DMCB,ABUCKS,BLOCK  USE NON-ZERO BUCKS TO PRINT          
         SR    R2,R2                                                            
         IC    R2,DMCB+4           NUMBER OF LINES                              
* DON'T CHANGE R2 TILL WE PRINT OUT THE BLOCK--CONTAINS NUM OF LINES            
*                                                                               
         L     R5,ABUCKS                                                        
         LH    R3,0(R5)                                                         
* DELETE ALL ZERO BUCKETS FROM READING BUYS                                     
         LA    R3,0(R5,R3)                                                      
         AHI   R3,-1                                                            
         AHI   R5,-12                                                           
         SPACE 1                                                                
BPRT200  LA    R5,14(R5)                                                        
         CR    R5,R3                                                            
         BNH   *+8                                                              
         B     BPRT210                                                          
         OC    6(4,R5),6(R5)       NO DOLLARS?                                  
         BNZ   BPRT200                                                          
* DELETE BUCKET                                                                 
         GOTO1 RECUP,DMCB,(X'FF',ABUCKS),(R5),(R5)                              
         L     R1,ABUCKS                                                        
         LH    R3,0(R1)                                                         
         LA    R3,0(R1,R3)                                                      
         BCTR  R3,0                                                             
         SH    R5,=H'14'                                                        
         B     BPRT200                                                          
         SPACE 1                                                                
*                                                                               
**RT210  GOTO1 REGENTL2,DMCB,ABUCKS,BLOCK                                       
BPRT210  DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)         MUST HAVE R2 FROM ABOVE HERE                 
         L     R1,ATBUCKS                                                       
         CLC   =X'0002',0(R1)      ANY TRADE BUCKETS?                           
         BE    *+8                                                              
         LA    R6,1(R6)            YES - ALLOW AN EXTRA LINE                    
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    BPRT220                                                          
         BP    BPRT215                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     BPRT220                                                          
*                                                                               
BPRT215  STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
BPRT220  DS    0H                                                               
         CLI   OPTION1,C'N'        STN/AGY COPY?                                
         BE    BPRTX                                                            
*                                                                               
         LA    R6,BLOCK                                                         
*                                                                               
         L     R1,ATBUCKS                                                       
         CLC   =X'0002',0(R1)      ANY TRADE DOLLARS IN ARRAY?                  
         BE    BPRT225             NO                                           
*                                  YES - DISPLAY DOLLAR TYPE                    
         L     R1,ABUCKS                                                        
         CLC   =X'0002',0(R1)      ANY CASH DOLLARS IN ARRAY?                   
         BE    BPRT225A            NO                                           
*                                  YES - DISPLAY DOLLAR TYPE                    
         MVC   P+1(08),=C'**CASH**'                                             
         GOTO1 REPORT                                                           
*                                                                               
* PRINT TOTAL BLOCKS                                                            
*                                                                               
BPRT225  DS    0H                  CASH TOTALS                                  
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,BPRT225                                                       
*                                                                               
BPRT225A DS    0H                                                               
         L     R1,ATBUCKS                                                       
         CLC   =X'0002',0(R1)      ANY TRADE DOLLARS IN ARRAY?                  
         BE    BPRT229             NO - DONE W/TOTALS                           
*                                                                               
         GOTO1 REGENTL2,DMCB,ATBUCKS,BLOCK  TRADE BUCKETS                       
         ZIC   R2,DMCB+4           NUMBER OF LINES                              
         ZIC   R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)         MUST HAVE R2 FROM ABOVE HERE                 
         LA    R6,1(R6)            EXTRA LINE FOR LABEL                         
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    BPRT226A                                                         
         BP    BPRT226                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     BPRT226A                                                         
*                                                                               
BPRT226  STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
BPRT226A DS    0H                                                               
         MVC   P+1(09),=C'**TRADE**'                                            
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,BLOCK                                                         
BPRT226B DS    0H                                                               
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,BPRT226B                                                      
*                                  YES - DISPLAY DOLLAR TYPE                    
         L     R1,ABUCKS                                                        
         CLC   =X'0002',0(R1)      ANY CASH DOLLARS IN ARRAY?                   
         BE    BPRTX               NO - DONE                                    
*                                                                               
* ADD TRADE TO CASH BUCKETS                                                     
*                                                                               
         L     R4,ABUCKS                                                        
         L     R3,=A(TEMPBUCK)                                                  
         A     R3,RELO                                                          
         MVC   0(200,R3),0(R4) TEMP GRAND TOTAL BUFFER                          
         L     R4,ATBUCKS                                                       
         GOTO1 =A(TOTLTOTL),RR=Y   CASH + TRADE BUCKETS                         
*&&DO                                                                           
         L     R2,ATBUCKS                                                       
         LA    R8,2(R2)            1ST TRADE BUCKET                             
         LH    R3,0(R2)                                                         
         LA    R3,0(R3,R2)                                                      
         BCTR  R3,0                                                             
*                                                                               
BPRT227  DS    0H                                                               
         L     R6,ABUCKS           CASH BUCKETS                                 
         LR    R2,R6                                                            
         LH    R1,0(R6)                                                         
         LA    R5,0(R1,R6)                                                      
         AHI   R5,-1                                                            
         AHI   R6,-12              1ST BUCKET-14                                
         LA    R4,14                                                            
BPRT227A BXLE  R6,R4,BPRT227D      NEXT TOTAL BUCKET                            
BPRT227B XC    4(2,R8),4(R8)                                                    
         GOTO1 RECUP,DMCB,(X'FF',(R2)),(R8),(R6)                                
*                                                                               
BPRT227C LA    R8,14(R8)           INCREMENT INDEX                              
         CR    R8,R3                                                            
         BNH   BPRT227                                                          
         B     BPRT227E                                                         
*                                                                               
BPRT227D CLC   2(2,R8),2(R6)       SAME YR-MONTH?                               
         BH    BPRT227A                                                         
         BL    BPRT227B                                                         
* ADD BUY BUCKET TO TOTAL BUCKET                                                
* ADD DOLLARS AND SPOTS                                                         
         MVC   DUB(8),6(R8)        $ AND SPOTS                                  
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R6)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R6),DUB                                                      
         B     BPRT227C                                                         
*&&                                                                             
BPRT227E DS    0H                                                               
         GOTO1 REGENTL2,DMCB,(R3),BLOCK  COMBINED BUCKETS                       
***>>    GOTO1 REGENTL2,DMCB,ABUCKS,BLOCK  COMBINED BUCKETS                     
         ZIC   R2,DMCB+4           NUMBER OF LINES                              
         ZIC   R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)         MUST HAVE R2 FROM ABOVE HERE                 
         LA    R6,1(R6)            EXTRA LINE FOR LABEL                         
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    BPRT228A                                                         
         BP    BPRT228                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     BPRT228A                                                         
*                                                                               
BPRT228  STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
BPRT228A DS    0H                                                               
         MVC   P+1(14),=C'**CASH+TRADE**'                                       
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,BLOCK                                                         
BPRT228B DS    0H                                                               
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,BPRT228B                                                      
*                                                                               
         B     BPRTX                                                            
*                                                                               
* BUILD ANOTHER SET OF BUCKETS FROM CONTRACT REC - 1 BUCKET PER MONTH           
*                                                                               
BPRT229  DS    0H                                                               
         GOTO1 REGENBUF,DMCB,RCONREC,BLOCK+400                                  
* ZERO OUT MONTH CTRS IN BUCKETS FROM READING BUYS - NO SPOTS IN CONREC         
         L     R6,ABUCKS                                                        
         LA    R6,2(R6)            FIRST BUCKET                                 
         SR    R4,R4                                                            
BPRT230  CLI   0(R6),0             LAST?                                        
         BE    BPRT240                                                          
         XC    10(4,R6),10(R6)     ZERO SPOT COUNTS                             
         IC    R4,1(R6)                                                         
         AR    R6,R4                                                            
         B     BPRT230                                                          
BPRT240  EQU   *                                                                
*                                                                               
*   FOR PETRY/TEST ORDERS (V1), SKIP FOLLOWING TEST                             
*                                                                               
***>     B     BPRT250             TESTING ***>                                 
         CLC   =C'PV',RCONKREP     PETRY?                                       
         BE    BPRT241             YES                                          
         CLC   =C'V1',RCONKREP     TEST CONVERSION REP                          
         BNE   BPRT245             NO                                           
BPRT241  EQU   *                                                                
         TM    RCONMODR+1,X'10'    CONVERTED ORDER?                             
         BO    BPRT250             YES - DON'T DO CHECK                         
* NOW CHECK CONTRACT BUCKETS AGAINST BUY BUCKETS                                
BPRT245  EQU   *                                                                
         L     R1,ABUCKS                                                        
         LH    R4,0(R1)                                                         
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),BLOCK+400                                                
         BE    BPRT250                                                          
* BUCKETS OUT                                                                   
***>     CLI   HALF2,1             'TRADE FOUND' SET?                           
***>     BE    BPRT250             YES - DON'T PUT OUT MESSAGE                  
*                                                                               
         MVC   P+10(62),=C'*** BUY TOTALS DO NOT AGREE WITH BUCKET TOTAX        
               LS -NOTIFY DDS ***'                                              
         GOTO1 REPORT                                                           
*                                                                               
*   TEST DUMP                                                                   
***>>>   DC    H'0'                                                             
*   TEST DUMP END                                                               
*                                                                               
BPRT250  EQU   *                                                                
BPRTX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A FLIGHT ENTRY FOR LATER XSORT CALL                          
***********************************************************************         
BLDENTRY TM    RBUYCNTL,X'80'      BUILD A FLIGHT ENTRY FOR                     
         BZ    *+10                CANCELLED, BUT NOT DELETED BUYS.             
         CLI   RBUYCHGI,C'X'                                                    
         BER   RE                                                               
         L     RF,POINTER                                                       
         USING ENTD,RF                                                          
         MVC   ENTFLT,RBUYFLT      FLIGHT NUMBER                                
         MVC   ENTMAS,RBUYKMLN     MASTER LINE                                  
         MVC   ENTLIN,RBUYKLIN     LINE                                         
         MVC   ENTDA,KEY+28        DISK ADDRESS                                 
         ZIC   R1,COUNT                                                         
         LA    R1,1(R1)            INCREMENT RECORD COUNTER                     
         STC   R1,COUNT                                                         
         LA    RF,L'ENTREC(RF)                                                  
         ST    RF,POINTER                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO READ FLIGHTED BUYS AND TO CONTROL PRINTING OF                  
* HEADERS AND FLIGHT SUB-TOTALS.                                                
***********************************************************************         
FLTREAD  NTR1                                                                   
         L     R2,POINTER                                                       
         USING ENTD,R2                                                          
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,ENTDA,RBUYREC,      X        
               DMWORK                                                           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   FLIGHT,RBUYFLT      HAS FLIGHT NUMBER CHANGED                    
         BE    FLTREAD2            NO                                           
         MVC   FLIGHT,RBUYFLT      REPLACE WITH THE NEW FLIGHT                  
         OI    CONTROL,FLTH        TURN ON FLIGHT HEADER INDICATOR              
         TM    CONTROL,FLTWT                                                    
         BZ    *+8                                                              
         BAS   RE,WKTOT            PUT OUT WEEKLY TOTALS FOR LAST FLT           
         SPACE                                                                  
FLTREAD2 BAS   RE,UPWKTOT          UPDATE WEEKLY BUCKETS                        
         LA    R2,L'ENTREC(R2)     INCREMENT POINTER TO FLIGHT ENTRIES          
         ST    R2,POINTER                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
*   ROUTINE TO PRINT AGY COMMENT                                                
**********************************************************************          
DOAGYCMT CSECT                                                                  
         NMOD1 0,*AGYCMT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'1A'           RAGY2REC                                     
         MVC   KEY+19(4),RCONKAGY                                               
         MVC   KEY+23(2),RCONKAOF                                               
         MVC   KEY+25(2),RCONKREP                                               
         MVC   TEXTKEY,KEY         SAVE KEY FOR LATER COMPARE                   
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),TEXTKEY                                                  
         BNE   SLCMT                                                            
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,IOAREA,DMWORK                    
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'40'        AGY CMT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                  FIRST COMMENT LINE                           
         XC    WORK2,WORK2                                                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
         BAS   RE,NEXTEL                                                        
         BNE   AGYCMT20                                                         
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+62(0),0(R6)                                                
AGYCMT20 GOTO1 =A(PSTCMT),DMCB,(2,WORK2),(RC),(RA),(R9),RR=Y                    
*                                                                               
         CLI   WORK2+62,0                                                       
         BE    XIT                                                              
*                                  SECOND COMMENT LINE                          
         GOTO1 =A(PSTCMT),DMCB,(2,WORK2+62),(RC),(RA),(R9),RR=Y                 
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRTCOV - PRINT COVERSHEET                                                     
***********************************************************************         
PRTCOV   CSECT                                                                  
         NMOD1 0,*PRTCOV*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
COVWIDE  EQU   80                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         CLI   RCONREC,X'0C'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVERX                                                           
*                                                                               
         USING RCONCVEL,R6                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING RCOVREC,R2                                                       
         GOTOX (RFCONLOW,VREPFACS),DMCB,RCONREC  GET LOWEST K NUMBER            
         MVI   KEY,X'49'                                                        
         MVC   RCOVKREP,RCONKREP                                                
         MVC   RCOVKNAM,RCONCVNM                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   COVERX                                                           
***>>>   BE    *+6                                                              
***>>>   DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,ACOVAREA,    +        
               DMWORK,0                                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,ACOVAREA                                                      
         DROP  R6                                                               
*                                                                               
         MVI   WPCOVFLG,0          INIT FLAGS                                   
         TM    RCOVFLAG,RCOVFLAF                                                
         BZ    *+8                                                              
         OI    WPCOVFLG,X'80'      AUTO FORMAT ON                               
*                                                                               
         LA    R3,COVWIDE                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),=132C'*' SECTION HEADER                                     
         LA    R3,1(R3)                                                         
         SH    R3,=Y(L'COVHEAD)                                                 
         SRL   R3,1                                                             
         LA    R4,P                                                             
         AR    R4,R3                                                            
         MVC   0(L'COVHEAD,R4),COVHEAD                                          
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,RCOVEL1          POINT TO 1ST ELEM IN RCOVREC                 
         DROP  R2                                                               
*                                                                               
COVER050 DS    0H                                                               
         BAS   RE,GETCOV           GET NEXT COVER TEXT ELEM                     
         LTR   R6,R6               HAVE ANOTHER ELEMENT?                        
         BZ    COVER200            NO -WRAP IT UP                               
*                                                                               
         CLI   1(R6),2             PRINTING BLANK LINE?                         
         BH    COVER060            NO, NEXT CASE                                
         TM    WPCOVFLG,X'80'      AUTO FORMAT ON                               
         BZ    COVER055                                                         
         CLC   P,SPACES            ANYTHING ON LINE?                            
         BE    COVER055                                                         
         GOTO1 REPORT                                                           
COVER055 DS    0H                                                               
         ZIC   R1,LINE                                                          
         LA    R1,2(R1)                                                         
         CLM   R1,1,MAXLINES       ROOM LEFT?                                   
         BL    COVER058            YES                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     COVER050            NEXT ELEMENT                                 
COVER058 DS    0H                                                               
         GOTO1 REPORT                                                           
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER060 DS    0H                                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'2'            LENGTH FOR CURRENT DATA                      
         LA    R5,2(R6)            A(CURRENT DATA)                              
         MVC   FULL,0(R5)                                                       
         OC    FULL,SPACES         KEEP THE 1ST 4 CHAR IN CAPS                  
*                                                                               
         CH    R3,=H'2'            CHECK FOR '$P' PAGE BREAK                    
         BNE   COVER070                                                         
         CLC   =C'$P',FULL                                                      
         BNE   COVER070                                                         
         TM    WPCOVFLG,X'80'      AUTO FORMAT ON                               
         BZ    COVER065                                                         
         CLC   P,SPACES            ANYTHING ON LINE?                            
         BE    COVER065                                                         
         GOTO1 REPORT                                                           
COVER065 DS    0H                                                               
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     COVER050                                                         
*                                                                               
COVER070 DS    0H                  CHECK FOR '$ON' CONTROL                      
         CH    R3,=H'3'                                                         
         BNE   COVER080                                                         
         CLC   =C'$ON',FULL                                                     
         BNE   COVER080                                                         
         OI    WPCOVFLG,X'80'      AUTO FORMAT ON                               
         B     COVER050                                                         
*                                                                               
COVER080 DS    0H                  CHECK FOR '$OFF' CONTROL                     
         CH    R3,=H'4'                                                         
         BNE   COVER090                                                         
         CLC   =C'$OFF',FULL                                                    
         BNE   COVER090                                                         
         NI    WPCOVFLG,X'FF'-X'80' AUTO FORMAT OFF                             
         CLC   P,SPACES            ANYTHING ALREADY ON LINE?                    
         BE    COVER090            NO - OK                                      
         ZIC   R1,LINE                                                          
         LA    R1,2(R1)                                                         
         CLM   R1,1,MAXLINES       ROOM LEFT?                                   
         BL    COVER085            YES                                          
         MVC   PSAVE,P                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         MVC   P,PSAVE                                                          
         B     COVER050                                                         
COVER085 DS    0H                                                               
         GOTO1 REPORT                                                           
         B     COVER050                                                         
*                                                                               
COVER090 DS    0H                  HANDLE LINE TEXT                             
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),SPACES      CONVERT TO UPPERCASE                         
*                                                                               
         ZIC   R1,LINE                                                          
         LA    R1,2(R1)                                                         
         CLM   R1,1,MAXLINES       ROOM LEFT?                                   
         BL    COVER095            YES                                          
         MVC   PSAVE,P                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         MVC   P,PSAVE                                                          
*                                                                               
COVER095 DS    0H                                                               
         TM    WPCOVFLG,X'80'      AUTO FORMAT ON?                              
         BZ    COVER150            NO                                           
*                                                                               
COVER100 DS    0H                  FIGURE OUT HOW MUCH SPACE LEFT               
         LA    RF,P                START OF PRINT LINE                          
         LA    R2,COVWIDE                                                       
         AR    R2,RF               END OF PRINT LINE                            
         SR    R4,R4               CLEAR COUNTER                                
         BCTR  R2,0                BACK UP 1 CHAR                               
         CLI   0(R2),C' '          SPACE ?                                      
         BNE   *+18                                                             
         LA    R4,1(R4)                                                         
         CR    R2,RF               EMPTY LINE?                                  
         BE    COVER105            GO MOVE TEXT TO LINE                         
         B     *-20                                                             
*                                                                               
         CH    R4,=H'2'            AT LEAST 2 SPACES ON LINE                    
         BL    COVER120            NO - FLUSH LINE & GET NEW ONE                
*                                                                               
         BCTR  R4,0                LEAVE A SPACE BEFORE INSERTION               
         LA    R2,2(R2)            A(INSERTION POINT)                           
*                                                                               
         CR    R3,R4               TEXT LEN VS SPACE LEFT                       
         BH    COVER110            CAN'T FIT ALL TEXT                           
*                                                                               
COVER105 DS    0H                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)       TEXT TO LINE                                 
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER110 DS    0H                                                               
         LA    R5,0(R4,R5)         MAX DISPL INTO DATA WE CAN TAKE              
         LA    RE,2(R6)                                                         
         CLI   0(R5),C' '                                                       
         BE    *+18                                                             
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         CR    R5,RE                                                            
         BNH   COVER120            DON'T HAVE ANYTHING THAT FITS                
         B     *-18                                                             
*                                                                               
         BCTR  R4,0                DO PARTIAL MOVE TO PRINT LINE                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R6)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R5,1(R5)            START OF REMAINDER OF TEXT                   
         LA    R4,2(R4)            ALLOW FOR PREV EX AND SPACE                  
         SR    R3,R4               LENGTH OF REMAINING TEXT                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)          MOVE REST OF TEXT TO PRINT LINE              
         B     COVER050            GET NEXT ELEM                                
*                                                                               
COVER120 DS    0H                  NEED NEW LINE                                
         GOTO1 REPORT                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)          START NEW LINE                               
         B     COVER050            GET NEXT ELEM                                
                                                                                
*                                                                               
COVER150 DS    0H                  HANDLE NON-AUTOFORMAT LINE                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         GOTO1 REPORT                                                           
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER200 DS    0H                  CLOSE COVERSHEET SECTION                     
         TM    WPCOVFLG,X'80'      AUTO FORMAT ON?                              
         BZ    COVER210                                                         
         CLC   P,SPACES            ANYTHING LEFT ON LINE?                       
         BE    COVER210                                                         
         GOTO1 REPORT                                                           
COVER210 DS    0H                                                               
         LA    RF,COVWIDE                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),=132C'*'                                                    
         GOTO1 REPORT                                                           
         MVC   PAGE,=H'1'                                                       
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
COVERX   XIT1                                                                   
*                                                                               
COVHEAD  DC    C' CONTRACT COVERSHEET '                                         
*                                                                               
***********************************************************************         
* GETCOV - POINTS R6 TO NEXT ELEMENT IN RECORD SET R6=0 WHEN END OF REC         
***********************************************************************         
GETCOV   DS    0H                                                               
         ST    RE,FULL             SAVE RE FOR RETURN                           
*                                                                               
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           NEXT TEXT ELEM                               
         BE    GETCOVX             GOT IT, ALL DONE                             
*                                                                               
         L     R1,ACOVAREA                                                      
         USING RCOVREC,R1                                                       
         MVC   KEY(27),RCOVKEY     GET NEXT COVER RECORD IN SET                 
         ZIC   RE,RCOVKSEQ                                                      
         LA    RE,1(RE)                                                         
         STC   RE,KEY+26                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(27),KEYSAVE     ANOTHER RECORD IN SET?                       
         BE    GETCOV30            YES - PROCESS IT                             
         SR    R6,R6               NO - RETURN R6=0                             
         B     GETCOVX             RETURN                                       
*                                                                               
GETCOV30 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,ACOVAREA,    +        
               DMWORK,0                                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACOVAREA                                                      
         LA    R6,RCOVEL1          1ST ELEM IN REC                              
         CLI   0(R6),3                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
GETCOVX  DS    0H                                                               
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R1                                                               
         B     COVERX                                                           
         LTORG                                                                  
**********************************************************************          
*   ROUTINE TO PRINT CFC COMMENT                                                
**********************************************************************          
PRTCFC   CSECT                                                                  
         NMOD1 0,*PRTCFC*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         BAS   RE,SETCOMBO         GET CORRECT K NUMBER                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,FULL                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         GOTO1 REPORT                                                           
         MVC   P+10(66),=66C'*'                                                 
         MVC   P+29(28),=C' CONFIRMATION WITH COMMENTS '                        
         GOTO1 REPORT                                                           
         TM    RCFCIFLG,X'80'                                                   
         BZ    *+10                                                             
         MVC   P+13(L'MGOYMSG),MGOYMSG                                          
         TM    RCFCIFLG,X'40'                                                   
         BZ    *+10                                                             
         MVC   P+13(L'MGONMSG),MGONMSG                                          
         MVI   P+10,C'*'                                                        
         MVI   P+75,C'*'                                                        
         GOTO1 REPORT                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRTCFC10 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PRTCFC20                                                         
         MVC   P+13(60),2(R6)                                                   
         MVI   P+10,C'*'                                                        
         MVI   P+75,C'*'                                                        
         GOTO1 REPORT                                                           
         B     PRTCFC10                                                         
PRTCFC20 DS    0H                                                               
         MVC   P+10(66),=66C'*'                                                 
         GOTO1 REPORT                                                           
         B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
*  SETCOMBO                                                                     
*  IF COMBO ORDER, RETURNS LOWEST K NUMBER IN COMBO IN 'FULL' ELSE              
*  RETURNS K NUMBER                                                             
***********************************************************************         
SETCOMBO NTR1                                                                   
         MVC   FULL,RCONKCON                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   SCMBX                                                            
*                                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
SCMB20   DS    0H                                                               
         CLC   FULL,0(R5)          FULL VS. CURRENT K?                          
         BL    *+10                FULL IS LOWER - SKIP                         
         MVC   FULL,0(R5)          FULL IS HIGHER - REPLACE W/CURRENT           
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         BCT   R3,SCMB20                                                        
*                                                                               
SCMBX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE RECFCMSG                                                       
         LTORG                                                                  
*********************************************************************           
* RETRIEVE POINT PERSON NAME AS DICTATED BY PROFILE 20                          
*********************************************************************           
PTPNAME  CSECT                                                                  
         NMOD1 0,*PTPNAM*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         MVC   MYSVKEY,KEY                                                      
*                                                                               
         XC    SVPTCDE,SVPTCDE                                                  
         XC    SVPTNAM,SVPTNAM                                                  
         XC    SVPTTEL,SVPTTEL                                                  
*                                                                               
PKEYD    USING RPRDKEY,KEY                                                      
         CLI   OPT2A,C'Y'          PRODUCT RECORD ON FILE?                      
         BNE   PTPX                NO  - SKIP THIS CODE                         
*                                                                               
         XC    KEY,KEY             GET PRODUCT RECORD                           
         MVI   PKEYD.RPRDKTYP,X'09'                                             
         MVC   PKEYD.RPRDKADV,RCONKADV                                          
         MVC   PKEYD.RPRDKPRD,RCONPRD                                           
         MVC   PKEYD.RPRDKREP,REPID                                             
         DROP  PKEYD                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    PKEYD020                                                         
         DC    H'0'                                                             
PKEYD020 EQU   *                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PTPX                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA           SAVE POINT PERSON CODE                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTPX                                                             
         USING RPRDNELM,R6                                                      
         MVC   SVPTCDE,RPRDNPNT                                                 
         DROP  R6                                                               
*                                                                               
PTKEYD   USING RPTPKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   PTKEYD.RPTPKTYP,X'31'                                            
         MVC   PTKEYD.RPTPKREP,REPID                                            
         MVC   PTKEYD.RPTPKREC,SVPTCDE                                          
         DROP  PTKEYD                                                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PTPX                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA       SAVE POINT PERSON NAME AND TEL                   
         USING RPTPREC,R6                                                       
         MVC   SVPTNAM,RPTPNAME                                                 
         MVC   SVPTTEL,RPTPFONE                                                 
         DROP  R6                                                               
*                                                                               
PTPX     DS    0H                                                               
         MVC   KEY,MYSVKEY         ALL DONE, RESTORE ORIGINAL K                 
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),MYSVKEY                                                  
         BE    *+6                                                              
         DC    H'0'                DID NOT FIND ORIGINAL CONTRACT               
*                                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRINT CONTRACT TYPE AND DEVELOPMENT TYPE DESCRIPTION IF ANY                   
* AS DICTATED BY CONTRACT PROFILE 22                                            
*********************************************************************           
TYPEDESC CSECT                                                                  
         NMOD1 0,*TYPDES*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         MVC   MYSVKEY,KEY                                                      
*                                                                               
         CLI   RCONTYPE,0                                                       
         BNE   TYPED10                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEMENT PRESENT?               
         BAS   RE,GETEL                                                         
         BNE   TYPEDXX                                                          
*                                                                               
         USING RCONDVEL,R6                                                      
         CLI   RCONDVCT,0                                                       
         BE    TYPEDXX                                                          
         DROP  R6                                                               
*                                                                               
TYPED10  DS    0H                                                               
TKEYD    USING RCTYKEY,KEY                                                      
         XC    KEY,KEY             GET CONTRACT TYPE                            
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPID                                             
         DROP  TKEYD                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPED30                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA           SAVE DESCRIPTION                             
         USING RCTYREC,R6                                                       
         MVC   SVCTDES,RCTYDESC                                                 
         DROP  R6                                                               
*                                                                               
TYPED30  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEMENT PRESENT?               
         BAS   RE,GETEL                                                         
         BNE   TYPEDX                                                           
*                                                                               
         USING RCONDVEL,R6                                                      
         CLI   RCONDVCT,0                                                       
         BE    TYPEDX                                                           
*                                                                               
DKEYD    USING RDCTKEY,KEY                                                      
         XC    KEY,KEY             GET DEVELOPMENTAL CONTRACT TYPE              
         MVI   DKEYD.RDCTKTYP,X'3B'                                             
         MVC   DKEYD.RDCTKCTY,RCONDVCT                                          
         MVC   DKEYD.RDCTKREP,REPID                                             
         DROP  DKEYD                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPEDX                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA           SAVE DESCRIPTION                             
         USING RDCTREC,R6                                                       
         MVC   SVDTDES,RDCTDESC                                                 
         DROP  R6                                                               
*                                                                               
TYPEDX   DS    0H                                                               
         MVC   KEY,MYSVKEY         ALL DONE, RESTORE ORIGINAL K                 
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),MYSVKEY                                                  
         BE    *+6                                                              
         DC    H'0'                DID NOT FIND ORIGINAL CONTRACT               
*                                                                               
TYPEDXX  DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* FOR TYPE N CONTRACTS ONLY, IF A STANDARD COMMENT CODE IS FOUND                
* PRINT IT IN THE COMMENT SECTION                                               
*********************************************************************           
TYPECMT  CSECT                                                                  
         NMOD1 0,*TYPCMT*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         MVC   MYSVKEY,RCONREC                                                  
*                                                                               
         XC    KEY,KEY             GET CONTRACT TYPE                            
TKEYD    USING RCTYKEY,KEY                                                      
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPID                                             
         DROP  TKEYD                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPENX                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCTYREC,R6                                                       
         CLI   RCTY1LEN,RCTYELMX                                                
         BL    TYPENX              OLD RECORD DO NOT HAS S/C CODE               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'C='                                                   
         MVC   WORK+2(L'RCTYCMMT),RCTYCMMT                                      
         DROP  R6                                                               
*                                                                               
         LA    R8,IOAREA                                                        
*                                                                               
         GOTO1 =V(REGENSTC),DMCB,(X'03',WORK),(R8),DATAMGR,RCONREC,RR=Y         
         BNZ   TYPENX              COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R8),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    TYPENX                                                           
         CLI   0(R8),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    TYPENX                                                           
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
TYPEN10  DS    0H                                                               
         ZIC   R4,0(R8)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),1(R8)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         ZIC   R4,0(R8)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R8,R4                                                            
         CLI   0(R8),X'FF'         IF X'FF', DONE                               
         BE    TYPENX                                                           
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R8                        
         A     R4,=AL4(IOAREA-STORED+L'IOAREA+1)                                
         CR    R4,R8                                                            
         BH    TYPEN10                                                          
*                                                                               
TYPENX   DS    0H                                                               
*                                                                               
         MVC   KEY,MYSVKEY         ALL DONE, RESTORE ORIGINAL K                 
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),MYSVKEY                                                  
         BE    *+6                                                              
         DC    H'0'                DID NOT FIND ORIGINAL CONTRACT               
*                                                                               
         XMOD1                                                                  
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RCONREC,DMWORK                
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONTROL SPECIAL HEADLINES                                          
***********************************************************************         
HEADLINE CSECT                                                                  
         NMOD1 0,**HEAD**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   HEAD1+10(33),PRTRNAM                                             
*                                                                               
         MVC   HEAD2+10(20),PRTRAD1                                             
         MVC   HEAD3+10(34),PRTRAD2                                             
*                                                                               
         MVC   HEAD5+53(L'SVCTDES),SVCTDES                                      
         MVC   HEAD6+53(L'SVDTDES),SVDTDES                                      
*                                                                               
         MVC   HEAD7+5(4),RCONKADV                                              
         MVC   HEAD7+13(30),PRTADVNM                                            
         GOTO1 DATCON,DMCB,(4,RCDATE),(5,HEAD7+41)                              
*                                                                               
         ZAP   DUB,=P'0'                                                        
         OC    COMBONUM,COMBONUM   COMBO ORDER?                                 
         BZ    HL40                                                             
         MVO   DUB+3(5),SVCONNUM   FIRST AND ORIGINAL K                         
         B     HL50                                                             
*                                                                               
HL40     DS    0H                                                               
         MVO   DUB+3(5),RCONKCON                                                
*                                                                               
HL50     DS    0H                                                               
         EDIT  (P5,DUB+3),(8,HEAD7+53)                                          
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD   MOD NUMBER                                   
         CLI   RCONMOD,255                                                      
         BNE   *+8                                                              
         MVI   HALF,X'FF'                                                       
         EDIT  (2,HALF),(3,HEAD7+66),FLOAT=-                                    
         EDIT  (2,PAGE),(3,HEAD7+78)                                            
*                                                                               
         MVI   SOPTIONS,0          CLEAR STATION OPTIONS                        
         LA    R6,RSTAREC                                                       
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8                                                         
         BAS   RE,GETEL                                                         
         BNE   HL60                                                             
         CLI   RSTAOPT7,C'Y'       GET PRINTING OPTION                          
         BNE   HL60                                                             
         OI    SOPTIONS,STAOPT7                                                 
         DROP  R6                                                               
*                                                                               
HL60     DS    0H                                                               
         TM    SOPTIONS,STAOPT7    PRINT ADV, AGY AND TRAF#?                    
         BZ    HL80                                                             
         LA    R6,RCONREC          GET TRAFFIC NUMBER                           
         USING RCONXEL,R6                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL70                                                             
         MVC   HEAD8+59(L'RCONTRF),RCONTRF                                      
         DROP  R6                                                               
HL70     DS    0H                                                               
         LA    R6,RCONREC          GET ADVERTISER                               
         USING RCONXXEL,R6                                                      
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL80                                                             
         MVC   HEAD8+5(L'RCONXADV),RCONXADV                                     
         DROP  R6                                                               
*                                                                               
HL80     DS    0H                                                               
         MVI   HEAD8,0             ALWAYS PRINT AT LEAST A BLANK LINE           
*                                                                               
* GET PRODUCT NAME                                                              
         CLC   RCONPRD,SPACES                                                   
         BE    HL90                                                             
* PRODUCT RECORD USED                                                           
         MVC   P+5(3),RCONPRD                                                   
         CLI   OPT2A,C'Y'          PRODUCT RECORD FOUND?                        
         BNE   HL85                NO                                           
         MVC   P+13(20),RPRDNAME                                                
         B     HL100                                                            
HL85     EQU   *                                                                
         MVC   P+13(20),=C'**CODE NOT ON FILE**'                                
         B     HL100                                                            
* USE PRODUCT ELEMENT                                                           
HL90     LA    R6,RCONELEM                                                      
         MVI   ELCODE,5            LOOK FOR ELEMENT                             
         BAS   RE,FIRSTEL                                                       
         BNE   HL100                                                            
         MVC   P+13(20),2(R6)                                                   
*                                                                               
* THIS ROUTINE WILL BECOME OBSELETE WHEN COMBO-SUPPORT GOES ON-LINE!            
* MOVE STATION - IF COMBINED STATION, PRINT AM AND FM CALL LETTERS              
*                INSTEAD OF -C CALL LETTERS                                     
*                                                                               
HL100    CLI   RCONKSTA+4,C'C'                                                  
         BNE   HL140                                                            
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BE    HL110                                                            
         MVC   P+36(15),=C'COMBO STA ERROR'                                     
         B     HL170                                                            
*                                                                               
HL110    DS    0H                                                               
         USING RSTACSEL,R6                                                      
         LA    R4,P+36                                                          
*                                                                               
HL120    DS    0H                                                               
         MVC   0(4,R4),RSTACS                                                   
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),=C'- M/'                                                 
         MVC   1(1,R4),RSTACS+4                                                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   HL130                                                            
*                                                                               
         LA    R4,4(R4)                                                         
         MVC   0(4,R4),RSTACS                                                   
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),=C'- M'                                                  
         MVC   1(1,R4),RSTACS+4                                                 
         B     HL170                                                            
*                                                                               
HL130    DS    0H                                                               
         MVI   3(R4),C' '                                                       
         B     HL170                                                            
         DROP  R6                                                               
*                                                                               
* IF COMBINED STATION, PRINT '*COMBO*' AND PRINT ALL STATIONS OF                
* THE ORDER AT THE END OF THE HEADING AND BEFORE THE BUYLINES                   
*                                                                               
HL140    DS    0H                                                               
         OC    COMBONUM,COMBONUM   COMBO ORDER?                                 
         BZ    HL150                                                            
*                                                                               
         MVC   P+41(7),=C'*COMBO*'                                              
         B     HL170                                                            
*                                                                               
HL150    MVC   P+41(4),RCONKSTA                                                 
         MVC   P+45(3),=C'-TV'                                                  
         CLI   RCONKSTA+4,C' '                                                  
         BE    HL160                                                            
         MVC   P+46(2),=C'AM'                                                   
         CLI   RCONKSTA+4,C'A'                                                  
         BE    HL160                                                            
         MVC   P+46(2),=C'FM'                                                   
         CLI   RCONKSTA+4,C'F'                                                  
         BE    HL160                                                            
         MVC   P+46(2),=C'L '                                                   
*                                                                               
HL160    CLI   P+44,C' '                                                        
         BNE   *+14                                                             
         MVC   P+44(3),P+45                                                     
         MVI   P+47,C' '                                                        
HL170    MVC   P+53(L'RSTAMKT),RSTAMKT                                          
         SPACE 1                                                                
         TM    RSTASTAT,X'08'      08=NO CONTRACT TO STATION                    
         BO    *+10                                                             
         MVC   P+77(3),=C'***'       STARS INDICATE COPY TO STATION             
*                                                                               
*                                                                               
         TM    PROFILES+CNTPRN1B,CNTPRN1A   PRINT AFFL?                         
         BZ    *+10                NO                                           
         MVC   PSECOND+41(3),RSTAAFFL AFFILIATION                               
*                                                                               
         MVC   PTHIRD+51(4),RCONKAGY                                            
         CLC   RCONKAOF,SPACES                                                  
         BE    HL290                                                            
         MVI   PTHIRD+55,C'-'                                                   
         MVC   PTHIRD+56(2),RCONKAOF                                            
         CLI   PTHIRD+53,C' '                                                   
         BNE   HL280                                                            
         MVC   PTHIRD+53(3),PTHIRD+55                                           
         MVC   PTHIRD+56(2),SPACES                                              
         B     HL290                                                            
HL280    CLI   PTHIRD+54,C' '                                                   
         BNE   HL290                                                            
         MVC   PTHIRD+54(3),PTHIRD+55                                           
         MVI   PTHIRD+57,C' '                                                   
*                                                                               
* DISPLAY RATING SERVICE                                                        
HL290    DS    0H                                                               
         TM    PROFILES+CNTPRN2B,CNTPRN2A   PRINT RTG SRV, BOOK & DEMO?         
         BZ    HL320               NO                                           
*                                                                               
         LA    RE,RTGTBL                                                        
HL292    EQU   *                                                                
         CLC   RCONRTGS,0(RE)                                                   
         BE    HL294                                                            
         LA    RE,L'RTGTBL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   HL292                                                            
         MVC   PTHIRD+59(3),=C'NSI'  NOT FOUND: FORCE NSI                       
         B     HL296                                                            
HL294    EQU   *                                                                
         MVC   PTHIRD+59(3),0(RE)                                               
HL296    EQU   *                                                                
*                                                                               
* OPEN CTFILE FOR DEMOCON                                                       
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
*                                                                               
* PRINT FIRST DEMO (OR PRIME DEMO) AND FIRST BOOK                               
         LA    R4,IOAREA                                                        
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
*                                                                               
         LA    R5,WORK2                                                         
         XC    WORK2(30),WORK2                                                  
*                                                                               
         CLI   RCONKSTA+4,C' '     TV???                                        
         BE    HL300                                                            
         CLI   RCONKSTA+4,C'T'     TV???                                        
         BE    HL300                                                            
*                                                                               
* RADIO                                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'        BOP ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   HL320               NO BOP, NO DEMO                              
         USING RCONBPEL,R6                                                      
         MVC   PTHIRD+73(3),RCONBPMK TSA/ADI/MSA                                
         MVC   PTHIRD+77(6),RCONBBKS                                            
*                                                                               
         CLC   RCONBBKS,SPACES     NOW SUFFIX BOOK W/BOOK TYPE                  
         BE    HL289                                                            
         CLI   RCONBBKT,0                                                       
         BE    HL289                                                            
         CLI   RCONBBKT,C' '                                                    
         BE    HL289                                                            
         OC    PTHIRD+77(6),SPACES                                              
         LA    RF,PTHIRD+83                                                     
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),RCONBBKT                                                 
*                                                                               
HL289    DS    0H                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BE    *+14                YES                                          
         MVC   PTHIRD+63(8),RCONBPDM DISPLAY UN-VALIDATED (OLD) DEMO            
         B     HL320                                                            
*                                                                               
         MVI   DBSELMED,C'R'                                                    
         MVC   0(L'RCONBPDM-1,R5),RCONBPDM+1     DEMOS + ENDING ZERO            
         DROP  R6                                                               
*                                                                               
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 VDEMOCON,DMCB,(1,(R5)),(9,PTHIRD+63),(0,DBLOCKD),0               
         B     HL320                                                            
*                                                                               
* TV                                                                            
HL300    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        SAR ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   HL320               NO SAR, NO DEMO OR BOOKS                     
         USING RSAREL,R6                                                        
NEWSAR   USING RSARXEL,R6          USE THE NEW ELEMENT DSECT ALSO               
*                                                                               
* IF PRIME DEMO, USE IT, ELSE USE FIRST DEMO                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   0(L'RSARDEM,R5),RSARDEM                                          
         LA    RF,6                                                             
         TM    0(R5),X'40'                                                      
         BNZ   *+16                FOUND PRIME                                  
         LA    R5,3(R5)            NEXT DEM                                     
         BCT   RF,*-12                                                          
         LA    R5,WORK2                                                         
*                                                                               
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 VDEMOCON,DMCB,(1,(R5)),(9,PTHIRD+63),(0,DBLOCKD),0               
*                                                                               
         CLC   RSARBKS(2),=C'DR'                                                
         BNE   HL310                                                            
         MVC   PTHIRD+73(2),=C'DR'                                              
         B     HL320                                                            
         SPACE 1                                                                
HL310    DS    0H                                                               
*                                                                               
* SET UP DUMMY FIELD FOR UNBOOK                                                 
         XC    DFLDH,DFLDH                                                      
         XC    DFLDD,DFLDD                                                      
         MVI   DFLDH,X'10'                                                      
*                                                                               
         CLI   NEWSAR.RSARXLEN,RSARXLTH                                         
         BL    HL315                                                            
         TM    NEWSAR.RSARXFLG,X'04'                                            
         BZ    HL315                                                            
         DROP  R6,NEWSAR                                                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL312                                                            
         CLC   2(5,R6),=X'4040404040'                                           
         BE    HL312                                                            
         MVC   PTHIRD+73(5),2(R6)                                               
         B     HL320                                                            
HL312    MVI   ELCODE,X'40'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    2(2,R6),2(R6)                                                    
         BZ    HL313                                                            
         MVC   PTHIRD+73(5),2(R6)                                               
         B     HL320                                                            
HL313    GOTO1 =V(UNBOOK),DMCB,(1,4(R6)),DFLDH,0,0,RR=YES                       
         MVC   PTHIRD+73(L'DFLDD-2),DFLDD+2   IGNORE RATING SRVC                
         B     HL320                                                            
*                                                                               
HL315    XC    DMCB+8(8),DMCB+8                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         ST    R6,DMCB+8                                                        
         MVI   DMCB+8,C'L'         YES-SET LABEL OPTION                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        RESTORE R6 TO SAR ELEM                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSAREL,R6                                                        
         GOTO1 =V(UNBOOK),DMCB,(1,RSARBKS),DFLDH,,0                             
         MVC   PTHIRD+73(L'DFLDD-2),DFLDD+2 IGNORE RATING SRVC                  
         DROP  R4                                                               
         DROP  R6                                                               
*                                                                               
HL320    DS    0H                                                               
         MVI   PSECOND,0           MAKE SURE THESE LINES PRINT IF BLANK         
         MVI   PTHIRD,0                                                         
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+5(3),PRTSALCD                                                  
         MVC   P+13(20),PRTSALNM                                                
         MVC   PSECOND+13(20),PRTSALPH                                          
*                                                                               
         TM    SOPTIONS,STAOPT7    PRINT ADV, AGY AND TRAF#?                    
         BZ    HL330                                                            
         LA    R6,RCONREC          GET AGENCY                                   
         USING RCONXXEL,R6                                                      
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL330                                                            
         MVC   P+41(L'RCONXAGY),RCONXAGY                                        
         DROP  R6                                                               
HL330    DS    0H                                                               
         MVI   PTHIRD,0                                                         
         MVI   PFOURTH,0                                                        
         TM    PRTFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+10                NO                                           
         MVC   PFOURTH+46(L'PRTADVNM),PRTADVNM                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+13(L'PRTRRNAM),PRTRRNAM                                        
*                                                                               
* GET EASI ELEM                                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
* WILL JUST RESULT IN SPACES BEING MOVED FOR EASI CODES IN LATER                
* LINES - I WON'T HAVE TO KEEP CHECKING TO SEE IF THEY REALLY EXIST.            
         LA    R6,SPACES           DIRTY TRICK, BUT IT WILL WORK...             
         USING RCONIEL,R6                                                       
*                                                                               
* DISPLAY EI ADV                                                                
*        MVC   PFOURTH+2(3),=C'ADV'                                             
*        MVC   PFOURTH+6(L'RCONIADV),RCONIADV                                   
*                                                                               
         MVC   PSECOND+5(2),RCONKOFF                                            
         MVC   PSECOND+13(L'ROFFNAME),ROFFNAME                                  
         CLC   RCONKOFF,REQOFF                                                  
         BE    *+16                                                             
         MVC   PSECOND+35(4),=C'(  )'                                           
         MVC   PSECOND+36(2),REQOFF                                             
*                                                                               
         LA    RE,P+46                                                          
         TM    PRTFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+14                NO                                           
         MVC   0(04,RE),=C'C/O '                                                
         LA    RE,4(RE)                                                         
*                                                                               
         MVC   0(33,RE),PRTANAM            AGENCY NAME                          
*                                                                               
         TM    PRTFMTFL,X'80'           BUYER FIELD REPLACED                    
         BO    HL390                    YES - DONT PRINT LABEL                  
         MVC   PSECOND+46(12),=C'MEDIA BUYER-'                                  
         MVC   PSECOND+59(20),PRTBUYER                                          
         B     *+10                                                             
HL390    MVC   PSECOND+46(20),PRTBUYER                                          
*                                                                               
         MVC   PTHIRD+46(34),PRTAAD1                                            
         MVC   PFOURTH+46(36),PRTAAD2                                           
*                                                                               
*    DISPLAY POINT PERSON NAME                                                  
*                                                                               
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    HL391               IF OFF SKIP POINT PERSON                     
*                                                                               
         GOTO1 =A(PTPNAME),DMCB,(RC),(RA),(R9),RR=Y                             
*                                                                               
         CLC   RCONPRD,=C'   '     PROD CODE OR PRODUCT LITERAL ?               
         BE    HL391               NO--DONE                                     
*                                                                               
         CLI   SVPTCDE,0           IS THERE A POINT PERSON CODE                 
         BE    HL391               NO--DONE                                     
*                                                                               
         MVC   PTHIRD+5(3),=C'PTP'                                              
         MVC   PTHIRD+13(20),SVPTNAM                                            
*                                                                               
HL391    MVI   PSECOND,0                FORCE LINES TO PRINT                    
         MVI   PTHIRD,0                                                         
         MVI   PFOURTH,0                                                        
         MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+46(36),PRTAAD3                                                 
*                                                                               
* DISPLAY EI PRD & EST                                                          
HL400    DS    0H                                                               
*        MVC   P+2(3),=C'PRD'                                                   
*        MVC   P+6(L'RCONIPRD),RCONIPRD                                         
*        MVI   SPACING,1                                                        
*        GOTO1 REPORT                                                           
*        MVC   P+2(3),=C'EST'                                                   
*        MVC   P+6(L'RCONIEST),RCONIEST                                         
         DROP  R6                                                               
*                                                                               
         L     RF,ADCONLST        BUILD RFBLOCK                                 
         USING ADCONSD,RF                                                       
         MVC   DUB(4),VCOMFACS    A(COMFACS)                                    
         MVC   DUB+4(2),RCONKREP                                                
         DROP  RF                                                               
         GOTOX (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),PSECOND+15,0,DUB          
*                                                                               
         EDIT  (1,RCONWKS),(2,PSECOND+34)                                       
         MVI   SPACING,1                                                        
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
*                                                                               
         OC    COMBONUM,COMBONUM                                                
         BZ    HLX                                                              
*                                                                               
         LA    RE,4                AT MOST 4 COMPONENT STATIONS                 
         LA    RF,COMBOSC          POINT TO FIRST STATION TO PRINT              
         LA    R1,P+15             POINT TO OUTPUT LINE                         
*                                                                               
HL410    OC    0(L'COMBOS1,RF),0(RF)                                            
         BZ    HL420                                                            
         MVC   0(4,R1),0(RF)       CALL LETTER                                  
         MVI   4(R1),C'-'                                                       
         MVC   5(1,R1),4(RF)       BAND                                         
*                                                                               
         LA    RF,9(RF)                                                         
         LA    R1,8(R1)                                                         
         BCT   RE,HL410                                                         
*                                                                               
HL420    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
HLX      DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*** ROUTINE STORAGE ***                                                         
SAVER6   DS    F                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT PRINTED CONTRACT FIELDS PER CONTYPE FORMAT RECORD                      
***********************************************************************         
FMT      CSECT                                                                  
         NMOD1 0,**FMT***                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
* DEFAULT VALUES FOR PRINTED FIELDS                                             
*                                                                               
         MVI   PRTFMTFL,0               INITIALIZE FLAGS                        
         MVI   FMTFLK,0                                                         
*                                                                               
         MVC   PRTRNAM,RREPNAME         REP NAME                                
         MVC   PRTRAD1,ROFFADD1         REP ADDRESS 1                           
         XC    PRTRAD2,PRTRAD2                                                  
         MVC   PRTRAD2(18),ROFFADD2     REP ADDRESS 2                           
         LA    R4,PRTRAD2+18            FLOAT STATE AND ZIP                     
         OC    PRTRAD2,SPACES                                                   
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(2,R4),ROFFSTT                                                  
         MVC   5(10,R4),ROFFZIP                                                 
*                                                                               
         XC    PRTRRNAM,PRTRRNAM        REAL REP NAME FIELD                     
         XC    PRTADVNM,PRTADVNM                                                
         MVC   PRTADVNM(L'RADVNAME),RADVNAME                                    
         MVC   PRTBUYER,RCONBUYR        BUYER NAME                              
         MVC   PRTSALCD,RCONSAL         SALESMAN CODE                           
         MVC   PRTSALNM,RSALNAME                                                
         XC    PRTSALPH,PRTSALPH                                                
         MVC   PRTSALPH(12),RSALTEL                                             
*                                                                               
         XC    SVCTDES,SVCTDES     CLEAR TYPE DESCRIPTION                       
         XC    SVDTDES,SVDTDES     CLEAR DEV TYPE DESCRIPTION                   
*                                                                               
         MVC   PRTANAM,RAGYNAM2                                                 
         XC    PRTAAD1,PRTAAD1                                                  
         XC    PRTAAD2,PRTAAD2                                                  
         XC    PRTAAD3,PRTAAD3                                                  
         MVC   PRTAAD1(20),RAGYADD1                                             
         MVC   PRTAAD3(20),RAGYCITY                                             
*                                                                               
         TM    RAGYFLAG,X'80'           EXPANDED ADDRESS?                       
         BZ    FMT090                                                           
*                                                                               
         MVC   SVBUYK2,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGY2KEY,R6                                                      
         MVI   RAGK2TYP,X'1A'                                                   
         MVC   RAGK2AGY,RAGYKAGY                                                
         MVC   RAGK2AOF,RAGYKAOF                                                
         MVC   RAGK2REP,RAGYKREP                                                
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FMT050                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   FMT050                                                           
         USING RAGY2AE1,R6                                                      
         MVC   PRTAAD1,RAGY2AD1                                                 
         MVC   PRTAAD2(34),RAGY2AD2                                             
         DROP  R6                                                               
FMT050   DS    0H                                                               
         MVC   KEY,SVBUYK2         RE-ESTABLISH SEQ ORDER                       
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED REC              
         CLC   PRTAAD3(20),PRTAAD2                                              
         BNE   *+10                                                             
         MVC   PRTAAD3,SPACES                                                   
*                                                                               
* FLOAT STATE AND ZIP                                                           
FMT090   LA    R4,PRTAAD3+35                                                    
         OC    PRTAAD3(36),SPACES                                               
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(2,R4),RAGYSTAT                                                 
         MVC   5(10,R4),RAGYZIP                                                 
*                                                                               
         OC    PRTAAD2,SPACES                                                   
         CLC   PRTAAD2,SPACES                                                   
         BNE   FMT100                                                           
         MVC   PRTAAD2,PRTAAD3                                                  
         MVC   PRTAAD3,SPACES                                                   
FMT100   DS    0H                                                               
*                                                                               
*  SPECIAL CASE - KATZ CONVERTED CONTRACT - AGY NAME & ADDR IN CONREC           
*                                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT?                     
         BNO   FMT175                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'70'        AGENCY NAME ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT110              NOT FOUND - EXIT                             
         XC    PRTANAM,PRTANAM                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   PRTANAM(0),2(R6)                                                 
FMT110   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'71'        FIRST ADDRESS ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   FMT120              NOT FOUND - EXIT                             
         XC    PRTAAD1,PRTAAD1                                                  
         XC    PRTAAD2,PRTAAD2                                                  
         XC    PRTAAD3,PRTAAD3                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   PRTAAD1(0),2(R6)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'72'        SECOND ADDRESS ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT120              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   PRTAAD2(0),2(R6)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'73'        MAY BE A THIRD ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT120              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   PRTAAD3(0),2(R6)                                                 
FMT120   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'6F'        ADVERTISER NAME ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   FMT130              NOT FOUND - EXIT                             
         XC    PRTADVNM,PRTADVNM                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   PRTADVNM(0),2(R6)                                                
FMT130   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'74'        SALESPERSON NAME ELEMENT                     
         BAS   RE,GETEL            (PUT INTO POINT PERSON)                      
         BNE   FMT175              NOT FOUND - EXIT                             
         XC    SVPTNAM,SVPTNAM                                                  
         XC    SVPTTEL,SVPTTEL                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   SVPTNAM(0),2(R6)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'75'        SALESPERSON PHONE# ELEMENT                   
         BAS   RE,GETEL            (PUT INTO POINT PERSON PHONE)                
         BNE   FMT140              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   SVPTTEL(0),2(R6)                                                 
FMT140   DS    0H                       SPECIAL CASE - FOR CONVERTED            
         CLI   RCONTYPE,C'N'            TYPE N CONTRACTS, ALWAYS REPL           
         BNE   FMT175                   SAL NAME & PHONE                        
         MVC   PRTSALNM,SVPTNAM         REPL SAL NAME W/PT PERS NAME            
         MVC   PRTSALPH,SVPTTEL         REPL SAL PH# W/PT PERS PHONE            
         XC    PRTSALCD,PRTSALCD                                                
FMT175   DS    0H                                                               
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         MVC   SVBUYK2,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R6),RCTYKTYQ     REC TYPE                                      
         MVC   24(2,R6),RCONKREP     REP CODE                                   
         MVC   26(1,R6),RCONTYPE     CON TYPE                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FMT700                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'             FORMAT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT700                                                           
         LR    R4,R6                                                            
         USING RCTYFEL,R4                                                       
         MVC   FMTFLK,RCTYFPRC         SAVE OFF PROFILE BYTE                    
*                                                                               
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   FMT180                   YES                                     
*                                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT180                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    PRTFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
         B     FMT260                   SKIP CONTYPE AGENCY OVERRIDES           
*                                                                               
FMT180   DS    0H                                                               
         TM    RCTYFA1S,X'80'           REPLACE AGY ADDRESS 1?                  
         BNO   FMT200                   NO - NEXT FIELD                         
         XC    PRTAAD1,PRTAAD1                                                  
         MVI   TEMP,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT200                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTAAD1(0),3(R6)                                                 
FMT200   DS    0H                                                               
         TM    RCTYFA2S,X'80'           REPLACE AGY ADDRESS 2?                  
         BNO   FMT230                   NO - NEXT FIELD                         
         XC    PRTAAD2,PRTAAD2                                                  
         MVI   TEMP,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT230                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTAAD2(0),3(R6)                                                 
FMT230   DS    0H                                                               
         TM    RCTYFA3S,X'80'           REPLACE AGY ADDRESS 3?                  
         BNO   FMT240                   NO - NEXT FIELD                         
         XC    PRTAAD3,PRTAAD3                                                  
         MVI   TEMP,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT240                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTAAD3(0),3(R6)                                                 
FMT240   DS    0H                                                               
         TM    RCTYFANS,X'80'           REPLACE AGY NAME?                       
         BNO   FMT250                   NO - NEXT FIELD                         
         XC    PRTANAM,PRTANAM                                                  
         MVI   TEMP,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT250                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTANAM(0),3(R6)                                                 
FMT250   DS    0H                                                               
         TM    RCTYFABS,X'80'           REPLACE BUYER NAME?                     
         BNO   FMT260                   NO - NEXT FIELD                         
         OI    PRTFMTFL,X'80'           SET BUYER REPLACED FLAG                 
         XC    PRTBUYER,PRTBUYER                                                
         MVI   TEMP,C'F'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT260                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTBUYER(0),3(R6)                                                
*                                                                               
FMT260   DS    0H                                                               
         MVC   PRTRRNAM,PRTRNAM         DEFAULT TEXT - ACTUAL REP NAME          
         TM    RCTYFRRS,X'80'           REPL REAL REP NAME FIELD?               
         BNO   FMT270                   NO - NEXT FIELD                         
         XC    PRTRRNAM,PRTRRNAM                                                
         MVI   TEMP,C'D'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT270                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTRRNAM(0),3(R6)                                                
FMT270   DS    0H                                                               
         TM    RCTYFR1S,X'80'           REPLACE REP ADDRESS 1?                  
         BNO   FMT280                   NO - NEXT FIELD                         
         XC    PRTRAD1,PRTRAD1                                                  
         MVI   TEMP,C'B'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT280                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTRAD1(0),3(R6)                                                 
FMT280   DS    0H                                                               
         TM    RCTYFR2S,X'80'           REPLACE REP ADDRESS 2?                  
         BNO   FMT290                   NO - NEXT FIELD                         
         XC    PRTRAD2,PRTRAD2                                                  
         MVI   TEMP,C'C'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT290                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTRAD2(0),3(R6)                                                 
FMT290   DS    0H                                                               
         TM    RCTYFRNS,X'80'           REPLACE REP NAME?                       
         BNO   FMT300                   NO - NEXT FIELD                         
         XC    PRTRNAM,PRTRNAM                                                  
         MVI   TEMP,C'A'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT300                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTRNAM(0),3(R6)                                                 
FMT300   DS    0H                                                               
         DROP  R4                                                               
*                                                                               
*                                                                               
* HANDLE FORMAT OPTION BITS HERE                                                
*                                                                               
         TM    FMTFLK,X'80'             OPTION #1 - REPL SALP W/PT PERS         
         BZ    FMT650                                                           
         CLC   RCONPRD,SPACES                                                   
         BE    FMT650                                                           
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT?                     
         BO    FMT610              YES - ALREADY HAVE PT PERS INFO              
         GOTO1 =A(PTPNAME),DMCB,(RC),(RA),(R9),RR=Y                             
FMT610   MVC   PRTSALNM,SVPTNAM                                                 
         MVC   PRTSALCD,SVPTCDE                                                 
         MVC   PRTSALPH,SVPTTEL                                                 
*                                                                               
FMT650   DS    0H                                                               
         TM    FMTFLK,X'40'             OPTION #2  - DISP CON/DEV TYPE          
         BZ    FMT702                                                           
         GOTO1 =A(TYPEDESC),DMCB,(RC),(RA),(R9),RR=Y                            
         B     FMT702                                                           
FMT700   DS    0H                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT702                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    PRTFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
*                                                                               
FMT702   DS    0H                                                               
         BAS   RE,REPLADDR                                                      
*                                                                               
* RESTORE READ SEQ ORDER                                                        
*                                                                               
         MVC   KEY,SVBUYK2                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
                                                                                
FMTYES   CR    RB,RB                                                            
         B     FMTX                                                             
FMTNO    DS    0H                                                               
         LTR   RB,RB                                                            
FMTX     DS    0H                                                               
         XIT1                                                                   
*----------------------------                                                   
TXTSEEK  DS    0H                                                               
         LR    R0,RE                                                            
         MVI   ELCODE,X'12'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
TS010    BNE   TSNO                                                             
         CLC   2(1,R6),TEMP                                                     
         BE    TSYES                                                            
         BAS   RE,NEXTEL                                                        
         B     TS010                                                            
*                                                                               
TSNO     SR    R1,R1                                                            
         CR    R1,RB                                                            
         B     *+6                                                              
TSYES    CR    R1,R1                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*----------------------------                                                   
* CHECK ADV REC FOR REPLACEMENT AGY ADDRESS                                     
*----------------------------                                                   
REPLADDR NTR1                                                                   
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKADV,RCONKADV                                              
         MVC   K.RADVKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         L     RF,ADCONLST        BUILD RFBLOCK                                 
         USING ADCONSD,RF                                                       
         MVC   DUB(4),VCOMFACS    A(COMFACS)                                    
         MVC   DUB+4(2),RCONKREP                                                
         DROP  RF                                                               
         GOTOX (RFGETREC,VREPFACS),DMCB,KEY,IOAREA,0,DUB                        
         BE    REPL0040                                                         
*                                  RECORD NOT FOUND:                            
         CLI   FULL-1,C'Y'         DON'T DUMP OPTION SET?                       
         BE    FMTYES              YES - TREAT AS 'NO ADDRESS'                  
         DC    H'0'                                                             
REPL0040 EQU   *                                                                
         MVC   FULL(1),RCONTYPE                                                 
         MVC   FULL+1(2),RCONKOFF                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'20',IOAREA),(3,FULL)+        
               ,0,RR=Y                                                          
         CLI   12(R1),0            GOT IT?                                      
         BNE   FMTYES              NO ADDRESS                                   
*                                                                               
         ZICM  R6,13(R1),3         ELEMENT                                      
R        USING RADVAGEL,R6                                                      
         OI    PRTFMTFL,X'80'           SET BUYER REPLACED FLAG                 
         XC    PRTBUYER,PRTBUYER                                                
         XC    PRTAAD1,PRTAAD1                                                  
         XC    PRTAAD2,PRTAAD2                                                  
         XC    PRTAAD3,PRTAAD3                                                  
         MVC   PRTAAD1(34),R.RADVAGA1                                           
         MVC   PRTAAD2(34),R.RADVAGA2                                           
         MVC   PRTAAD3(36),R.RADVAGA3                                           
         B     FMTYES                                                           
         DROP  R                                                                
*----------------------------                                                   
* ROUTINE READS AGENCY RECORD AND RETURNS CC EQUAL IF THE IN CARE OF            
*  FLAG IS ON                                                                   
*                                                                               
*  ROUTINE GETS AN AIO AREA IN WORKING STORAGE AND POINTS R6 AT IT              
*----------------------------                                                   
COAGY    NTR1  WORK=(R6,2000/8)                                                 
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,RCONKAGY                                              
         MVC   K.RAGYKAOF,RCONKAOF                                              
         MVC   K.RAGYKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   FMTNO                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,(R6),DMWORK                   
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING RAGYREC,R6                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    FMTNO               NO                                           
         B     FMTYES              YES                                          
         DROP  R                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PATTERN                                                      
***********************************************************************         
PATTERN  CSECT                                                                  
         NMOD1 0,*PATTERN                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         LA    R2,3                                                             
P10      MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   HEAD1+8(15),=C'3 TEST PATTERNS'                                  
         MVI   HEAD12,0                                                         
         LA    R4,10                                                            
P15      MVC   P+41(41),X                                                       
         MVC   P+53(16),=C'  CENTER  THIS  '                                    
         GOTO1 REPORT                                                           
         BCT   R4,P15                                                           
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R4,10                                                            
P20      MVC   P+3(79),LINEPTN                                                  
         GOTO1 REPORT                                                           
         BCT   R4,P20                                                           
         BCT   R2,P10                                                           
         XMOD1                                                                  
LINEPTN  DC    C'XX XXX XXXXXXXXXXXX XXXXXXXXXXX XXX XXXXXXXXXXXX XXX XX        
               XX XXX XXX XXXXXXXXXX XXX'                                       
X        DC    50C'X'                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF CONTRACT IS A COMBO ORDER                                            
***********************************************************************         
CHKCOMBO CSECT                                                                  
         NMOD1 0,*CHKCMB*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         XC    COMBONUM,COMBONUM   COMBO CONTRACT INDICATOR/NUMBER              
*                                                                               
         LA    R6,RCONREC          PRESENCE OF A 17 ELEMENT MEANS THE           
         USING RCONCBEL,R6                                                      
         MVI   ELCODE,X'17'        CONTRACT IS A COMBO CONTRACT                 
         BAS   RE,GETEL                                                         
         BNE   CHKCOMX                                                          
*                                                                               
         XC    COMBOSC,COMBOSC                                                  
         ZIC   R1,RCONCBLN         AREA GETS USED BY OTHER CONTRACTS            
         SH    R1,=H'3'            OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COMBOSC(0),RCONCBST                                              
*                                                                               
         ZIC   RF,RCONCBLN         GET LENGTH OF ELEMENT                        
         S     RF,=F'2'            SUBT ELEMENT CODE AND LENGTHBYTES            
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY LENGTH OF ONE COMBO ENTRY          
         STC   RF,COMBONUM         =NUMBER OF COMBO COMPONENTS                  
         DROP  R6                                                               
*                                                                               
CHKCOMX  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ONLY COMBO ORDER SHOULD CALL THIS ROUTINE.  THIS ROUTINE WILL LOOP  *         
* THRU ALL MEMBER CONTRACTS OF THE COMBO ORDER AND CALLS A COMBO BUY  *         
* PRINTING ROUTINE TO PRINT ALL COMBO BUYS.                           *         
* *WARNING* THIS ROUTINE CHANGES BUT RESTORES THE KEY SEQUENCE USE    *         
*           BY REPORTER.  ALSO, MODE MUST BE SET TO CONLAST WHEN WE   *         
*           EXIT THIS ROUTINE.                                        *         
***********************************************************************         
PCOMBO   CSECT                                                                  
         NMOD1 0,*PCOMBO*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
*                                                                               
         L     R1,AGRAND                                                        
         MVC   0(2,R1),=X'0002'    INIT GRAND TOTAL STORAGE                     
         L     R1,ATGRAND                                                       
         MVC   0(2,R1),=X'0002'    INIT GRAND TOTAL STORAGE                     
*                                                                               
         MVC   SVCONKEY,RCONREC    SAVE FIRST COMBO K KEY                       
         MVI   PRTCMBPT,1          INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R2,COMBOC1          SET TO FIRST COMPONENT K#                    
*                                                                               
PCOMBO10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
*                                                                               
         MVI   MODE,PROCBUY        MODE GETS RESET BY BUYPRT                    
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),0(4,R2)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPID                                                   
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BNZ   PCOMBO60            THEN, SKIP THIS CONTRACT                     
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RCONREC,DMWORK                
         TM    DMCB+8,X'FF'                                                     
         BNZ   PCOMBO60            THEN, SKIP THIS CONTRACT                     
*                                                                               
PCOMBO40 DS    0H                                                               
*                                                                               
         MVC   P+12(4),RCONKSTA    PRINT COMPONENT STATIONS                     
         MVI   P+16,C'-'                                                        
         MVC   P+17(1),RCONKSTA+4                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(7,P+27),ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,DOBUY            GO PRINT COMBO BUYLINES                      
*                                                                               
         TM    RCONMODR+1,X'20'    MON?                                         
         BZ    PCOMBO43            NO BUYS, DO TOTAL HERE                       
         GOTO1 REGENBUF,DMCB,RCONREC,ABUCKS                                     
*                                                                               
PCOMBO43 DS    0H                                                               
         L     R3,AGRAND           CASH GRAND TOTALS                            
         L     R4,ABUCKS           CASH STATION TOTALS                          
         GOTO1 =A(TOTLTOTL),RR=Y   ACCUMULATE GRAND TOTALS                      
*                                                                               
         L     R3,ATGRAND          TRADE GRAND TOTALS                           
         L     R4,ATBUCKS          TRADE STATION TOTALS                         
         GOTO1 =A(TOTLTOTL),RR=Y   ACCUMULATE GRAND TOTALS                      
*                                                                               
PCOMBO50 DS    0H                   RE-INITILIZE FOR BUY PROCESSING             
         L     R1,ATBUCKS                                                       
         MVC   0(2,R1),=H'2'                                                    
         L     R1,ABUCKS                                                        
         MVC   0(2,R1),=H'2'                                                    
* BLOCK+400 IS USED TO COMPARE CONTRACT BUCKETS AGAINST BUY BUCKETS             
         XC    BLOCK+400(256),BLOCK+400                                         
         XC    FLTFLDS,FLTFLDS                                                  
         MVC   POINTER,VSORTA      INIT PTR TO FLIGHT ENTRIES                   
*                                                                               
PCOMBO60 ZIC   RF,PRTCMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,PRTCMBPT                                                      
*                                                                               
         CLC   PRTCMBPT,COMBONUM   WE HAVE THIS MANY COMBO K'S TO DO            
         BH    PCOMBO70                                                         
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
         B     PCOMBO10                                                         
*                                                                               
PCOMBO70 DS    0H                                                               
         BAS   RE,GRANDPRT         PRINT THE GRAND TOTAL                        
         OI    CMBSTAT,CMBDONE     SET DONE FLAG                                
*                                                                               
         MVC   KEY,SVCONKEY        ALL DONE, RESTORE ORIGINAL COMBO K           
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         CLC   KEY(27),SVCONKEY                                                 
         BE    *+6                                                              
         DC    H'0'                DID NOT FIND ORIGINAL CONTRACT               
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RCONREC,DMWORK                
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
PCOMBOX  DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETRIEVES ALL BUYS OF A COMBO ORDER AND PASSES THEM TO *         
* ROUTINE BUYPRT FOR PROCESSING.  'MODE' IS SET TO 'PROCBUY' AND THEN *         
* 'CONLAST' FOR EACH PASS TO BUYPRT TO SIMULATE CALLS FROM REPORTER.  *         
***********************************************************************         
DOBUY    NTR1                                                                   
         LA    R5,KEY                                                           
         USING RCONKEY,R5          GET K#, REVERSE 9'S COMPL.                   
         PACK  WORK(1),RCONPCON+3(1)                                            
         PACK  WORK+1(1),RCONPCON+2(1)                                          
         PACK  WORK+2(1),RCONPCON+1(1)                                          
         PACK  WORK+3(1),RCONPCON(1)                                            
         DROP  R5                                                               
*                                                                               
         LA    R5,KEY                                                           
         USING RBUYKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPID                                                   
         MVC   RBUYKCON,WORK                                                    
         DROP  R5                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    DOBUY20                                                          
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
DOBUY10  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
*                                                                               
DOBUY20  CLC   KEY(22),KEYSAVE                                                  
         BNE   DOBUYX              NO MORE BUYLINES                             
*                                                                               
         TM    KEY+27,X'C0'        VOID                                         
         BO    DOBUY10                                                          
*                                                                               
         MVC   SVBUYKEY,KEY                                                     
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,RBUYREC,     X        
               DMWORK                                                           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(BUYPRT),DMCB,(RC),(RA),(R9),RR=Y                              
*                                                                               
         MVC   KEY,SVBUYKEY        RE-ESTABLISH SEQ ORDER                       
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    DOBUY10                                                          
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
DOBUYX   DS    0H                                                               
         MVI   MODE,CONTLAST                                                    
         GOTO1 =A(BUYPRT),DMCB,(RC),(RA),(R9),RR=Y                              
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT THE GRAND TOTAL - COMBO ONLY                                  *         
***********************************************************************         
GRANDPRT NTR1                                                                   
         CLI   OPTION1,C'N'        STN/AGY COPY?                                
         BE    GRANDX                                                           
*                                                                               
* PRINT CASH TOTALS                                                             
*                                                                               
         L     R1,AGRAND                                                        
         CLC   =X'0002',0(R1)      ANY CASH GRAND TOTALS?                       
         BE    GRAND100                                                         
         GOTO1 REGENTL2,DMCB,AGRAND,BLOCK  FORMAT CASH                          
         SR    R2,R2                                                            
         IC    R2,DMCB+4           NUMBER OF LINES                              
         SR    R6,R6                                                            
         IC    R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    GRAND20                                                          
         BP    GRAND10                                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     GRAND20                                                          
*                                                                               
GRAND10  STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
GRAND20  DS    0H                                                               
         MVC   P+2(13),=C'COMBO TOTALS:'                                        
         L     R1,ATGRAND          TRADE BUCKETS ALSO?                          
         CLC   =X'0002',0(R1)                                                   
         BE    *+10                                                             
         MVC   P+2(15),=C'**CASH TOTALS**'                                      
         GOTO1 REPORT                                                           
*                                                                               
* PRINT TOTAL BLOCK                                                             
         LA    R6,BLOCK                                                         
GRAND40  DS    0H                                                               
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,GRAND40                                                       
*                                                                               
* PRINT TRADE TOTALS                                                            
*                                                                               
GRAND100 DS    0H                                                               
         L     R1,ATGRAND                                                       
         CLC   =X'0002',0(R1)      ANY TRADE GRAND TOTALS?                      
         BE    GRANDX                                                           
         GOTO1 REGENTL2,DMCB,ATGRAND,BLOCK  FORMAT TRADE                        
         SR    R2,R2                                                            
         IC    R2,DMCB+4           NUMBER OF LINES                              
         SR    R6,R6                                                            
         IC    R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    GRAND120                                                         
         BP    GRAND110                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     GRAND120                                                         
*                                                                               
GRAND110 STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
GRAND120 DS    0H                                                               
         MVC   P+2(13),=C'COMBO TOTALS:'                                        
         L     R1,AGRAND          CASH BUCKETS ALSO?                            
         CLC   =X'0002',0(R1)                                                   
         BE    *+10                                                             
         MVC   P+2(16),=C'**TRADE TOTALS**'                                     
         GOTO1 REPORT                                                           
*                                                                               
* PRINT TOTAL BLOCK                                                             
         LA    R6,BLOCK                                                         
GRAND140 DS    0H                                                               
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,GRAND140                                                      
*                                                                               
* PRINT COMBINED TOTALS                                                         
*                                                                               
         L     R1,AGRAND                                                        
         CLC   =X'0002',0(R1)      TRADE + CASH BUCKETS?                        
         BE    GRANDX              NO - DONE                                    
*                                                                               
         L     R3,AGRAND           BUILD COMBINED BUCKETS                       
         L     R4,ATGRAND                                                       
         GOTO1 =A(TOTLTOTL),RR=Y                                                
*                                                                               
         L     R1,AGRAND           NOW HAS COMBINED BUCKETS                     
         CLC   =X'0002',0(R1)      ANY GRAND TOTALS?                            
         BE    GRANDX                                                           
         GOTO1 REGENTL2,DMCB,AGRAND,BLOCK  FORMAT COMBINED                      
         SR    R2,R2                                                            
         IC    R2,DMCB+4           NUMBER OF LINES                              
         SR    R6,R6                                                            
         IC    R6,LINE             LINE COUNT SO FAR                            
         LA    R6,1(R6,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLINES                                                      
         SR    R4,R6                                                            
         BZ    GRAND220                                                         
         BP    GRAND210                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),(RA),(R9),RR=Y                            
         B     GRAND220                                                         
*                                                                               
GRAND210 STC   R4,SPACING                                                       
         CLI   SPACING,3                                                        
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
GRAND220 DS    0H                                                               
         MVC   P+2(16),=C'**GRAND TOTALS**'                                     
         GOTO1 REPORT                                                           
*                                                                               
* PRINT TOTAL BLOCK                                                             
         LA    R6,BLOCK                                                         
GRAND240 DS    0H                                                               
         MVC   P+3(80),0(R6)                                                    
         GOTO1 REPORT                                                           
         LA    R6,80(R6)                                                        
         BCT   R2,GRAND240                                                      
*                                                                               
GRANDX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* PSTCMT                                                              *         
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   CSECT                                                                  
         NMOD1 0,*PSTCMT*                                                       
         L     R6,0(R1)                                                         
         L     RC,4(R1)                                                         
         L     RA,8(R1)                                                         
         L     R9,12(R1)                                                        
         MVC   STCMODE,0(R1)                                                    
*                                                                               
         LA    R8,IOAREA                                                        
         GOTO1 =V(REGENSTC),DMCB,(STCMODE,0(R6)),(R8),DATAMGR,RCONREC, X        
               RR=Y                                                             
         BNZ   PSTCMTXX            COMMENT NOT FOUND, PRINT NOTHING             
*                                                                               
         CLI   0(R8),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTXX                                                         
         CLI   0(R8),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R8)            GET LENGTH                                   
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),1(R8)                                                    
         MVI   P-1,X'40'                                                        
         BAS   RE,PRTREP                                                        
*                                                                               
         ZIC   R4,0(R8)            BUMP TO NEXT ENTRY                           
         AR    R8,R4               X'FF' MARKS THE END OF BLOCK                 
         CLI   0(R8),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R8                        
         A     R4,=AL4(IOAREA-STORED+L'IOAREA+1)                                
         CR    R4,R8                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENT                      
         CLI   STCMODE,2           FROM CONTRACT COMMENT                        
         BNE   PSTCMT30                                                         
         CLI   1(R6),3                                                          
         BL    PSTCMTXX                                                         
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),2(R6)                                                    
         B     PSTCMT40                                                         
*                                                                               
PSTCMT30 CLI   STCMODE,3           FROM STATION COMMENT                         
         BNE   PSTCMTXX            ANYTHING ELSE DON'T PRINT                    
         LA    R4,59                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),0(R6)                                                    
*                                                                               
PSTCMT40 MVI   SPACING,2                                                        
*                                                                               
PSTCMTX  BAS   RE,PRTREP           COMMENT PRINTED, SKIP A LINE                 
PSTCMTXX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRINT DARE AGENCY HIATUS DATES IF ANY                                         
*********************************************************************           
HIATUS   CSECT                                                                  
         NMOD1 0,*HIATUS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
         LR    R5,R6                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
         USING RCONHIEL,R6                                                      
                                                                                
         CLI   RCONHILN,2          SKIP IF NO DATES                             
         BNH   HIATUSX                                                          
                                                                                
         GOTO1 REPORT                                                           
         MVC   P+13(23),=C'*AGENCY HIATUS DATE(S)*'                             
         GOTO1 REPORT                                                           
                                                                                
         ZIC   R2,RCONHILN                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RCONHIDT                                                      
         DROP  R6                                                               
                                                                                
         LA    R4,P+13                                                          
                                                                                
* IF WEEKLY, WILL TRY TO COLLASP DATES. IE AUG24-3W                             
                                                                                
HIATUS20 DS    0H                                                               
         LA    R3,1                NUMBER OF CONSECUTIVE WEEKS                  
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,0(R4))                                  
         LA    R4,5(R4)                                                         
                                                                                
         USING RCONDREL,R5                                                      
         TM    RCONDRFG,X'08'      DAILY?                                       
         BO    HIATUS50                                                         
         DROP  R5                                                               
                                                                                
         CH    R2,=H'1'                                                         
         BNH   HIATUS40                                                         
                                                                                
HIATUS30 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R6)),(0,WORK2)                                  
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,WORK2+6)                                
         GOTO1 ADDAY,DMCB,WORK2,WORK,7                                          
         CLC   WORK(6),WORK2+6     IF NEXT DATE IS EXACTLY ONE WEEK             
         BNE   HIATUS40            AWAY, KEEP LOOKING                           
                                                                                
         MVC   WORK2(6),WORK2+6                                                 
         LA    R3,1(R3)                                                         
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         CH    R2,=H'1'                                                         
         BH    HIATUS30                                                         
         SR    R2,R2                                                            
                                                                                
HIATUS40 DS    0H                                                               
         MVI   0(R4),C'-'                                                       
         EDIT  (R3),(2,1(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   1(R4),C'W'                                                       
         LA    R4,2(R4)                                                         
                                                                                
HIATUS50 DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
                                                                                
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         LA    RF,P+70                                                          
         CR    R4,RF                                                            
         BL    HIATUS20                                                         
         GOTO1 REPORT                                                           
         LA    R4,P+13                                                          
         B     HIATUS20                                                         
                                                                                
HIATUS80 DS    0H                                                               
         GOTO1 REPORT                                                           
                                                                                
HIATUSX  DS    0H                                                               
         GOTO1 REPORT                                                           
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READRECS - READ RSTAREC & RADVREC & RAGYREC FOR CORNTRACT           *         
***********************************************************************         
READRECS CSECT                                                                  
         NMOD1 0,*RRECS*                                                        
         L     RC,0(R1)                                                         
*                                                                               
* READ OFFICE RECORD                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),RCONKREP                                               
         MVC   KEY+25(2),RCONKOFF                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,ROFFREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
* READ SALESMAN RECORD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),RCONKREP                                               
         MVC   KEY+24(3),RCONSAL                                                
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    READ0020                                                         
*                                                                               
*   TEST OPT2 - 2ND   END                                                       
*                                                                               
         CLI   FULL-1,C'Y'         PROCESS NO-FINDS?                            
         BE    READ0010            YES                                          
         DC    H'0'                NO  - DIE                                    
READ0010 EQU   *                                                                
         MVC   RSALNAME,=C'**S/P NOT ON FILE** '                                
         MVC   RSALTEL,SPACES                                                   
         B     READ0030                                                         
READ0020 EQU   *                                                                
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RSALREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
READ0030 EQU   *                                                                
*                                                                               
* READ ADVERTISER RECORD                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),RCONKADV                                               
         MVC   KEY+25(2),RCONKREP                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC FOUND?                                   
         BZ    READ0040            YES                                          
*                                                                               
         CLI   FULL-1,C'Y'         PROCESS NO-FINDS?                            
         BE    READ0036            YES                                          
         DC    H'0'                NO  - DIE                                    
READ0036 EQU   *                                                                
         MVC   RADVNAME,=C'**ADV NOT ON FILE** '                                
         B     READ0044                                                         
READ0040 EQU   *                                                                
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RADVREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
READ0044 EQU   *                                                                
*                                                                               
* READ PRODUCT RECORD                                                           
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BE    RREC050                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),RCONPRD                                                
         MVC   KEY+25(2),RCONKREP                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC FOUND?                                   
         BNZ   RREC050             NO  - SKIP RECORD READ                       
         MVI   OPT2A,C'Y'          SET 'PRODUCT REC FOUND' FLAG                 
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RPRDREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
RREC050  DS    0H                                                               
*                                                                               
* READ AGENCY RECORD                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),RCONKAGY                                               
         MVC   KEY+25(2),RCONKREP                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RAGYREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),RCONKREP                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FF'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RSTAREC,DMWORK                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  TOTLTOTL:  ACCUMULATES EACH STATION'S TOTALS INTO A GRAND                    
*    TOTAL ARRAY, CONSIDERING DIFFERENCES IN THE MONTHS REPORTED                
*    WITHIN STATIONS.  THE FINAL TOTALS WILL BE FED BACK THROUGH                
*    THE PRINT ROUTINE TO PRODUCE GRAND TOTALS ON THE SCREEN                    
*    R3            =    ADDRESS OF GRAND TOTALS                                 
*    R4            =    TOTALS OF STATION IN PROGRESS                           
*    IOAREA        =    TEMPORARY WORK AREA                                     
***********************************************************************         
BCKTDATE EQU   2                                                                
BCKTVALU EQU   6                                                                
BCKTSPOT EQU   10                                                               
*                                                                               
TOTLTOTL NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOAREA           ADDRESS TEMP WORK SPACE                      
         MVC   0(200,R2),0(R3)     LOAD TEMPORARY WORK SPACE                    
         ZICM  R1,0(R4),2          L(ENTRY)                                     
         LA    RE,0(R1,R4)         A(END OF ENTRIES)                            
         LR    RF,R2               A(TEMP WORK SPACE)                           
         ZICM  R0,0(RF),2          L(ENTRY)                                     
         AR    RF,R0               A(END OF ENTRIES)                            
         LA    R1,2(R4)            A(1ST BUCKET STA IN PROG)                    
         LA    R2,2(R2)            A(1ST BUCKET TEMP WORK SPACE)                
         LA    R6,2(R3)            A(1ST BUCKET ACCUMULATOR)                    
TOTO0010 EQU   *                                                                
         CR    R1,RE               STA IN PROGRESS AT END?                      
         BNE   TOTO0020            NO  - CHECK TEMP WORK SPACE                  
         CR    R2,RF               YES - TEMP WORK SPACE AT END?                
         BNE   TOTO0050            NO  - RUN TEMP WORK SPACE                    
         B     TOTO0100            YES -                                        
TOTO0020 EQU   *                                                                
         CR    R2,RF               TEMP WORK SPACE AT END?                      
         BE    TOTO0060            YES - RUN OUT STA IN PROGRESS                
*                                                                               
*   NEITHER ARRAY AT END:  COMPARE DATES                                        
*                                                                               
         CLC   BCKTDATE(2,R1),BCKTDATE(R2)                                      
         BL    TOTO0030            STA IN PROG < TEMP WORK SPACE                
         BH    TOTO0040            TEMP WORK SPACE < STA IN PROG                
*                                                                               
*   DATES EQUAL:  ACCUMULATE SPOTS AND TOTALS                                   
*                                                                               
*                                                                               
         MVC   DUB(4),BCKTSPOT(R1)                                              
         L     R4,DUB                                                           
         MVC   DUB(4),BCKTSPOT(R2)                                              
         L     R5,DUB                                                           
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
         MVC   BCKTSPOT(4,R6),DUB  LOAD NEW SPOTS                               
*                                                                               
         MVC   DUB(4),BCKTVALU(R1)                                              
         L     R4,DUB                                                           
         MVC   DUB(4),BCKTVALU(R2)                                              
         L     R5,DUB                                                           
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
***>>    MVC   0(14,R6),0(R1)      SET UP ACCUMULATOR ELEMENT                   
         MVC   0(4,R6),0(R1)       INSERT ELT CODE, DATES                       
         MVC   BCKTVALU(4,R6),DUB  LOAD NEW DOLLARS                             
*                                                                               
         LA    R1,14(R1)                                                        
         LA    R2,14(R2)                                                        
         LA    R6,14(R6)                                                        
         B     TOTO0010                                                         
TOTO0030 EQU   *                                                                
         MVC   0(14,R6),0(R1)      RUN OUT STA IN PROGRESS                      
         LA    R1,14(R1)                                                        
         LA    R6,14(R6)                                                        
         B     TOTO0010                                                         
TOTO0040 EQU   *                                                                
         MVC   0(14,R6),0(R2)      RUN OUT TEMP WORK SPACE                      
         LA    R2,14(R2)                                                        
         LA    R6,14(R6)                                                        
         B     TOTO0010                                                         
TOTO0050 EQU   *                   RUN OUT TEMP WORK SPACE                      
         CR    R2,RF               END OF TEMP WORK SPACE?                      
         BE    TOTO0100            YES                                          
         MVC   0(14,R6),0(R2)                                                   
         LA    R2,14(R2)                                                        
         LA    R6,14(R6)                                                        
         B     TOTO0050                                                         
TOTO0060 EQU   *                   RUN OUT STA IN PROGRESS                      
         CR    R1,RE               END OF STA IN PROGRESS?                      
         BE    TOTO0100            YES                                          
         MVC   0(14,R6),0(R1)                                                   
         LA    R1,14(R1)                                                        
         LA    R6,14(R6)                                                        
         B     TOTO0060                                                         
TOTO0100 EQU   *                                                                
         SR    R6,R3               RECALCULATE LENGTH                           
         STH   R6,DUB                                                           
         MVC   0(2,R3),DUB         INSERT NEW LENGTH                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155REREP1002S06/18/02'                                      
         END                                                                    
