*          DATA SET RERRGONUP  AT LEVEL 090 AS OF 10/22/14                      
*PHASE RERRGUPA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REPVALMN                REREPVAL                                       
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
         TITLE 'RERRGONUP - REPPAK RRGON FILE UPDATE'                           
**********************************************************************          
* FOR REP7 ONLY SZ & S2 - NRRGON - PROD RUNS AS RERRGUPB             *          
* FOR ALL OF REP3 NOW                                                *          
**********************************************************************          
* HISTORY OF CHANGES                                                 *          
**********************************************************************          
* NOV09/93 (BU ) --- INCREASED SPACEND TABLE TO 300K                 *          
*                                                                    *          
* JUN20/94 (BU ) --- ADD 'AT' TO OFFICE LIST FOR BLAIR TEST          *          
*                                                                    *          
* JUN21/94 (BU ) --- FIX CROSS-COMPANY STATION ERROR                 *          
*                                                                    *          
* JUN22/94 (BU ) --- ENHANCE DISPLAYS TO FIND CROSS-COMPANY STATION  *          
*                    ERROR                                           *          
*                                                                    *          
* MAR02/95 (BU ) --- ADD 'DE' TO OFFICE LIST FOR BLAIR TEST          *          
*                                                                    *          
* JUN28/95 (BU ) --- INCREASED SPACEND TABLE TO 400K                 *          
*                                                                    *          
* JUL18/95 (BGR) --- TESTED FOR NO DATA, STOP PROGRAM CHECK          *          
*                                                                    *          
* JUL19/95 (BGR) -17 ADDED ADV & AGY, ADDED PRIOR YEAR-1             *          
*                                                                    *          
* FEB26/96 (BGR) -18 ADDED GROUPS A, I, & P                          *          
*                                                                    *          
* JUN06/96 (BGR) -19 ADD CONFIRMED, DIRECT, CURRENT WEEK             *          
*                    AND TAPE FILE IN                                *          
*                                                                    *          
* JUN17/96 (BGR) -20 DO ADD/DEL CK IN GETWK, DIRECT                  *          
*                                                                    *          
* JUL03/96 (BGR) -29 FIX DIRECTYP - PUT IN REPTAB                    *          
*                                                                    *          
* JUL09/96 (BGR) -30 CHANGE DELETE TEST (AFTER CONFIRMED TEST)       *          
*                                                                    *          
* JUL10/96 (BGR) -31 FORCE DIRECT CUTOFF DATES TO PRINT              *          
*                                                                    *          
* JUL10/96 (BGR) -32/33 CLEAR RSORTKEY-BUDGET DATA FOULED UP CON SORT*          
*                                                                    *          
* JUL11/96 (BGR) -34 REMOVE TAPE FILE DCB                            *          
*                                                                    *          
* NOV01/96 (BGR) -35/36 BYPASS C*MP BUDGETS, BLKSIZE FROM 2108-27404 *          
* JAN28/97 (BGR) -37 ALLOW S3 REP ONLY                               *          
* APR07/97 (BGR) -41 ALLOW ALL OF REP3                               *          
* MAY05/97 (BGR) -42 ALLOW ALL OF REP4                               *          
* JUN26/97 (BGR) -45 FIX MASTER REP READ SUB STATIONS RECS           *          
* AUG06/97 (BGR) -46 ADD MORE TESTS FOR CONFIRMED DOLLARS            *          
* NOV17/97 (BGR) -47 ADD CAGENCY, STOP NULL FLDS, NULL 4TH BYTE CAG  *          
*                    ADD 'DA' TO OFFICE LIST FOR BLAIR TEST          *          
* NOV24/97 (BGR) -48 FIX PRIOR YEAR UPDATE                           *          
* FEB26/98 (BGR) -49 ADD BYPASS BACK BILLING FOR KATZ RADIO          *          
* MAR12/98 (BGR) -50 4K RECORDS                                      *          
* MAR23/98 (BGR) -51 BYPASS PRIOR UPDATE, FIX LOW POWER TV           *          
* MAY15/98 (BGR) -52 FIX FOR 1F CHANGED TO 1D ELEM ONLINE            *          
* MAY20/98 (BGR) -53 CHANGE SPACEND FROM 400 TO 500                  *          
* JUN09/98 (BGR) -54 DROP USER LOCK-OUT                              *          
* JUL06/98 (BGR) -55 PETNY DIRECT ONLY UPDATE                        *          
* JUL13/98 (BGR) -56 ADD FOX AND BLAIR TO DIRECT UPDATE              *          
* SEP17/98 (BGR) -57 ADD 4TH SET OF COUNTERS - PERIOD AND YTD        *          
*                    REQUEST YEAR -1 AS AT TODAY                     *          
* OCT12/98 (BGR) -58 ONLY PROCESS B1, GN, UV                         *          
* OCT26/98 (BGR) -59 PROCESS ALL REPS, 4TH SET OF BUCKETS UNUSED     *          
*                    FOR 98 REPS (NON-PACING) CHG ADV IF AAA FROM    *          
*                    4TH POS BLANK TO NULL                           *          
* OCT27/98 (BGR) -60 USE RCONKAOF FOR AGY/OFF - NOT RCONKOFF         *          
* NOV03/98 (BGR) -61 BYPASS TRACE, ADD SAPESPERSON                   *          
* NOV10/98 (BGR) -62 STOP UPDATING RODYPBLC                          *          
* JAN29/99 (BGR) -63 PRT ERR MSG FOR CONFIMED/UNCONFIRMED CONTRACTS  *          
*                    FIX BUDGET UPDATES WHEN CONFIRMED OR DIRECT     *          
* MAR19/99 (BGR) -64 FIX CAGY - 3RD POSITION IF BLANK, MAKE NULL     *          
* MAY24/99 (BGR) -65 FIX SSB AND ADD GROUP N                         *          
* OCT14/99 (BGR) -66 FIX PRIOR YR AS AT TODAY - I HOPE               *          
* OCT26/99 (BGR) -67 PROCESS I8, NOT BYPASS IT                       *          
* JAN04/00 (BGR) -68 FIX DATCON CALL FOR TODAY                       *          
* NOV20/01 (BU ) --- BYPASS ALL BACKBILLING ORDERS ON UPDATE         *          
* FEB12/02 (BGR)  72 MAKE SPACEND TABLE LARGER-FROM 500000 TO 800000 *          
* APR03/02 (BU ) --- ADJUST FOR DAILY PACING                         *          
* OCT25/02 (BU ) --- ADD GROUP W FOR WSMNY (WEST SIDE MEDIA)         *          
* JAN20/04 (BU ) --- DON'T ADD ALLIED STATIONS TO TABLE              *          
* MAR01/08 (KUI) --- LEAP YEAR FIX                                   *          
* OCT15/14 (DEIS) -- LEAP YEAR FIX FOR 2016                          *          
*                    PRODUCE DFSORT DIAGNOSTIC MESSAGES              *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*        REG   USAGE                                                 *          
*        R1    WORK                                                  *          
*        R2    WORK                                                  *          
*        R3    WORK                                                  *          
*        R4    WORK                                                  *          
*        R5    WORK                                                  *          
*        R6    WORK                                                  *          
*        R7    WORK                                                  *          
*        R8    PRINT DSECT                                           *          
*        R9    BASE 3                                                *          
*        RA    BASE 2                                                *          
*        RB    BASE 1                                                *          
*        RC    BASE 4                                                *          
*        RD    WORK                                                  *          
*        RE    WORK                                                  *          
*        RF    WORK                                                  *          
*                                                                    *          
**********************************************************************          
RERRGUP  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,RERRGUP,VREGSAVE,RA,R9                                         
         LA    RC,2048(R9)                                                      
         LA    RC,2048(RC)                                                      
         USING RERRGUP+4096+4096+4096,RC                                        
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,=V(HEXOUT)                                                    
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         MVI   WRITEFLG,C'Y'                                                    
         MVI   TRACEFLG,C'N'                                                    
         XC    TRACECTR,TRACECTR                                                
         GOTO1 =A(CARDPROC)                                                     
         SPACE                                                                  
         GOTO1 =V(DATCON),(R1),(5,0),(0,DUB)                                    
         GOTO1 =V(GETDAY),(R1),DUB,FULL                                         
         CLI   DMCB,1             ARE WE MONDAY?                                
         BE    RG110005            YES                                          
*                                 NEED TO GET PREVIOUS MONDAY                   
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         LNR   R2,R1                                                            
         GOTO1 =V(ADDAY),DMCB,DUB,DUB,(R2)                                      
         SPACE                                                                  
RG110005 GOTO1 =V(DATCON),(R1),(0,DUB),(2,THISADJ)                              
*                                  SET ADJUSTED MONDAY DATE                     
         CLC   DATORIDE,SPACES     ANY OVERRIDE DATE?                           
         BNH   RG11                NO                                           
         GOTO1 =V(DATCON),DMCB,(3,DATORIDE),(2,THISDAY)                         
*                                  YES - USE DATE OVERRIDE, NOT TODAY'S         
         B     RG110010                                                         
RG11     EQU   *                                                                
         GOTO1 =V(DATCON),(R1),(5,0),(2,THISDAY)                                
*                                  SET TODAY'S DATE                             
RG110010 EQU   *                                                                
*                                                                               
*                         1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7                 
         MVC   P+1(35),=C'DATES FOR RUN: ADJ=          TODAY='                  
         GOTO1 =V(DATCON),DMCB,(2,THISADJ),(5,P+20)                             
         GOTO1 =V(DATCON),DMCB,(2,THISDAY),(5,P+36)                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    REPDAILY,REPDAILY   CLEAR REP/DAILY FLAG TABLE                   
         XC    REPMASTR,REPMASTR                                                
         LA    RF,REPDAILY         SET TABLE USE ADDRS                          
         ST    RF,THISREP                                                       
         ST    RF,NEXTREP          SET NEXT SLOT AVAILABLE                      
*                                                                               
         L     R1,=A(MONTAB1)                                                   
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'MONTAB1*'                                             
         L     R1,=A(MONTAB2)                                                   
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'MONTAB2*'                                             
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,A(SORTCARD),(X'80',A(RECCARD)),(X'80',0)         
*                                                                               
*   OPEN AND READ CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           READ ID RECORD                               
         MVC   WORK+15(10),SVID                                                 
         OC    WORK+15(10),SPACES                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                INVALID ID                                   
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
         SR    R0,R0                                                            
*                                                                               
RG12     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'21'         TEST AUTH ELEMENT                            
         BNE   RG14                                                             
         CLI   2(R1),X'08'         YES-IS IT 'REP' SYSTEM?                      
         BNE   RG14                                                             
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL WITH               
         B     RG16                REP UTL CODE                                 
*                                                                               
RG14     IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RG12                                                             
*                                                                               
RG16     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                   
         EJECT                                                                  
*          DATA SET REREPRG02A AT LEVEL 068 AS OF 06/13/96                      
*                                                                               
*  THIS ROUTINE SETS THE DIRECT RESPONSE CUTOFF DATE:                           
*        DATE OF RUN IS ADJUSTED:                                               
*              IF DATE OF RUN IS WEDNESDAY, DATE IS USED AS IS                  
*              IF DATE OF RUN IS NOT WEDNESDAY, FOLLOWING                       
*                 WEDNESDAY IS CALCULATED, AND ITS DATE USED.                   
*        IF AS-AT DATE, AS CALCULATED ABOVE, IF AFTER 14TH OF MONTH,            
*              CURRENT YEAR/MONTH IS SET, ELSE PREVIOUS IS SET                  
*                                                                               
*                                                                               
*                                                                               
DRCUTOFF DS   0H                                                                
         GOTO1 =V(DATCON),DMCB,(5,0),(0,WORK+36)                                
         MVC   WORK+42(6),WORK+36                                               
         GOTO1 =V(GETDAY),DMCB,WORK+42,FULL                                     
*                                  GET DAY OF WEEK                              
         CLI   DMCB,3              IS RUNDATE WEDNESDAY?                        
         BE    DRCU0060            YES - USE IT FOR CUTOFF TEST                 
         LA    RF,WEDADJST                                                      
DRCU0010 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                DAY NOT FOUND                                
         CLC   DMCB(1),0(RF)       DAY OF WEEK VS TABLE                         
         BE    DRCU0020            FOUND                                        
         LA    RF,5(RF)            BUMP TO NEXT ENTRY                           
         B     DRCU0010            GO BACK FOR NEXT                             
DRCU0020 EQU   *                                                                
         ZICM  R3,1(RF),4          GET DAY ADJUSTMENT TO WED                    
         GOTO1 =V(ADDAY),DMCB,WORK+42,WORK+36,(R3)                              
*                                                                               
*   TEST                                                                        
         MVC   P+1(14),=C'DRCUTOFF CALC='                                       
         MVC   P+16(13),=C'+42/+36/(R3)='                                       
         MVC   P+29(6),WORK+42                                                  
         MVI   P+35,C'/'                                                        
         MVC   P+36(6),WORK+36                                                  
         MVI   P+42,C'/'                                                        
         EDIT  (R3),(6,P+43),FILL=0,WRK=MYWORK                                  
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         B     DRCU0060                                                         
*                                                                               
MYWORK   DS    CL20                                                             
*                                                                               
*   WEDNESDAY ADJUSTMENT TABLE:  EACH ENTRY IS TWO CHARACTERS                   
*        CHAR 1:  DAY OF WEEK                                                   
*        CHAR 2:  ADJUSTMENT VALUE  - SUN THRU TUESDAY ADJUST                   
*                                     THU THRU SATURDAY DON'T                   
*                                                                               
WEDADJST DC    X'01',X'00000002'         MONDAY                                 
         DC    X'02',X'00000001'         TUESDAY                                
         DC    X'04',X'FFFFFFFF'         THURSDAY - BACK UP 1 DAY               
         DC    X'05',X'FFFFFFFE'         FRIDAY   - BACK UP 2 DAYS              
         DC    X'06',X'00000004'         SATURDAY                               
         DC    X'07',X'00000003'         SUNDAY                                 
         DC    X'0000'                                                          
DRCU0060 EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(0,WORK+36),(3,WORK+42)                          
*                                  CONVERT DATE TO BINARY YMD                   
         CLI   WORK+44,X'0F'       DAY 15TH OR AFTER?                           
         BL    DRCU0080            NO  - SET PREVIOUS MONTH                     
         MVC   QWDRCOFF,WORK+42    YES - SAVE YM OF AS-AT DATE                  
         B     DRCU0160            EXIT                                         
DRCU0080 EQU   *                   SET PREVIOUS MONTH                           
         CLI   WORK+43,1           IS MONTH JANUARY?                            
         BE    DRCU0100            YES                                          
         ZIC   RF,WORK+43          NO  - DECREMENT 1 MONTH                      
         BCTR  RF,0                                                             
         STC   RF,WORK+43          REINSERT MONTH                               
         MVC   QWDRCOFF,WORK+42    SAVE YM OF AS-AT DATE                        
         B     DRCU0160            EXIT                                         
DRCU0100 EQU   *                                                                
         MVI   WORK+43,12          JANUARY: OVERLAY WITH DECEMBER               
         ZIC   RF,WORK+42          DECREMENT YEAR BY 1                          
         BCTR  RF,0                                                             
         STC   RF,WORK+42                                                       
         MVC   QWDRCOFF,WORK+42    SAVE YM OF AS-AT DATE                        
*                                  EXIT                                         
DRCU0160 EQU   *                                                                
         MVC   QWDRCOFP,QWDRCOFF   SET PRIOR YEAR CUTOFF                        
         ZIC   RF,QWDRCOFP         BACK UP 1 YEAR                               
         BCTR  RF,0                                                             
         STC   RF,QWDRCOFP         RESTORE YEAR                                 
         CLI   QWDRCOFP,96         LEAP YEAR?  1996                             
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLI   QWDRCOFP,100        LEAP YEAR?  2000                             
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLI   QWDRCOFP,104        LEAP YEAR?  2004                             
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLI   QWDRCOFP,108        LEAP YEAR?  2008                             
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLI   QWDRCOFP,112        LEAP YEAR? 2012                              
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLI   QWDRCOFP,116        LEAP YEAR? 2016                              
         BE    DRCU0180            YES - ANY DATE OKAY                          
         CLC   QWDRCOFP+1(2),=X'021D'                                           
*                                  NO  - FEB 29?                                
         BNE   DRCU0180            NO  - OKAY                                   
         MVC   QWDRCOFP+1(2),=X'021C'                                           
*                                  YES - SET TO FEB 28                          
DRCU0180 EQU   *                                                                
         MVC   P+1(10),=C'DR CUTOFF:'                                           
         MVC   WORK+48(2),QWDRCOFF                                              
         MVI   WORK+50,1                                                        
         GOTO1 =V(DATCON),DMCB,(3,WORK+48),(5,P+15)                             
         MVC   WORK+48(2),QWDRCOFP                                              
         MVI   WORK+50,1                                                        
         GOTO1 =V(DATCON),DMCB,(3,WORK+48),(5,P+25)                             
         GOTO1 =V(PRINTER)                                                      
DRCU0200 EQU   *                                                                
         XC    WORK+36(20),WORK+36 CLEAR THE WORK AREA                          
         EJECT                                                                  
*                                                                               
*        PEEL AND SORT RECOVERY FILE                                            
         SPACE                                                                  
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    IN004                                                            
         DC    H'0'                                                             
         SPACE                                                                  
IN004    SR    R3,R3                                                            
*                                                                               
IN010    DS   0H                                                                
         LA    R0,RECVHDR-4                                                     
         L     R1,=A(RECVIN)                                                    
         GET   (1),(0)                                                          
*                                                                               
IN014    CLI   RFILTY,X'82'        TEST REPFILE                                 
         BNE   IN010                                                            
         SPACE                                                                  
         XC    RSORTKEY,RSORTKEY                                                
         SPACE                                                                  
         CLI   RKEY,X'0C'          TEST CONTRACT REC                            
         BE    IN020                                                            
         CLI   RKEY,X'13'          TEST BUDGET REC                              
         BNE   IN010                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
***      MVC   P+1(18),=C'PROCESSING BUDGET'                                    
***      MVC   P+22(64),RKEY                                                    
***      GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         LA    RF,RKEY             SET A(KEY)                                   
         ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
         AR    RF,RE               BUMP TO END OF RECORD                        
         XC    0(16,RF),0(RF)      CLEAR MANY BYTES AT REC END                  
*                                                                               
         LA    R2,RKEY             BUDGET REC                                   
         USING RBUDD,R2                                                         
         SPACE                                                                  
* BYPASS REPS NOT UNDER NEW PACING - YEAR '99                                   
         SPACE                                                                  
*        LA    R0,(L'OLDTAB)/2                                                  
*        LA    R1,OLDTAB                                                        
*        CLC   RBUDKREP,0(R1)      IF NEW REP                                   
*        BE    IN010                BYPASS                                      
*        LA    R1,2(,R1)                                                        
*        BCT   R0,*-14                                                          
         SPACE                                                                  
         LA    R1,RBUDKREP                                                      
*                                                                               
*   TEST                                                                        
***      MVC   P+1(07),=C'CHKREP1'                                              
***      GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         BAS   RE,CHKREP           CHECK THE REP CODE                           
         BNE   IN010                                                            
         SPACE                                                                  
* YRSTART/YREND ARE FILLED IN IN CHKREP ROUTINE FROM RRGON FILE *               
         SPACE                                                                  
         CLC   RBUDKYR,YRSTART     TEST YEAR                                    
         BL    IN010                                                            
         CLC   RBUDKYR,YREND                                                    
         BH    IN010                                                            
         CLC   =C'C*MP',RBUDKSTA   THIS A COMPANY BUDGET                        
         BE    IN010                YES, BYPASS                                 
         SPACE                                                                  
         MVC   RSORTREP,RBUDKREP                                                
         XC    RSORTSUB,RSORTSUB                                                
         MVI   RSORTTYP,C'B'                                                    
         MVC   RSORTBUD,RBUDKYR                                                 
         B     IN030                                                            
*                                                                               
*LDTAB   DC    C'B1GNUV'              B1=TELEMUNDO GN & UN=UNIVISION            
*LDTABCT EQU   (*-OLDTAB)/2                                                     
*                                                                               
IN020    EQU   *                                                                
*                                                                               
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0000                                                         
***      MVC   P+1(18),=C'PROCESSING CON:  '                                    
***      MVC   P+22(64),RKEY                                                    
***      GOTO1 =V(PRINTER)                                                      
TEST0000 EQU   *                                                                
***      CLC   =X'06986147',RKEY+23                                             
***      BNE   TEST0010                                                         
***      MVI   TESTFLAG,C'Y'       TURN ON TEST FLAG                            
***      MVC   P+1(17),=C'SPEC CON FOUND:  '                                    
***      MVC   P+22(64),RKEY                                                    
***      GOTO1 =V(PRINTER)                                                      
TEST0010 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
*   TEST:  SKIP ALL BUT KRD ORDERS                                              
*                                                                               
***      CLC   =C'J0',RKEY+2                                                    
***      BNE   IN010                                                            
*   TEST:  SKIP ALL BUT KRD ORDERS                                              
*                                                                               
         LA    RF,RKEY             SET A(KEY)                                   
         ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
         AR    RF,RE               BUMP TO END OF RECORD                        
         XC    0(16,RF),0(RF)      CLEAR MANY BYTES AT REC END                  
*                                                                               
         LA    R2,RKEY             CONTRACT REC                                 
         USING RCOND,R2                                                         
         SPACE                                                                  
* BYPASS REPS NOT UNDER NEW PACING - YEAR '99                                   
         SPACE                                                                  
*        LA    R0,(L'OLDTAB)/2                                                  
*        LA    R1,OLDTAB                                                        
*        CLC   RCONKREP,0(R1)      IF NEW REP                                   
*        BE    IN010                BYPASS                                      
*        LA    R1,2(,R1)                                                        
*        BCT   R0,*-14                                                          
         SPACE                                                                  
         CLI   RCONKGRP,C'A'       ALLOW GROUPS A/F/I/P/R/N/W & T               
         BE    IN024                                                            
         CLI   RCONKGRP,C'F'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'I'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'P'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'R'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'T'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'W'                                                    
         BE    IN024                                                            
         CLI   RCONKGRP,C'N'                                                    
         BNE   IN010                                                            
         SPACE                                                                  
IN024    LA    R1,RCONKREP                                                      
         SPACE                                                                  
*                                                                               
*   TEST                                                                        
***      MVC   P+1(07),=C'CHKREP2'                                              
***      GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         BAS   RE,CHKREP           CHECK THE REP CODE                           
         BNE   IN010                                                            
         SPACE                                                                  
         GOTO1 =A(CHKBB)            CHECK FOR BACK BILLING ELIMINATION          
         BNE   IN010                                                            
         SPACE                                                                  
         MVC   RSORTREP,RCONKREP                                                
         XC    RSORTSUB,RSORTSUB                                                
         MVI   RSORTTYP,C'C'                                                    
         MVC   RSORTCON,RCONKCON                                                
*                                                                               
IN030    LA    R3,1(R3)            ABSOLUTE SEQ NUM                             
         STCM  R3,15,RSORTSEQ                                                   
         LH    RE,RECVHDR-4                                                     
         LA    RE,L'RSORTKEY+4(RE) ADD EXPANSION LENGTH                         
         SLL   RE,16                                                            
         ST    RE,RLEN                                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
*                                                                               
IN032    OC    MASTREP,MASTREP     CHECK FOR MASTER REP                         
         BZ    IN010                                                            
         MVC   RSORTSUB,RSORTREP   YES-PUT ADDITIONAL RECORD FOR MASTER         
         MVC   RSORTREP,MASTREP                                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN   REP                               
         B     IN010                                                            
*                                                                               
IN100    DS   0H                                                                
         CLOSE (RECVIN,)                                                        
         B     UT001                                                            
         EJECT                                                                  
* CHECK THE REP CODE                                                            
* INPUT  : R1=A(REP CODE)                                                       
* OUTPUT : CC - EQ REP IS ON RRGON FILE AND START/END Y/M SET                   
*          CC - NE REP NOT ON RRGON FILE                                        
*                                                                               
CHKREP   NTR1                                                                   
         CLC   REPCD,0(R1)         TEST CHANGE OF REP                           
         BE    CHKREPX                                                          
*                                                                               
*   TEST0001                                                                    
***      MVC   P+1(08),=C'NEW REP:'                                             
***      MVC   P+10(2),0(R1)                                                    
***      GOTO1 =V(PRINTER)                                                      
*   TEST0001 END                                                                
*                                                                               
         MVC   REPCD,0(R1)                                                      
         MVI   REPSW,C'N'          SET FIRST TIME FOR REP                       
         LA    R4,REPTAB                                                        
*                                                                               
CHKREP2  CLI   0(R4),0                                                          
         BE    CHKREP4             YES                                          
         CLI   0(R4),X'FF'                                                      
         BE    CHKREP4             YES                                          
         CLC   REPCD,0(R4)                                                      
         BE    CHKREP10            NO                                           
         LA    R4,L'REPTAB(R4)                                                  
         B     CHKREP2                                                          
*                                                                               
CHKREP4  MVC   DMFILE,RRGON                                                     
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,REPCD                                                     
         MVC   ROKHD1,=X'0001'                                                  
         BAS   RE,HIGH             GET TYPE 1 HDR REC FOR THIS REP              
         CLC   KEY,KEYSAVE                                                      
         BNE   CHKREPX                                                          
*                                                                               
*   TEST0001                                                                    
***      MVC   P+1(13),=C'REP ON RRGON:'                                        
***      MVC   P+15(2),REPCD                                                    
***      GOTO1 =V(PRINTER)                                                      
*   TEST0001 END                                                                
*                                                                               
         CLI   0(R4),X'FF'         TEST REP TABLE FULL                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R7,RRGREC                                                        
         MVC   0(2,R4),REPCD       SAVE REP CODE                                
*                                                                               
*                                  SAVE GLOBAL START YR/MON                     
*                                                                               
         MVC   DUB(2),RODHD1ST                                                  
         MVI   DUB+2,01                            FORCE DAY                    
         GOTO1 =V(DATCON),DMCB,(3,DUB),WORK                                     
         GOTO1 =V(ADDAY),(R1),WORK,WORK+6,F'-365'  BACK UP 1 YEAR               
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,FULL)                              
         MVC   2(2,R4),FULL                                                     
         MVC   4(2,R4),RODHD1EN                                                 
         MVC   8(4,R4),RODHDOPT    SAVE TYPES DOLLARS ON FILE                   
         CLC   REPCD,=C'I2'        UNLESS REP IS I2                             
         BE    CHKREP10                                                         
*        CLC   REPCD,=C'I8'        NOW PROCESS I8                               
*        BE    CHKREP10                                                         
         MVC   DMFILE,REPDIR       READ REP RECORD TO SEE IF REP HAS A          
         XC    KEY,KEY             MASTER REP                                   
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPCD                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CHKREP10                                                         
         SPACE                                                                  
         BAS   RE,GETREP                                                        
         SPACE                                                                  
         MVC   15(1,R4),RREPPROF+8   SAVE DIRECT TYPE                           
         L     RF,NEXTREP          SET A(NEXT AVAILABLE SLOT IN TABLE)          
         CLI   0(RF),X'FF'         ANY ROOM IN TABLE?  ALLOWS 80                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - EXPAND TABLE SIZE                      
         MVC   0(2,RF),RREPKREP    SAVE REP IN TABLE                            
         MVC   2(1,RF),RREPPROF+27  SAVE DAILY PACING FLAG                      
         LA    RF,LREPSLOT(RF)                                                  
         ST    RF,NEXTREP                                                       
*                                                                               
         MVC   P+1(10),=C'REP FOUND:'                                           
         MVC   P+12(02),RREPKREP                                                
         MVC   P+16(01),RREPPROF+27                                             
         MVC   P+20(20),RREPNAME                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SPACE                                                                  
         CLC   RREPMAST,=X'FFFF'   CHECK FOR MASTER REP                         
         BE    CHKREP10                                                         
         OC    RREPMAST,RREPMAST                                                
         BZ    CHKREP10                                                         
         CLC   RREPMAST,=CL2'  '                                                
         BE    CHKREP10                                                         
         CLC   REPMASTR,=X'FFFF'   MASTER REP ALREADY DONE?                     
         BE    CHKREP4A            YES - LEAVE AS IS                            
         MVC   REPMASTR,RREPMAST   NO  - SAVE MASTER REP CODE                   
*                                                                               
         MVC   P+1(17),=C'MASTER REP FOUND:'                                    
         MVC   P+20(2),REPMASTR                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKREP4A EQU   *                                                                
         MVC   DMFILE,RRGON        YES-TEST MASTER REP IS ON RRGON              
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,RREPMAST                                                  
         MVC   ROKHD1,=X'0001'                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY,KEYSAVE                                                      
         BNE   CHKREP10                                                         
         LA    R7,RRGREC                                                        
         MVC   6(2,R4),RREPMAST    YES-SAVE IT                                  
         LA    R5,REPTAB           CHECK MASTER REP IS IN REP TABLE             
*                                                                               
CHKREP6  CLI   0(R5),0                                                          
         BE    CHKREP8                                                          
         CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                EXPAND MAX N'REPS                            
         CLC   RREPMAST,0(R5)                                                   
         BE    CHKREP10                                                         
         LA    R5,L'REPTAB(R5)                                                  
         B     CHKREP6                                                          
*                                                                               
CHKREP8  MVC   0(2,R5),RREPMAST                                                 
*                                                                               
*                                  SAVE START YR/MON                            
*                                                                               
         MVC   DUB(2),RODHD1ST                                                  
         MVI   DUB+2,01                            FORCE DAY                    
         GOTO1 =V(DATCON),DMCB,(3,DUB),WORK                                     
         GOTO1 =V(ADDAY),(R1),WORK,WORK+6,F'-365'  BACK UP 1 YEAR               
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,FULL)                              
         MVC   2(2,R5),FULL                                                     
         MVC   4(2,R5),RODHD1EN                                                 
         MVC   8(4,R5),RODHDOPT    SAVE TYPES DOLLARS ON FILE                   
         MVC   15(1,R5),15(R4)     SAVE DIRECT TYPE                             
*                                                                               
CHKREP10 MVI   REPSW,C'Y'          REP IS ACCEPTED                              
         MVC   RRGSTART,2(R4)      GLOBAL START YR/MN                           
         MVC   RRGEND,4(R4)        GLOBAL END YR/MN                             
         MVC   TYPETAB,8(R4)       SAVE TYPES - ALL/CONF/UNCONF/DIR             
*                                  TO GENERATE RRGON RECS                       
         SPACE                                                                  
         MVC   MASTREP,6(R4)       MASTER REP                                   
         ZIC   R1,RRGSTART                                                      
         CVD   R1,DUB                                                           
         UNPK  YRSTART(2),DUB                                                   
         OI    YRSTART+1,X'F0'     GLOBAL START YEAR                            
         IC    R1,RRGEND                                                        
         CVD   R1,DUB                                                           
         UNPK  YREND(2),DUB                                                     
         OI    YREND+1,X'F0'       GLOBAL END YEAR                              
*                                                                               
CHKREPX  EQU   *                                                                
         OC    REPMASTR,REPMASTR   WAS A MASTER REP FOUND?                      
         BZ    CHKREXIT            NO  - EXIT                                   
         CLC   REPMASTR,=X'FFFF'   MASTER REP ALREADY DONE?                     
         BE    CHKREXIT            YES                                          
*                                                                               
         MVC   DMFILE,REPDIR       SET TO READ REP FILE                         
*                                                                               
         XC    KEY,KEY             MASTER REP                                   
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPMASTR                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MASTER BETTER BE ON FILE                     
         BAS   RE,GETREP                                                        
         L     RF,NEXTREP          SET A(NEXT AVAILABLE SLOT IN TABLE)          
         CLI   0(RF),X'FF'         ANY ROOM IN TABLE?  ALLOWS 80                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - EXPAND TABLE SIZE                      
         MVC   0(2,RF),RREPKREP    SAVE REP IN TABLE                            
         MVC   2(1,RF),RREPPROF+27  SAVE DAILY PACING FLAG                      
         LA    RF,LREPSLOT(RF)                                                  
         ST    RF,NEXTREP                                                       
*                                                                               
         MVC   P+1(10),=C'REP FOUND:'                                           
         MVC   P+12(02),RREPKREP                                                
         MVC   P+16(01),RREPPROF+27                                             
         MVC   P+20(20),RREPNAME                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   REPMASTR,=X'FFFF'                                                
*                                                                               
CHKREXIT EQU   *                                                                
         MVC   DMFILE,RRGON        SET TO READ RRGON FILE                       
         CLI   REPSW,C'Y'                                                       
*        WHY IS THERE A COMPARE, BUT NO TEST OF RESULT?                         
*        BECAUSE THIS IS SETTING THE CONDITION CODE FOR THE                     
*        ENTIRE ROUTINE.  THIS IS SO DUMB THAT IT'S ALMOST                      
*        INTELLIGENT....                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*        READ THE SORTED RECOVERY FILE                                          
*                                                                               
*                                                                               
UT001    DS    0H                                                               
         OPEN  (TEMP,(OUTPUT))                                                  
*                                                                               
UT002    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BZ    UT004                                                            
*                                                                               
         L     R1,=A(TEMP)                                                      
         PUT   (1),(R5)                                                         
         B     UT002                                                            
*                                                                               
UT004    GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (TEMP,)                                                          
         SPACE                                                                  
         LTR   R3,R3               ANY RECORDS FOUND?                           
         BZ    PRINTRP2                                                         
         SPACE                                                                  
         OPEN  (TEMP,(INPUT))                                                   
         L     R6,=A(TEMPREC)                                                   
         L     R1,=A(TEMP)                                                      
         GET   (1),(R6)                                                         
*                                                                               
***      LA    RF,0(R6)            SET A(KEY)                                   
***      ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
***      AR    RF,RE               BUMP TO END OF RECORD                        
***      XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT REC END                   
*                                                                               
         SPACE                                                                  
         XC    REPCD,REPCD                                                      
         ZAP   TOTCHA,=P'0'        TOTAL RECORDS CHANGED COUNTER                
         ZAP   TOTADD,=P'0'        TOTAL RECORDS ADDED COUNTER                  
         ZAP   TOTNUL,=P'0'        TOTAL RECORDS NULL VALUE KEY                 
         BAS   RE,READSTA          READ STATION RECORDS                         
*                                                                               
UT010    LA    R0,RLEN                                                          
         BAS   RE,MOVE             MOVE TO FIRST REC                            
         L     R0,=A(LASTREC)                                                   
         BAS   RE,MOVE             MOVE TO LAST REC                             
         CLC   REPCD,RSORTREP      TEST CHANGE OF REP                           
         BE    *+8                 NO                                           
         BAS   RE,POSTREP          YES-POST RRGON RECORDS FOR LAST REP          
*                                      AND INIT FOR NEW REP                     
*   RSORTSUB IS ONLY SET FOR MASTER REPS, WHERE IT INDICATES THE ORIG-          
*   INAL COMPANY TO WHICH THE CONTRACT BELONGS.  THE FIELD 'SUBREPCD'           
*   IS USED IN THE 'GETSTA' ROUTINE FOR MASTER REP RECORDS TO CORRECTLY         
*   LOCATE THE ORIGINAL COMPANY'S STATION, AS A MASTER REP DOES NOT             
*   HAVE STATIONS.                                                              
*                                                                               
         MVC   SUBREPCD,RSORTSUB   INITIALIZE FOR NEW SUBREP                    
*                                                                               
         LA    R5,RSORTCON-RSORTKEY+L'RSORTCON                                  
         CLI   RSORTTYP,C'B'                                                    
         BNE   UT020                                                            
         LA    R5,RSORTBUD-RSORTKEY+L'RSORTBUD                                  
*                                                                               
UT020    MVI   LASTSW,C'N'         GET NEXT RECORD                              
         L     R6,=A(TEMPREC)                                                   
         L     R1,=A(TEMP)                                                      
         GET   (1),(R6)                                                         
*                                                                               
****     LA    RF,0(R6)            SET A(KEY)                                   
****     ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
****     AR    RF,RE               BUMP TO END OF RECORD                        
****     XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT REC END                   
*                                                                               
         EX    R5,COMPREC          COMPARE THIS REC TO FIRST REC                
         SPACE                                                                  
         BNE   UT035                                                            
         L     R0,=A(LASTREC)      EQUAL - MOVE IT TO LAST REC                  
         BAS   RE,MOVE                                                          
         B     UT020                       AND READ NEXT                        
*                                                                               
* END OF FILE FOR TEMP SORTED BUDGET & CONTRACT RECORDS                         
*                                                                               
UT030    MVI   LASTSW,C'Y'         EOF                                          
*                                                                               
UT035    CLI   RSORTTYP,C'B'                                                    
         BE    UT040                                                            
         BAS   RE,PROCON           PROCESS CONTRACT RECORD                      
         B     UT050                                                            
*                                                                               
UT040    BAS   RE,PROBUD           PROCESS BUDGET RECORD                        
*                                                                               
UT050    CLI   LASTSW,C'Y'                                                      
         BNE   UT010                                                            
         MVC   P+1(11),=C'END OF DATA'                                          
         GOTO1 =V(PRINTER)                                                      
         CLOSE (TEMP,)                                                          
         BAS   RE,POSTREP          POST RRGON RECORDS FOR LAST REP              
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     PRINTRP2                                                         
         SPACE 2                                                                
COMPREC  CLC   RSORTKEY(0),4(R6)   * EXECUTED                                   
         EJECT                                                                  
PRINTRP2 EQU   *                                                                
         GOTO1 =A(PRINTREP)                                                     
         XBASE                                                                  
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
PRTRRGS  NTR1                                                                   
         LA    R3,RKEY                                                          
         SR    R5,R5                                                            
         ICM   R5,3,RKEY+27        GET USED REC LEN                             
         LA    R4,=CL20'FIRST CONTRACT'                                         
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         L     R3,=A(LASTREC)                                                   
         LA    R3,LKEY-LRECD(R3)                                                
         ICM   R5,3,27(R3)         GET USED REC LEN                             
         LA    R4,=CL20'LAST CONTRACT'                                          
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         LA    R3,SORT2REC                                                      
         LA    R5,116              GET USED REC LEN                             
         LA    R4,=CL20'O/P INTO SORT'                                          
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO POST RRGON DETAIL RECORDS FOR OLD REP,                             
* AND INITIALIZE FOR NEW REP                                                    
*                                                                               
POSTREP  NTR1  ,                                                                
         OC    REPCD,REPCD         TEST FIRST REP                               
         BZ    PR20                YES                                          
         MVC   DMFILE,RRGON        NO-POST RRGON DETAIL RECORDS                 
         ZAP   ADDCNT,=P'0'                                                     
         ZAP   CHACNT,=P'0'                                                     
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,REPCD                                                     
         MVC   ROKHD1,=X'0001'                                                  
         BAS   RE,HIGH             GET TYPE 1 HDR REC FOR THIS REP              
         CLC   KEY,KEYSAVE                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,RRGREC                                                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,RODHD1DT)                               
         THMS                                                                   
         STCM  R1,15,RODHD1TM                                                   
         BAS   RE,WRITE          WRITE TYPE 1 HDR REC WITH DATE & TIME          
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'   READ SORTED RECORDS                    
         ICM   RE,15,4(R1)                                                      
         BZ    PR10                                                             
*                                                                               
PR2      MVC   SORT2REC,0(RE)                                                   
         SPACE                                                                  
         GOTO1 =A(GETSORT)         SUMMARIZE ALL EQUAL SORT RECORDS             
         SPACE                                                                  
         LA    R7,KEY              SEE IF RRGON RECORD EXISTS                   
         XC    KEY,KEY                                                          
         MVC   ROKEY,SORT2REC                                                   
         BAS   RE,HIGH                                                          
         CLC   ROKEY,SORT2REC                                                   
         BNE   PR6                 NO - ADD THE RECORD TO RRGON                 
         LA    R0,ROCTRS           YES - ADD DIFFERENCES TO COLS                
         LA    R0,19                                                            
         LA    R7,RRGREC                                                        
         SPACE                                                                  
         LA    R1,RODATA-ROREC                                                  
         LA    R4,SORT2REC(R1)                                                  
         LA    R5,RRGREC(R1)                                                    
*                                                                               
PR4      ICM   RE,15,0(R4)                                                      
         ICM   RF,15,0(R5)                                                      
         AR    RF,RE                                                            
         STCM  RF,15,0(R5)                                                      
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,PR4                                                           
*                                                                               
         AP    CHACNT,=P'1'                                                     
         AP    TOTCHA,=P'1'                                                     
         BAS   RE,WRITE            PUT UPDATED RRGON REC                        
         B     PR8                                                              
*                                                                               
PR6      AP    ADDCNT,=P'1'        RRGON KEY HIGH - PUT SORTER REC              
         AP    TOTADD,=P'1'                                                     
         BAS   RE,ADD                                                           
         BE    PR8                                                              
         DC    H'0'                OVERFLOW AREA FULL?                          
*                                                                               
PR8      ICM   RE,15,ASORTREC      NEXT SORT RECORD                             
         BNZ   PR2                                                              
*                                                                               
PR10     GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         MVC   P(9),=C'**** REP '  PRINT REP TOTALS                             
         MVC   P+9(2),REPCD                                                     
         MVC   P+11(5),=C' ****'                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'RRGON RECORDS'                                          
         MVC   P+14(7),=C'CHGED ='                                              
         OI    CHACNT+3,X'0F'                                                   
         UNPK  P+24(7),CHACNT                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'RRGON RECORDS'                                          
         MVC   P+14(7),=C'ADDED ='                                              
         OI    ADDCNT+3,X'0F'                                                   
         UNPK  P+24(7),ADDCNT                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PR20     CLI   LASTSW,C'Y'         TEST LAST REP                                
         BE    PRX                 YES                                          
         MVC   REPCD,RSORTREP      NO-INITIALIZE FOR NEW REP                    
         GOTO1 =V(SORTER),DMCB,A(SORTCD2),(X'80',A(RECCD2)),(X'80',0)           
         XC    SVGROUP,SVGROUP                                                  
         XC    SVSTA,SVSTA                                                      
         MVI   SVSTATY,0                                                        
         XC    SVTVB,SVTVB                                                      
         XC    SVOWNER,SVOWNER                                                  
         XC    SVTEAM,SVTEAM                                                    
         XC    SVOFF,SVOFF                                                      
         XC    SVRANK,SVRANK                                                    
         XC    SVMKT,SVMKT                                                      
         XC    SVADV,SVADV                                                      
         XC    SVAGY,SVAGY                                                      
         XC    SVAFFIL,SVAFFIL                                                  
         XC    SVCTGY,SVCTGY                                                    
*                                                                               
         LA    R4,REPTAB           GET START/END Y/M FOR THIS REP               
*                                                                               
PR22     CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   REPCD,0(R4)                                                      
         BE    *+12                                                             
         LA    R4,L'REPTAB(R4)                                                  
         B     PR22                                                             
*                                                                               
         MVC   SETUPREP,0(R4)      SAVE THIS REP CODE                           
*                                                                               
         MVC   RRGSTART,2(R4)                                                   
         MVC   RRGEND,4(R4)                                                     
         MVC   TYPETAB,8(R4)       SAVE TYPES - ALL/CONF/UNCONF/DIR             
         MVC   DIRECTYP,15(R4)     SAVE DIRECT TYPE                             
         SPACE                                                                  
         GOTO1 SETUPMON            SET UP THE MONTH INFO                        
         L     R4,=A(RECTAB)                                                    
         LA    R0,MAXTYPS          CLEAR RECORD TABLE                           
         XC    0(L'RECTAB,R4),0(R4)                                             
         LA    R4,L'RECTAB(R4)                                                  
         BCT   R0,*-10                                                          
         MVC   DMFILE,RRGON        BUILD NEW RECORD TABLE                       
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         LA    R2,4                                                             
         LA    R3,TYPETAB                                                       
         L     R4,=A(RECTAB)                                                    
PR23     CLI   0(R3),0                                                          
         BE    PR28                                                             
         XC    KEY,KEY                                                          
         MVC   ROKREP,REPCD                                                     
         MVC   ROKHD2,=X'0002'                                                  
         SPACE                                                                  
         MVC   ROKHD2RQ,0(R3)                                                   
         CLI   0(R3),C'A'                                                       
         BNE   *+8                                                              
         MVI   ROKHD2RQ,0                                                       
         SPACE                                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(ROKHD2TY-ROKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   PR26                                                             
         DC    H'0'                RECORD TABLE FULL                            
*                                                                               
PR24     BAS   RE,SEQ              GET THE TYPE 2 HDR RECS                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(ROKHD2TY-ROKEY),KEYSAVE                                      
         BNE   PR28                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   PR26                                                             
         DC    H'0'                RECORD TABLE FULL                            
*                                                                               
PR26     MVC   0(1,R4),ROKHD2RQ                                                 
         MVC   1(7,R4),ROKHD2TY    MOVE RRGON TYPE & STRT/END MONTHS            
         LA    R4,L'RECTAB(R4)      TO RECTAB                                   
         B     PR24                                                             
*                                                                               
PR28     LA    R3,1(,R3)                                                        
         BCT   R2,PR23                                                          
         SPACE                                                                  
         MVI   0(R4),X'FF'                                                      
         CLC   KEY(ROKREST-ROKEY),KEYSAVE   CHECK AT LEAST ONE                  
         BE    *+6                          DETAIL RECORD                       
         DC    H'0'                                                             
         MVC   DMFILE,REPDIR                                                    
         L     R4,SPACENXT                                                      
         ST    R4,SPACEREP                                                      
         XC    KEY,KEY             GET OFFICE REGIONS IN SPACEND                
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPCD                                                  
         BAS   RE,HIGH                                                          
*                                                                               
PR30     CLC   KEY(25),KEYSAVE                                                  
         BNE   PR32                                                             
         BAS   RE,GETOFF                                                        
         MVC   0(2,R4),=X'0406'                                                 
         MVC   2(2,R4),ROFFKOFF                                                 
         MVC   4(2,R4),ROFFREG                                                  
         LA    R4,6(R4)                                                         
         C     R4,=A(SPACENDX)                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SEQ                                                           
         B     PR30                                                             
*                                                                               
PR32     XC    KEY,KEY             GET CATEGORY CLASSES IN SPACEND              
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPCD                                                  
         BAS   RE,HIGH                                                          
*                                                                               
PR34     CLC   KEY(25),KEYSAVE                                                  
         BNE   PR36                                                             
         BAS   RE,GETCAT                                                        
         MVC   0(2,R4),=X'1306'                                                 
         MVC   2(2,R4),RCTGKCTG                                                 
         MVC   4(2,R4),RCTGCLSS                                                 
         LA    R4,6(R4)                                                         
         C     R4,=A(SPACENDX)                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SEQ                                                           
         B     PR34                                                             
*                                                                               
PR36     MVI   0(R4),X'FF'         MARK END OF SPACEND                          
         ST    R4,SPACENXT                                                      
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO ADD UP SORTER RECORDS WITH SAME KEY                                
*                                                                               
*                                                                               
*        ROUTINE TO PROCESS CONTRACT RECORDS                                    
*        COMPARES FIRST CONTRACT TO LAST CONTRACT AND SAVES DIFERENCES          
*                                                                               
SETUPMON NTR1                                                                   
*                                                                               
*        ROUTINE TO SET UP MONTH INFORMATION                                    
*                                                                               
         MVI   SETUPDPC,C' '       CLEAR DAILY FLAG                             
         LA    R4,REPDAILY         FIND REP IN DAILY TABLE                      
SM000    EQU   *                                                                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    SM004               YES - NOT FOUND: NOT DAILY                   
         CLC   SETUPREP,0(R4)      NO  - REP FOUND?                             
         BE    SM002               YES                                          
         LA    R4,LREPSLOT(R4)     NO  - BUMP TO NEXT SLOT                      
         B     SM000               GO BACK FOR NEXT                             
SM002    EQU   *                                                                
         MVC   SETUPDPC,2(R4)      GET FLAG FROM TABLE                          
SM004    EQU   *                                                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(14),=C'SETUPMON FLAG:'                                       
         MVC   P+16(2),0(R4)                                                    
         MVC   P+19(1),SETUPDPC                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         LA    R4,MONINF           SET UP MONTH INFO TABLE                      
         XC    MONINF,MONINF                                                    
         LA    R3,COLVALS                                                       
         USING COLVALD,R3                                                       
         MVC   WORK(2),RRGSTART                                                 
         ZIC   RE,RRGSTART                                                      
         BCTR  RE,0                                                             
         STC   RE,WORK+2                                                        
*                                                                               
         MVC   WORK+3(1),RRGSTART+1                                             
*                                                                               
         LA    R5,24                                                            
*                                                                               
SM010    MVC   0(2,R4),WORK                                                     
         MVC   4(2,R4),WORK+2                                                   
         MVC   COLYRMN(2),WORK                                                  
         MVC   COLPYRM(2),WORK+2                                                
         CLI   WORK+1,12                                                        
         BL    SM020                                                            
         MVI   WORK+1,1                                                         
         MVC   WORK+2(2),WORK                                                   
         IC    RE,WORK                                                          
         LA    RE,1(RE)                                                         
         STC   RE,WORK                                                          
*                                                                               
         B     SM030                                                            
*                                                                               
SM020    IC    RE,WORK+1                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WORK+1                                                        
         STC   RE,WORK+3                                                        
*                                                                               
*M030    CLC   RRGEND,WORK                                                      
SM030    CLC   RRGEND,0(R4)                                                     
         BE    SM040                                                            
         LA    R3,COLVALX-COLVALD(R3)                                           
         LA    R4,8(R4)                                                         
         BCT   R5,SM010                                                         
         DC    H'0'                ERROR - MORE THAN 24 MONTHS                  
*                                                                               
SM040    EQU   *                                                                
         CLC   DATORIDE,SPACES     ANY OVERRIDE DATE?                           
         BNH   SM045               NO                                           
         GOTO1 =V(DATCON),DMCB,(3,DATORIDE),(X'20',WORK)                        
*                                  YES - USE DATE OVERRIDE, NOT TODAY'S         
         B     SM048                                                            
SM045    EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',WORK) SET UP MON FORECAST           
SM048    EQU   *                                                                
         SPACE                                                                  
         PACK  DUB(2),WORK(2)                                                   
         SPACE                                                                  
         CP    DUB(2),=P'0'        FOR 2000 SET TO GET 99                       
         BNE   *+10                                                             
         ZAP   DUB(2),=P'100'                                                   
         SPACE                                                                  
         SP    DUB(2),=P'1'                                                     
         UNPK  WORK+6(2),DUB(2)                                                 
         OI    WORK+7,X'F0'                                                     
         MVC   WORK+8(4),WORK+2                                                 
         GOTO1 =V(GETDAY),DMCB,WORK,FULL                                        
         CLC   WORK(6),=C'851229'                                               
         BH    SM050                                                            
         CLI   DMCB,1                                                           
         BH    SM050                                                            
         GOTO1 =V(ADDAY),DMCB,WORK+6,DUB,7                                      
         MVC   WORK+6(6),DUB                                                    
*                                                                               
SM050    GOTO1 GETMDAY,DMCB,WORK,MONFORC                                        
*                                                                               
*   TEST:                                                                       
         MVC   P+01(18),=C'MONFORC  /FIRST  :'                                  
         MVC   P+22(04),MONFORC                                                 
         MVC   P+30(06),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
*   TEST: END                                                                   
*                                                                               
         GOTO1 GETMDAY,DMCB,WORK+6,MONFORC+2                                    
*                                                                               
*   TEST:                                                                       
         MVC   P+01(18),=C'MONFORC  /GETMDAY:'                                  
         MVC   P+22(04),MONFORC                                                 
         MVC   P+30(06),WORK+6                                                  
         GOTO1 =V(PRINTER)                                                      
*   TEST: END                                                                   
*                                                                               
         MVC   MONFORC+2(2),MONFORC            TEMPTEST                         
*                                                                               
*                                                                               
*   TEST:                                                                       
         MVC   P+01(18),=C'MONFORC  /NEXT    '                                  
         MVC   P+22(04),MONFORC                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST: END                                                                   
*                                                                               
         MVC   DUB(2),RRGSTART     WRITE THE MONTHS TO MONTH TABLES             
         MVI   DUB+2,1                                                          
         GOTO1 =V(DATCON),DMCB,(3,DUB),(X'20',WORK)   START MONTH               
*                                                                               
         MVC   DUB(2),RRGEND                                                    
         MVI   DUB+2,1                                                          
         GOTO1 =V(DATCON),DMCB,(3,DUB),(X'20',WORK+6)  END MONTH                
*                                                                               
         L     R3,=A(MONTAB1)                                                   
         L     R4,=A(MONTAB2)                                                   
         LA    R5,23                                                            
*                                                                               
         MVC   0(6,R3),WORK                                                     
         MVC   0(6,R4),WORK                                                     
*                                                                               
SM060    CLC   0(6,R3),WORK+6                                                   
         BE    SM999                                                            
         LA    R6,40(R3)                                                        
         GOTO1 =V(ADDAY),DMCB,(R3),(R6),45                                      
*                                                                               
         MVC   4(2,R6),=C'01'                                                   
*                                                                               
         LR    R3,R6                                                            
         LA    R4,40(R4)                                                        
         MVC   0(6,R4),0(R3)                                                    
         BCT   R5,SM060                                                         
*                                                                               
SM999    XIT1                                                                   
         EJECT                                                                  
PROCON   NTR1                                                                   
*                                                                               
***      LA    RF,RKEY             SET A(KEY)                                   
***      ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
***      AR    RF,RE               BUMP TO END OF RECORD                        
***      XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT REC END                   
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0040                                                         
**       MVC   P+1(10),=C'PROCON IN '                                           
**       GOTO1 =V(PRINTER)                                                      
**       MVC   P+1(18),=C'PROCESSING CON:  '                                    
**       MVC   P+22(64),RKEY                                                    
**       GOTO1 =V(PRINTER)                                                      
TEST0040 EQU   *                                                                
*                                                                               
         LA    R0,4                                                             
         LA    R1,TYPETAB                                                       
PC010    CLI   0(R1),0                                                          
         BE    PC250                                                            
         STC   R0,TYPECT                                                        
         ST    R1,TYPEPTR                                                       
         MVC   THISTYPE,0(R1)                                                   
         SPACE                                                                  
         MVI   DIRCTFLG,C'N'       THIS IS NOT A DIRECT CONTRACT                
         MVI   CONFLAG,C'N'                                                     
         SPACE                                                                  
         CLI   THISTYPE,C'A'       DOING ALL                                    
         BE    PC030                                                            
         SPACE                                                                  
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    PC030                                                            
         SPACE                                                                  
* DIRECT FOR PETNY IS DIRECT ONLY - HARD CODED                                  
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BE    PC020                                                            
         DC    H'0'                                                             
         SPACE                                                                  
PC020    DS   0H                                                                
         LA    R6,RKEY                                                          
         MVI   ELCODE,X'01'        CHECK FOR DIRECT                             
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   DIRECTYP,RCONTYPE                                                
         BNE   PC030                                                            
         MVI   DIRCTFLG,C'Y'       THIS IS A DIRECT CONTRACT                    
         DROP  R6                                                               
*                                                                               
         CLI   TRACEFLG,C'Y'       TRACE REQUESTED?                             
         BNE   PC030                                                            
*                                                                               
         CLC   TRACECTR,=F'1000'   DISPLAY FIRST 1000                           
         BH    PC030                                                            
         L     RF,TRACECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TRACECTR                                                      
*                                                                               
         MVC   P(6),=C'DIRECT'                                                  
         MVC   P+6(27),RKEY                                                     
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARA,RKEY,WORK,27,=C'SEP'                             
         MVC   P,SPACES                                                         
         MVC   P+6(27),WORK                                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P+6(27),WORK+27                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
* POST MONTHLY TOTALS FOR FIRST RECORD *                                        
         SPACE                                                                  
PC030    MVI   NOPOSTMN,C'N'                                                    
         LA    R3,RKEY                                                          
         USING RCOND,R3                                                         
         SPACE                                                                  
         CLI   RRECTY,3            IF FIRST RECORD IS ADD                       
         BE    PC032                FIRST BUCKETS ARE ZERO                      
         SPACE                                                                  
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    PC031                                                            
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BNE   PC034                                                            
         SPACE                                                                  
         CLC   =C'BL',RCONKREP     IF BLAIR DIRECT, NO CONFIRMED CK             
         BE    PC034                                                            
         CLC   =C'FN',RCONKREP     IF FOX DIRECT, NO CONFIRMED CK               
         BE    PC034                                                            
         CLC   =C'PV',RCONKREP     IF PETNY DIRECT, NO CONFIRMED CK             
         BE    PC034                                                            
         SPACE                                                                  
PC031    DS   0H                                                                
         BAS   RE,CKCF             GO CHECK CONFIRMED                           
         BNE   PC032                NO, NOT CONF                                
         MVI   CONFLAG,C'Y'                                                     
         B     PC034                                                            
         SPACE                                                                  
PC032    MVI   NOPOSTMN,C'Y'       DO NOT POST MONTH                            
         SPACE                                                                  
PC034    L     R4,=A(MONTAB1)                                                   
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0060                                                         
**       MVC   P+1(10),=C'POSTMON 1 '                                           
**       MVC   P+12(64),RCONKEY                                                 
**       GOTO1 =V(PRINTER)                                                      
TEST0060 EQU   *                                                                
*                                                                               
         BAS   RE,POSTMON          POST FIRST RECORD MONTH TABLE                
         SPACE                                                                  
         MVI   NOPOSTMN,C'N'                                                    
         L     R3,=A(LASTREC)                                                   
         LA    R3,LKEY-LRECD(R3)                                                
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    PC035                                                            
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BNE   PC036                                                            
         SPACE                                                                  
         CLC   =C'BL',RCONKREP     IF BLAIR DIRECT, NO CONFIRMED CK             
         BE    PC036                                                            
         CLC   =C'FN',RCONKREP     IF FOX DIRECT, NO CONFIRMED CK               
         BE    PC036                                                            
         CLC   =C'PV',RCONKREP     IF PETNY DIRECT, NO CONFIRMED CK             
         BE    PC036                                                            
         SPACE                                                                  
PC035    BAS   RE,CKCF             GO CHECK CONFIRMED                           
         BE    PC036                                                            
         CLI   CONFLAG,C'N'        WAS 1ST CONTRACT CONFIRMED                   
         BE    PC240                NO                                          
         SPACE                                                                  
         GOTO1 =A(OFFPRNT1)                                                     
         B     PC240                BYPASS                                      
         DROP  R3                                                               
         SPACE                                                                  
PC036    TM    29(R3),X'80'        IF LAST RECORD IS DELETE                     
         BZ    PC038                LAST BUCKETS ARE ZERO                       
         MVI   NOPOSTMN,C'Y'                                                    
         SPACE                                                                  
PC038    L     R4,=A(MONTAB2)                                                   
         SPACE                                                                  
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0070                                                         
**       MVC   P+1(10),=C'POSTMON 2 '                                           
**       MVC   P+12(64),0(R3)                                                   
**       GOTO1 =V(PRINTER)                                                      
***      CLC   =X'06887442',23(R3)                                              
***      BNE   *+6                                                              
***      DC    H'0'                                                             
TEST0070 EQU   *                                                                
*                                                                               
         BAS   RE,POSTMON          POST LAST RECORD MONTH TABLE                 
*                                                                               
         ST    R3,SAVEACON         SAVE A(CONTRACT RECORD)                      
*                                                                               
         SPACE                                                                  
* FIND DIFFERENCES BETWEEN 2 TABLES, AND CURRENT (THIS WEEKS) BOOKED            
         SPACE                                                                  
         LA    R2,MONINF                                                        
         LA    R3,COLVALS                                                       
         USING COLVALD,R3                                                       
         L     R4,=A(MONTAB1)                                                   
         L     R5,=A(MONTAB2)                                                   
         LA    R6,24                                                            
*                                                                               
PC110    DS   0H                                                                
         CLI   THISTYPE,C'D'       DO DIRECT (CONFIRMED IMPLIED)                
         BNE   PC111                                                            
         CLI   DIRCTFLG,C'Y'       THIS A DIRECT CONTRACT                       
         BNE   PC111               NO  - PROCESS ALL BUCKETS                    
         SPACE                                                                  
         CLC   COLYRMN,QWDRCOFF    YES - CHECK CUTOFF DATE                      
         BNL   PC112                                                            
         SPACE                                                                  
PC111    DS   0H                                                                
         USING VALMOND,R4                                                       
*        L     RE,8(R4)            CURRENT AS AT ESTIMATE                       
         L     RE,VALACTB          CURRENT AS AT ESTIMATE                       
*        TM    6(R4),X'01'         TEST FOR CURRENT AS AT INVOICE               
         TM    VALASAT,VALASATC    TEST FOR CURRENT AS AT INVOICE               
         BZ    *+8                                                              
*        L     RE,12(R4)           CURRENT AS AT INVOICE                        
         L     RE,VALACTMN         CURRENT AS AT INVOICE                        
         DROP  R4                                                               
         USING VALMOND,R5                                                       
         SPACE                                                                  
*        L     RF,8(R5)            SAME FOR LAST RECORD                         
         L     RF,VALACTB          CURRENT AS AT ESTIMATE                       
*        TM    6(R5),X'01'                                                      
         TM    VALASAT,VALASATC    TEST FOR CURRENT AS AT INVOICE               
         BZ    *+8                                                              
*        L     RF,12(R5)                                                        
         L     RF,VALACTMN         CURRENT AS AT INVOICE                        
         SR    RF,RE                                                            
         BAS   RE,ROUND                                                         
         ST    RF,CUREST           CURRENT ESTIMATE DIFFERENCE                  
*                                                                               
*   TEST: DISPLAY DIFFERENCE                                                    
**       OC    CUREST,CUREST       ANY DIFFERENCE?                              
**       BZ    TEST0100            NO                                           
**       MVC   P+1(05),=C'DIFF:'                                                
**       EDIT  CUREST,(8,P+10)                                                  
**       L     RF,SAVEACON                                                      
**       GOTO1 =V(HEXOUT),DMCB,23(RF),P+24,4,=C'TOG'                            
**       L     RF,SAVEACON                                                      
**       MVC   P+32(27),0(RF)                                                   
**       GOTO1 =V(PRINTER)                                                      
**       MVC   P+1(64),0(R5)                                                    
**       GOTO1 =V(PRINTER)                                                      
TEST0100 EQU   *                                                                
         DROP  R5                                                               
*                                                                               
*                                                                               
         B     NOPRIEST                                                         
*                                                                               
         L     RE,20(R4)           PRIOR AS AT INVOICE                          
         TM    6(R4),X'04'         TEST FOR PRIOR AS AT INVOICES                
         BZ    *+8                                                              
         L     RE,24(R4)           NONE - USE PRIOR AS AT ESTIMATE              
         SPACE                                                                  
         L     RF,20(R5)           SAME FOR LAST RECORD                         
         TM    6(R5),X'04'                                                      
         BZ    *+8                                                              
         L     RF,24(R5)                                                        
         SR    RF,RE                                                            
         BAS   RE,ROUND                                                         
         ST    RF,PRIEST           PRIOR ESTIMATE DIFFERENCE                    
*                                                                               
NOPRIEST DS   0H                                                                
         L     RE,32(R4)           PRIOR TOTAL INVOICE                          
         TM    6(R4),X'02'         TEST FOR ANY PRIOR INVOICES                  
         BZ    *+8                                                              
         L     RE,36(R4)           NONE - USE PRIOR TOTAL ESTIMATE              
         SPACE                                                                  
         L     RF,32(R5)           SAME FOR LAST RECORD                         
         TM    6(R5),X'02'                                                      
         BZ    *+8                                                              
         L     RF,36(R5)                                                        
         SR    RF,RE                                                            
         BAS   RE,ROUND                                                         
         ST    RF,PRIFIN           PRIOR FINAL DIFFERENCE                       
         SPACE                                                                  
NOPRIOR  DS   0H                                                                
         GOTO1 =A(GETWK)           GET THIS WEEK BOOKING TOTALS                 
         SPACE                                                                  
         BAS   RE,GETCL            GET LAST YEAR AS AT TODAY                    
         SPACE                                                                  
         B     PC113                                                            
         SPACE                                                                  
* ZERO BUCKETS THAT ARE NOT USED *                                              
         SPACE                                                                  
PC112    XC    CUREST,CUREST                                                    
         XC    PRIEST,PRIEST                                                    
         XC    PRIFIN,PRIFIN                                                    
         XC    CURLST,CURLST                                                    
         XC    CURWK,CURWK                                                      
         SPACE                                                                  
PC113    LA    R4,40(R4)           NEXT MONTH                                   
         LA    R5,40(R5)                                                        
         CLI   0(R4),0                                                          
         BE    PC114                                                            
         LA    R3,COLVALX-COLVALD(R3)                                           
         LA    R2,8(,R2)           NEXT MON/YR TABLE ENTRY                      
         BCT   R6,PC110                                                         
         B     PC115                                                            
*                                                                               
PC114    CLI   0(R5),0                                                          
         BE    PC115                                                            
         DC    H'0'                                                             
*                                                                               
PC115    DS   0H                                                                
         L     R4,=A(RECTAB)                                                    
         USING RPTD,R4                                                          
*                                                                               
         MVI   TMPPRTSW,0          USED FOR TRACE                               
*                                                                               
PC120    XC    SORT2REC,SORT2REC   CLEAR SORT REC                               
         LA    R7,SORT2REC                                                      
         USING RORECD,R7                                                        
         MVC   ROKREP,REPCD                                                     
         CLI   THISTYPE,C'A'       DO ALL                                       
         BE    PC122                                                            
         SPACE                                                                  
         MVI   ROKDTLRQ,C'C'                                                    
         CLI   THISTYPE,C'C'       DO CONFIRMED                                 
         BE    PC122                                                            
         SPACE                                                                  
         MVI   ROKDTLRQ,C'D'                                                    
         CLI   THISTYPE,C'D'       DO DIRECT (CONFIRMED IMPLIED)                
         BE    PC122                                                            
         DC    H'0'                                                             
PC122    CLC   ROKDTLRQ,RPTYPE                                                  
         BNE   PC200                                                            
         SPACE                                                                  
         MVC   ROKDTLTY,RPTYP      MOVE RRGON REC TYPE                          
         LA    R5,RPTYP                                                         
         LA    R6,ROKDTLVL                                                      
         LA    R2,RKEY                                                          
         USING RCOND,R2                                                         
         CLC   REPCD,=C'BL'        TEST BLAIR                                   
         BNE   PC125               NO                                           
         CLC   RPTYP,=X'040500'    TEST OFFICE/TEAM                             
         BNE   PC125               NO                                           
         CLC   RCONKOFF,=C'NY'     YES-TEST OFFICE=NY,CH OR LA                  
         BE    PC125                                                            
         CLC   RCONKOFF,=C'CH'                                                  
         BE    PC125                                                            
         CLC   RCONKOFF,=C'LA'                                                  
         BE    PC125                                                            
         CLC   RCONKOFF,=C'DE'     NEW OFFICE 3/2/95                            
         BE    PC125                                                            
         CLC   RCONKOFF,=C'DA'     NEW OFFICE 11/19/97                          
         BE    PC125                                                            
         CLC   RCONKOFF,=C'AT'     NEW OFFICE 6/20/94                           
         BNE   PC200               NO-SKIP                                      
*                                                                               
PC125    LA    R0,3                                                             
*                                                                               
PC130    LA    R1,ROWCTAB          POST COLUMNS                                 
*                                                                               
PC140    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R5)                                                    
         BE    PC150                                                            
         LA    R1,4(R1)                                                         
         B     PC140                                                            
*                                                                               
PC150    L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
*                                                                               
         OC    0(8,R6),0(R6)       THIS A NULL CODE                             
         BZ    PC194                                                            
*                                                                               
         LA    R5,1(R5)                                                         
         CLI   0(R5),0                                                          
         BE    PC160                                                            
         LA    R6,8(R6)                                                         
         BCT   R0,PC130                                                         
*                                                                               
PC160    LA    R3,COLVALS          POST MONTH                                   
         USING COLVALD,R3                                                       
*                                                                               
PC170    DS    0H                                                               
         CLI   THISTYPE,C'D'       DO DIRECT (CONFIRMED IMPLIED)                
         BNE   PC174                                                            
         CLI   DIRCTFLG,C'Y'       THIS A DIRECT CONTRACT                       
         BNE   PC174                NO  - PROCESS ALL BUCKETS                   
         CLC   COLYRMN,QWDRCOFF                                                 
         BNL   PC200                                                            
         SPACE                                                                  
PC174    CLC   COLYRMN,RPENYR      TEST BEYOND LAST MONTH                       
         BH    PC200               YES - DONE FOR THIS RRGON TYPE               
         MVC   ROKDTLYM,COLYRMN    MOVE YR/MN                                   
         CLC   COLYRMN(1),RPSTYR   TEST BEFORE START YEAR                       
         BL    PC190               LOW - NEXT MONTH                             
         BH    PC180                                                            
         SPACE                                                                  
         CLC   COLYRMN,RPSTYR      EQUAL - TEST FOR PRIOR MONTH                 
         BNL   PC180                                                            
         MVI   ROKDTLYM+1,0        PRIOR MONTH = MONTH 0                        
*                                                                               
PC180    DS   0H                                                                
         MVC   RODPCBLG,CUREST     POST DATA                                    
         MVC   RODPPBLG,PRIEST                                                  
         MVC   RODPPFIN,PRIFIN                                                  
         MVC   RODYCBLG,CUREST                                                  
         MVC   RODYPBLG,PRIEST                                                  
         MVC   RODYPFIN,PRIFIN                                                  
*                                                                               
         MVC   RODCWKBT,CURWK                                                   
         MVC   RODYPBLC+4(4),LSTWK                                              
*                                                                               
         MVC   RODPPBLC,CURLST                                                  
*        MVC   RODYPBLC,CURLST     YTD NOW PRIOR YR-1                           
*                                                                               
         OC    RODATA(76),RODATA   TEST FOR ANY COL VALS                        
         BZ    PC190                                                            
*                                                                               
         CLI   TRACEFLG,C'T'      REQUEST FOR SORTED RECS                       
         BNE   PC184                                                            
         CLI   TMPPRTSW,1          IS THIS THE LAST                             
         BE    PC184                                                            
*                                                                               
*                                                                               
         CLC   TRACECTR,=F'1000'   DISPLAY FIRST 1000                           
         BH    PC184                                                            
         L     RF,TRACECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TRACECTR                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(HEXOUT),DMCB,RCONKCON-RCOND+RKEY,P+15,4                       
         MVC   P+5(10),=C'CONTRACT ='                                           
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTRRGS          TEMPX                                        
*                                                                               
PC184    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORT2REC                                 
*                                                                               
PC190    LA    R3,COLVALX-COLVALD(R3)  NEXT MONTH                               
         B     PC170                                                            
*                                                                               
PC194    AP    TOTNUL,=P'1'                                                     
         SPACE                                                                  
PC200    LA    R4,RPTX-RPTD(R4)    NEXT RRGON TYPE                              
         MVI   TMPPRTSW,1          TEMPX                                        
         CLI   0(R4),X'FF'                                                      
         BNE   PC120                                                            
         SPACE                                                                  
PC240    ZIC   R0,TYPECT                                                        
         L     R1,TYPEPTR                                                       
         SPACE                                                                  
PC250    LA    R1,1(,R1)                                                        
         BCT   R0,PC010                                                         
         SPACE                                                                  
PC999    EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0050                                                         
**       MVC   P+1(10),=C'PROCON OUT'                                           
**       GOTO1 =V(PRINTER)                                                      
TEST0050 EQU   *                                                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         DS    0F                                                               
ROWCTAB  DC    AL1(QLSTA),AL3(CSTA)                                             
         DC    AL1(QLREG),AL3(CRGN)                                             
         DC    AL1(QLOFF),AL3(COFF)                                             
         DC    AL1(QLTEM),AL3(CTEAM)                                            
         DC    AL1(QLSAL),AL3(CSAL)                                             
         DC    AL1(QLGRP),AL3(CGROUP)                                           
         DC    AL1(QLADV),AL3(CADVER)                                           
         DC    AL1(QLAGY),AL3(CAGENC)                                           
         DC    AL1(QLAFF),AL3(CAFFIL)                                           
         DC    AL1(QLCLS),AL3(CCLASS)                                           
         DC    AL1(QLCAT),AL3(CCTGY)                                            
         DC    AL1(QLSTY),AL3(CSTATY)                                           
         DC    AL1(QLTVB),AL3(CTVB)                                             
         DC    AL1(QLOWN),AL3(COWNER)                                           
         DC    AL1(QLGRGRP),AL3(CGRGRP)                                         
         DC    AL1(QLRNK),AL3(CRANK)                                            
         DC    AL1(QLCON),AL3(CCONTY)                                           
         DC    AL1(QLMKT),AL3(CMKT)                                             
         DC    AL1(QLDCT),AL3(CDCT)                                             
         DC    AL1(QLAGENCY),AL3(CAGENCY)                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
QLSTA    EQU   02                                                               
QLREG    EQU   03                                                               
QLOFF    EQU   04                                                               
QLTEM    EQU   05                                                               
QLSAL    EQU   06                                                               
QLGRP    EQU   07                                                               
QLADV    EQU   08                                                               
QLAGY    EQU   10                                                               
QLAFF    EQU   11                                                               
QLCLS    EQU   13                                                               
QLCAT    EQU   15                                                               
QLSTY    EQU   22                                                               
QLTVB    EQU   23                                                               
QLOWN    EQU   24                                                               
QLGRGRP  EQU   25                                                               
QLRNK    EQU   30                                                               
QLCON    EQU   33                                                               
QLMKT    EQU   39                                                               
QLDCT    EQU   45                                                               
QLAGENCY EQU   47                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
CSTA     MVC   0(4,R6),RCONKSTA                                                 
         MVC   4(3,R6),=C'-TV'                                                  
         CLI   RCONKSTA+4,C' '                                                  
         BNH   CSTA20                                                           
         CLI   RCONKSTA+4,C'T'                                                  
         BE    CSTA20                                                           
         SPACE                                                                  
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   CSTA10                                                           
         MVC   4(2,R6),=C'-L'                                                   
         MVI   6(R6),0                                                          
         B     CSTA20                                                           
         SPACE                                                                  
CSTA10   MVI   6(R6),C'M'                                                       
         MVC   5(1,R6),RCONKSTA+4                                               
         SPACE                                                                  
CSTA20   CLI   3(R6),C' '                                                       
         BHR   RE                                                               
         MVC   3(3,R6),4(R6)                                                    
         MVI   6(R6),0                                                          
         BR    RE                                                               
         SPACE                                                                  
CRGN     L     RF,SPACEREP                                                      
         SR    R1,R1                                                            
*                                                                               
CRGN1    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),4                                                          
         BNE   *+14                                                             
         CLC   RCONKOFF,2(RF)                                                   
         BE    CRGN2                                                            
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CRGN1                                                            
*                                                                               
CRGN2    MVC   0(2,R6),4(RF)                                                    
         BR    RE                                                               
         SPACE                                                                  
COFF     MVC   0(2,R6),RCONKOFF                                                 
         BR    RE                                                               
         SPACE                                                                  
CTEAM    MVC   0(2,R6),RCONTEM                                                  
         BR    RE                                                               
         SPACE                                                                  
CSAL     MVC   0(3,R6),RCONSAL                                                  
         BR    RE                                                               
         SPACE                                                                  
CGROUP   MVC   0(2,R6),RCONKGRP                                                 
         BR    RE                                                               
         SPACE                                                                  
CADVER   MVC   0(4,R6),RCONKADV                                                 
         CLI   3(R6),C' '          IF 4TH POS BLK                               
         BNER  RE                                                               
         MVI   3(R6),0             FORCE TO NULL                                
         CLI   2(R6),C' '          IF 3RD POS BLK                               
         BNER  RE                                                               
         MVI   2(R6),0             FORCE TO NULL                                
         BR    RE                                                               
         SPACE                                                                  
CAGENC   MVC   0(4,R6),RCONKAGY                                                 
         MVC   4(2,R6),RCONKAOF                                                 
         CLI   5(R6),C' '          IF 6TH POS BLK                               
         BNER  RE                                                               
         MVI   5(R6),0             FORCE TO NULL                                
         CLI   4(R6),C' '          IF 5TH POS BLK                               
         BNER  RE                                                               
         MVI   4(R6),0             FORCE TO NULL                                
         CLI   3(R6),C' '          IF 4TH POS BLK                               
         BNER  RE                                                               
         MVI   3(R6),0             FORCE TO NULL                                
         CLI   2(R6),C' '          IF 3RD POS BLK                               
         BNER  RE                                                               
         MVI   2(R6),0             FORCE TO NULL                                
         BR    RE                                                               
         SPACE                                                                  
CAGENCY  MVC   0(4,R6),RCONKAGY                                                 
         CLI   3(R6),C' '          IF BLANK, MAKE NULL                          
         BH    *+8                                                              
         MVI   3(R6),0                                                          
         CLI   2(R6),C' '          IF BLANK, MAKE NULL                          
         BHR   RE                                                               
         MVI   2(R6),0                                                          
         BR    RE                                                               
         SPACE                                                                  
CGRGRP   MVC   0(1,R6),RCONKGRP                                                 
         BR    RE                                                               
         SPACE 1                                                                
CCLASS   CLC   RCONCTGY,SVCTGY                                                  
         BE    CCLASS6                                                          
         MVC   SVCTGY,RCONCTGY                                                  
         MVC   SVCLASS,SPACES                                                   
         L     R3,SPACEREP                                                      
         SR    R1,R1               FIND CATEGORY IN SPACEND                     
CCLASS2  CLI   0(R3),X'FF'                                                      
         BE    CCLASS6                                                          
         CLI   0(R3),X'13'                                                      
         BNE   CCLASS4                                                          
         CLC   RCONCTGY,2(R3)                                                   
         BH    CCLASS4                                                          
         BL    CCLASS6                                                          
         MVC   SVCLASS,4(R3)                                                    
         B     CCLASS6                                                          
CCLASS4  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CCLASS2                                                          
CCLASS6  MVC   0(2,R6),SVCLASS                                                  
         BR    RE                                                               
         SPACE 1                                                                
CCTGY    MVC   0(2,R6),RCONCTGY                                                 
         BR    RE                                                               
         SPACE 1                                                                
CAFFIL   MVC   0(3,R6),SVAFFIL                                                  
         CLC   SVSTA,RCONKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(3,R6),SVAFFIL                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
CSTATY   MVC   0(1,R6),SVSTATY                                                  
         CLI   SVSTATY,0                                                        
         BE    *+12                                                             
         CLC   RCONKSTA,SVSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTATY                                                      
         MVC   0(1,R6),SVSTATY                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
CTVB     MVC   0(2,R6),SVTVB                                                    
         CLC   SVSTA,RCONKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(2,R6),SVTVB                                                    
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
COWNER   MVC   0(3,R6),SVOWNER                                                  
         CLC   SVSTA,RCONKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(3,R6),SVOWNER                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
CCONTY   MVC   0(1,R6),RCONTYPE                                                 
         MVI   1(R6),C' '                                                       
         BR    RE                                                               
         SPACE 1                                                                
CRANK    MVC   0(1,R6),SVRANK                                                   
         CLC   SVSTA,RCONKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(1,R6),SVRANK                                                   
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
CMKT     MVC   0(4,R6),SVMKT                                                    
         CLC   SVSTA,RCONKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RCONKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(4,R6),SVMKT                                                    
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
CDCT     ST    R6,WORK                                                          
         ST    RE,WORK+4                                                        
         ST    R0,WORK+8                                                        
         XC    HALF,HALF                                                        
         SPACE                                                                  
         LA    R6,RKEY                                                          
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEM                           
         BAS   RE,GETEL                                                         
         USING RCONDVEL,R6                                                      
         BNE   CDCT10                                                           
         MVC   HALF,RCONDVCT                                                    
         DROP  R6                                                               
CDCT10   L     R6,WORK                                                          
         L     RE,WORK+4                                                        
         L     R0,WORK+8                                                        
         MVC   0(2,R6),HALF                                                     
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
PROBUD   NTR1                                                                   
*                                                                               
*        ROUTINE TO PROCESS BUDGET RECORDS                                      
*                                                                               
         LA    R2,RKEY                                                          
         USING RBUDD,R2                                                         
         PACK  DUB,RBUDKYR                                                      
         CVB   R1,DUB                                                           
         STC   R1,BUDYEAR          BUDGET YEAR                                  
         CLI   RRECTY,3            IF FIRST RECORD IS ADD                       
         BNE   *+10                 THEN BEGINNING BUDGETS ARE ZERO             
         XC    RBUDSJAN,RBUDSJAN                                                
         LA    R4,RBUDSJAN                                                      
         L     R3,=A(LASTREC)                                                   
         LA    R3,LKEY-LRECD(R3)                                                
         LA    R5,RBUDSJAN-RBUDREC(R3)                                          
         TM    29(R3),X'80'        IF LAST RECORD IS DELETE                     
         BZ    *+10                 THEN LAST BUDGETS ARE ZERO                  
         XC    0(L'RBUDSJAN,R5),0(R5)                                           
         LA    R0,12                                                            
*                                                                               
PB010    ICM   RE,15,0(R4)         CALCULATE BUDGET DIFFERENCES                 
         ICM   RF,15,0(R5)                                                      
         SR    RF,RE                                                            
         CLI   BUDYEAR,84          1984 FIGURES ARE IN THOUSANDS                
         BNE   *+8                                                              
         MH    RF,=H'1000'                                                      
         STCM  RF,15,0(R4)         DIFFS GO INTO FIRST REC                      
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,PB010                                                         
*                                                                               
         LA    R0,MAXCTBUD         CLEAR CONTRACT TYPE BUDGET TABLE             
         L     R1,=A(CTBUDTAB)                                                  
         XC    0(L'CTBUDTAB,R1),0(R1)                                           
         LA    R1,L'CTBUDTAB(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         CLI   RRECTY,3            IF FIRST RECORD IS NOT AN ADD,               
         BE    PB013                                                            
         L     R5,=A(CTBUDTAB)     SCAN REC FOR CONTRACT TYPE BUDGETS           
         LA    RF,MAXCTBUD                                                      
         LA    R4,RBUDELEM                                                      
         SR    R0,R0                                                            
*                                                                               
PB011    CLI   0(R4),0                                                          
         BE    PB013                                                            
         CLI   0(R4),2                                                          
         BNE   PB012                                                            
         USING RBUDELE2,R4                                                      
         MVC   0(1,R5),RBUDTYPE    FOUND-MOVE BUDGETS TO TABLE                  
         OI    1(R5),X'01'         INDICATE FROM FIRST RECORD                   
         MVC   2(48,R5),RBUDSTA2                                                
         LA    R5,L'CTBUDTAB(R5)                                                
         BCT   RF,PB012                                                         
         DC    H'0'                INCREASE MAXCTBUD                            
*                                                                               
PB012    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PB011                                                            
*                                                                               
PB013    TM    29(R3),X'80'        TEST LAST RECORD IS A DELETE                 
         BO    PB024                                                            
         LA    R4,RBUDELEM-RBUDREC(R3)  NO-SCAN LAST RECORD FOR                 
*                                          CONTRACT TYPE BUDGETS                
PB014    CLI   0(R4),0                                                          
         BE    PB024                                                            
         CLI   0(R4),2                                                          
         BNE   PB022                                                            
         USING RBUDELE2,R4                                                      
         L     R5,=A(CTBUDTAB)     FOUND-SEE IF CONTRACT TYPE                   
         LA    R6,MAXCTBUD               BUDGET WAS IN FIRST RECORD             
*                                                                               
PB015    CLI   0(R5),0                                                          
         BNE   PB016                                                            
         MVC   0(1,R5),RBUDTYPE    NO-ADD TO TABLE                              
         OI    1(R5),X'02'         INDICATE FROM 2ND RECORD                     
         MVC   2(48,R5),RBUDSTA2                                                
         B     PB022                                                            
*                                                                               
PB016    CLC   RBUDTYPE,0(R5)                                                   
         BNE   PB020                                                            
         OI    1(R5),X'02'         YES-CALCULATE BUDGET DIFFERENECS             
         LA    R0,12                                                            
         LA    R1,RBUDSTA2                                                      
         LA    R5,2(R5)                                                         
*                                                                               
PB018    L     RE,0(R5)                                                         
         L     RF,0(R1)                                                         
         SR    RF,RE                                                            
         ST    RF,0(R5)                                                         
         LA    R1,4(R1)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,PB018                                                         
         B     PB022                                                            
*                                                                               
PB020    LA    R5,L'CTBUDTAB(R5)                                                
         BCT   R6,PB015                                                         
         DC    H'0'                INCREASE MAXCTBUD                            
*                                                                               
PB022    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PB014                                                            
*                                                                               
*                                                                               
PB024    L     R4,=A(RECTAB)       FOR EACH RRGON TYPE -                        
         USING RPTD,R4                                                          
*                                                                               
PB026    CLC   BUDYEAR,RPSTYR                                                   
         BL    PB090               BUDGET YEAR LOW                              
         CLC   BUDYEAR,RPENYR                                                   
         BH    PB090               BUDGET YEAR HIGH                             
         LA    R1,BUDTYPES                                                      
*                                                                               
PB027    CLC   RPTYP,0(R1)         ALLOW ONLY CERTAIN RRGON TYPES               
         BE    PB028                                                            
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    PB090                                                            
         B     PB027                                                            
*                                                                               
PB028    LA    R0,3                TEST OFFICE IS IN KEY                        
         LA    R1,RPTYP                                                         
         CLI   0(R1),4                                                          
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     *+14                                                             
         OC    RBUDKTEM,RBUDKTEM   YES - TEST FOR OFFICE IN BUDGET KEY          
         BZ    PB090                     NO - SKIP                              
         XC    ACTBUD,ACTBUD                                                    
         LA    R0,3                TEST CONTRACT TYPE IS IN KEY                 
         LA    R1,RPTYP                                                         
         CLI   0(R1),33                                                         
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     PB030                                                            
         OC    RBUDKTEM,RBUDKTEM   YES-OFFICE MUST BE IN BUDGET KEY             
         BZ    PB090                                                            
         L     R1,=A(CTBUDTAB)     OK-INDICATE CONTRACT TYPE IN KEY             
         CLI   0(R1),0             AND THERE MUST BE CONTRACT TYPE              
         BE    PB090               BUDGETS                                      
         ST    R1,ACTBUD              BY SETTING ADDRESS OF FIRST               
*                                                                               
PB030    XC    SORT2REC,SORT2REC   CLEAR SORT REC                               
         LA    R7,SORT2REC                                                      
         USING RORECD,R7                                                        
         MVC   ROKREP,REPCD                                                     
         MVC   ROKDTLTY,RPTYP      MOVE IN RGGON TYPE                           
         LA    R5,RPTYP                                                         
         LA    R6,ROKDTLVL                                                      
         LA    R0,3                                                             
*                                                                               
PB040    LA    R1,ROWBTAB          POST KEY VALUES                              
*                                                                               
PB042    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R5)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PB042                                                            
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),0                                                          
         BE    PB050                                                            
         LA    R6,8(R6)                                                         
         BCT   R0,PB040                                                         
*                                                                               
PB050    LA    R3,RBUDSJAN         POST MONTH ROW                               
         ICM   R1,15,ACTBUD        TEST CONTRACT TYPE BUDGETS                   
         BZ    *+8                                                              
         LA    R3,2(R1)            YES-R3=A(CURRENT CONTRACT TYPE               
         LA    R6,1                         BUDGETS)                            
         LA    R0,12                                                            
*                                                                               
PB060    LR    R1,R6               R6 = MONTH                                   
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R1,R3               R1 = A(MONTH BUDGET DIFF)                    
         MVC   ROKDTLYM(1),BUDYEAR MOVE YEAR                                    
         STC   R6,ROKDTLYM+1        AND MONTH                                   
         CLC   BUDYEAR,RPENYR      TEST FOR END YEAR                            
         BNE   PB070                                                            
         CLM   R6,1,RPENMN         YES - TEST FOR BEYOND LAST MONTH             
         BH    PB090                                                            
         B     PB080                                                            
*                                                                               
PB070    CLC   BUDYEAR,RPSTYR      TEST FOR START YEAR                          
         BNE   PB080                                                            
         CLM   R6,1,RPSTMN         YES - TEST FOR BEFORE FIRST MONTH            
         BNL   PB080                                                            
         MVI   ROKDTLYM+1,0              YES - PRIOR MONTH 0                    
*                                                                               
PB080    ICM   RE,15,ACTBUD        TEST CONTRACT TYPE BUDGETS                   
         BZ    PB082                                                            
         TM    1(RE),X'20'         YES-TEST THIS CONTRACT TYPE IS IN            
         BO    PB082                   LAST BUDGET RECORD                       
         L     R1,0(R1)            NO-NEGATE THE BUDGETS                        
         LCR   R1,R1                                                            
         ST    R1,RODPCBUD                                                      
         ST    R1,RODYCBUD                                                      
         B     PB084                                                            
*                                                                               
PB082    MVC   RODPCBUD,0(R1)      POST DATA                                    
         MVC   RODYCBUD,0(R1)                                                   
*                                                                               
PB084    OC    RODATA,RODATA       TEST ANY DATA                                
         BZ    PB088                                                            
*                                                                               
         MVC   ROKDTLRQ,RPTYPE                                                  
*                                                                               
         CLI   TRACEFLG,C'D'      REQUEST FOR DATE DISPLAY?                     
         BNE   PB086                                                            
*                                                                               
*                                                                               
         CLC   TRACECTR,=F'1000'   DISPLAY FIRST 1000                           
         BH    PB086                                                            
         L     RF,TRACECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TRACECTR                                                      
*                                                                               
         MVC   P(6),=C'TO SRT'                                                  
         MVC   P+6(116),0(R7)                                                   
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARA,(R7),WORK,116,=C'SEP'                            
         MVC   P,SPACES                                                         
         MVC   P+6(116),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+6(116),WORK+116                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PB086    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORT2REC                                 
*                                                                               
PB088    LA    R6,1(R6)            NEXT MONTH                                   
         BCT   R0,PB060                                                         
*                                                                               
         ICM   R1,15,ACTBUD                                                     
         BZ    PB090                                                            
         LA    R1,L'CTBUDTAB(R1)                                                
         CLI   0(R1),0                                                          
         BE    PB090                                                            
         ST    R1,ACTBUD                                                        
         B     PB030                                                            
*                                                                               
PB090    LA    R4,RPTX-RPTD(R4)    NEXT RRGON TYPE                              
         CLI   0(R4),X'FF'                                                      
         BNE   PB026                                                            
*                                                                               
PB999    B     EXIT                                                             
         SPACE 3                                                                
BUDTYPES DS   0C                                                                
         DC    AL1(25,00,00)       GRGRP                                        
         DC    AL1(07,00,00)       GROUP                                        
         DC    AL1(07,03,00)       GROUP/REGION                                 
         DC    AL1(07,04,00)       GROUP/OFFICE                                 
         DC    AL1(07,04,05)       GROUP/OFFICE/TEAM                            
         DC    AL1(07,04,22)       GROUP/OFFICE/STATION TYPE                    
         DC    AL1(07,04,33)       GROUP/OFFICE/CONTYPE                         
         DC    AL1(07,22,00)       GROUP/STATION TYPE                           
         DC    AL1(07,23,00)       GROUP/TVB                                    
         DC    AL1(07,30,00)       GROUP/MARKET RANK                            
         DC    AL1(07,33,00)       GROUP/CONTYPE                                
         DC    AL1(02,00,00)       STATION                                      
         DC    AL1(02,04,00)       STATION/OFFICE                               
         DC    AL1(02,04,33)       STATION/OFFICE/CONTYPE                       
         DC    AL1(02,33,00)       STATION/CONTYPE                              
         DC    AL1(02,39,00)       STATION/MARKET                               
         DC    AL1(04,05,00)       OFFICE/TEAM                                  
         DC    AL1(04,05,22)       OFFICE/TEAM/STATION TYPE                     
         DC    AL1(04,22,00)       OFFICE/STATION TYPE                          
         DC    AL1(04,24,00)       OFFICE/OWNER                                 
         DC    AL1(04,24,39)       OFFICE/OWNER/MARKET                          
         DC    AL1(04,33,00)       OFFICE/CONTYPE                               
         DC    AL1(04,39,00)       OFFICE/MARKET                                
         DC    AL1(22,00,00)       STATION TYPE                                 
         DC    AL1(23,00,00)       TVB                                          
         DC    AL1(24,00,00)       OWNER                                        
         DC    AL1(24,39,00)       OWNER/MARKET                                 
         DC    AL1(30,00,00)       MARKET RANK                                  
         DC    AL1(33,00,00)       CONTYPE                                      
         DC    AL1(33,39,00)       CONTYPE/MARKET                               
         DC    AL1(39,00,00)       MARKET                                       
         DC    X'FF'                                                            
         EJECT                                                                  
ROWBTAB  DC    AL1(02),AL3(BSTA)                                                
         DC    AL1(03),AL3(BRGN)                                                
         DC    AL1(04),AL3(BOFF)                                                
         DC    AL1(05),AL3(BTEAM)                                               
         DC    AL1(07),AL3(BGROUP)                                              
         DC    AL1(11),AL3(BAFFIL)                                              
         DC    AL1(22),AL3(BSTATY)                                              
         DC    AL1(23),AL3(BTVB)                                                
         DC    AL1(24),AL3(BOWNER)                                              
         DC    AL1(25),AL3(BGRGRP)                                              
         DC    AL1(30),AL3(BRANK)                                               
         DC    AL1(33),AL3(BCONTY)                                              
         DC    AL1(39),AL3(BMKT)                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0H                                                               
BSTA     MVC   0(4,R6),RBUDKSTA                                                 
         MVC   4(3,R6),=C'-TV'                                                  
         CLI   RBUDKSTA+4,C' '                                                  
         BNH   BSTA2                                                            
         CLI   RBUDKSTA+4,C'T'                                                  
         BE    BSTA2                                                            
         MVI   6(R6),C'M'                                                       
         MVC   5(1,R6),RBUDKSTA+4                                               
BSTA2    CLI   3(R6),C' '                                                       
         BHR   RE                                                               
         MVC   3(3,R6),4(R6)                                                    
         MVI   6(R6),0                                                          
         BR    RE                                                               
         SPACE                                                                  
BOFF     MVC   0(2,R6),RBUDKTEM                                                 
         BR    RE                                                               
         SPACE                                                                  
BRGN     L     RF,SPACEREP                                                      
         SR    R1,R1                                                            
*                                                                               
BRGN1    CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLI   0(RF),4                                                          
         BNE   *+14                                                             
         CLC   RBUDKTEM,2(RF)                                                   
         BE    BRGN2                                                            
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     BRGN1                                                            
*                                                                               
BRGN2    MVC   0(2,R6),4(RF)                                                    
         BR    RE                                                               
         SPACE                                                                  
BGROUP   MVC   0(2,R6),SVGROUP                                                  
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         BAS   RE,GETSTA                                                        
         MVC   0(2,R6),SVGROUP                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BGRGRP   MVC   0(1,R6),SVGROUP                                                  
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         BAS   RE,GETSTA                                                        
         MVC   0(1,R6),SVGROUP                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BAFFIL   MVC   0(3,R6),SVAFFIL                                                  
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(3,R6),SVAFFIL                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BSTATY   MVC   0(1,R6),SVSTATY                                                  
         CLI   SVSTATY,0                                                        
         BE    *+12                                                             
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTATY                                                      
         MVC   0(1,R6),SVSTATY                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BTVB     MVC   0(2,R6),SVTVB                                                    
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(2,R6),SVTVB                                                    
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BOWNER   MVC   0(3,R6),SVOWNER                                                  
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(3,R6),SVOWNER                                                  
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BTEAM    MVC   0(2,R6),SVTEAM      TEAM                                         
         OC    SVTEAM,SVTEAM                                                    
         BZ    BTEAM2                                                           
         CLC   SVSTA,RBUDKSTA      TEST STATION/OFFICE CHANGE                   
         BNE   BTEAM2                                                           
         CLC   SVOFF,RBUDKTEM                                                   
         BER   RE                                                               
BTEAM2   LR    RF,RE               YES-GET TEAM                                 
         MVC   SVSTA,RBUDKSTA                                                   
         MVC   SVOFF,RBUDKTEM                                                   
         BAS   RE,GETSTA                                                        
         BAS   RE,GETTEAM                                                       
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BRANK    MVC   0(1,R6),SVRANK      MARKET RANK                                  
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(1,R6),SVRANK                                                   
         LR    RE,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BCONTY   L     R1,ACTBUD           CONTRACT TYPE                                
         MVC   0(1,R6),0(R1)                                                    
         MVI   1(R6),C' '                                                       
         BR    RE                                                               
         SPACE 1                                                                
BMKT     MVC   0(4,R6),SVMKT       MARKET                                       
         CLC   SVSTA,RBUDKSTA                                                   
         BER   RE                                                               
         LR    RF,RE                                                            
         MVC   SVSTA,RBUDKSTA                                                   
         BAS   RE,GETSTA                                                        
         MVC   0(4,R6),SVMKT                                                    
         LR    RE,RF                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
* GET LAST YEAR AS AT TODAY                                  *                  
* IF DOING CONFIRMED OR DIRECT, WILL ONLY COME HERE IF LAST  *                  
* REC IS CONFIRMED, HOWEVER, FIRST REC MAY NOT BE CONFIRMED  *                  
         SPACE 2                                                                
         USING COLVALD,R3                                                       
GETCL    NTR1                                                                   
         XC    CURLSTF,CURLSTF                                                  
         XC    CURLSTL,CURLSTL                                                  
         MVI   ELCODE,X'03'                                                     
         SPACE                                                                  
         CLI   RRECTY,3            IF FIRST RECORD IS ADD                       
         BE    GCL030               IGNORE FIRST REC                            
         SPACE                                                                  
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    GCL000               YES                                         
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BNE   GCL006               NO                                          
         SPACE                                                                  
*  IF BLAIR, FOX, OR PETNY DIRECT, NO CONFIRMED CK                              
         SPACE                                                                  
         CLC   =C'BL',RCONKREP-RCOND+RKEY                                       
         BE    GCL006               YES                                         
         CLC   =C'FN',RCONKREP-RCOND+RKEY                                       
         BE    GCL006               YES                                         
         CLC   =C'PV',RCONKREP-RCOND+RKEY                                       
         BE    GCL006               YES                                         
         SPACE                                                                  
GCL000   CLI   CONFLAG,C'N'        NOT CONFIRMED                                
         BE    GCL030               IGNORE FIRST REC                            
         SPACE                                                                  
GCL006   LA    R6,RKEY                                                          
         BAS   RE,GETEL                                                         
         BNE   GCL030                                                           
         USING RCONBKEL,R6                                                      
GCL010   CLC   RCONBKYR(2),4(R2)   THIS LAST YR/MON                             
         BNE   GCL020                                                           
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,CURLSTF                                                       
         ST    R0,CURLSTF                                                       
         SPACE                                                                  
GCL020   BAS   RE,NEXTEL                                                        
         BE    GCL010                                                           
         DROP  R6                                                               
GCL030   L     R6,=A(LASTREC)                                                   
         LA    R6,LKEY-LRECD(R6)                                                
         TM    29(R6),X'80'        IF LAST RECORD IS DELETE                     
         BO    GCL060               LAST BUCKETS ARE ZERO                       
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BNE   GCL060                                                           
         USING RCONBKEL,R6                                                      
GCL040   CLC   RCONBKYR(2),4(R2)   THIS LAST YR/MON                             
         BNE   GCL050                                                           
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,CURLSTL                                                       
         ST    R0,CURLSTL                                                       
         SPACE                                                                  
GCL050   BAS   RE,NEXTEL                                                        
         BE    GCL040                                                           
         DROP  R6                                                               
         SPACE                                                                  
GCL060   MVI   BYTE,0              SET FIRST TIME FLAG                          
         SPACE                                                                  
         MVI   ELCODE,X'04'                                                     
         SPACE                                                                  
         CLI   RRECTY,3            IF FIRST RECORD IS ADD                       
         BE    GCL090               IGNORE FIRST REC                            
         SPACE                                                                  
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    GCL064               YES                                         
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BNE   GCL066               NO                                          
         SPACE                                                                  
*  IF BLAIR, FOX, OR PETNY DIRECT, NO CONFIRMED CK                              
         SPACE                                                                  
         CLC   =C'BL',RCONKREP-RCOND+RKEY                                       
         BE    GCL066               YES                                         
         CLC   =C'FN',RCONKREP-RCOND+RKEY                                       
         BE    GCL066               YES                                         
         CLC   =C'PV',RCONKREP-RCOND+RKEY                                       
         BE    GCL066               YES                                         
         SPACE                                                                  
GCL064   CLI   CONFLAG,C'N'        NOT CONFIRMED                                
         BE    GCL090               IGNORE FIRST REC                            
         SPACE                                                                  
GCL066   LA    R6,RKEY                                                          
         BAS   RE,GETEL                                                         
         BNE   GCL090                                                           
         USING RCONBKEL,R6                                                      
GCL070   CLC   RCONBKYR(2),4(R2)   THIS LAST YR/MON                             
         BNE   GCL080                                                           
         SPACE                                                                  
         CLI   BYTE,0              THIS FIRST TIME                              
         BNE   GCL074               NOPE                                        
         MVI   BYTE,1                                                           
         XC    CURLSTF,CURLSTF     ZERO OUT ORDERED                             
         SPACE                                                                  
GCL074   DS    0H                                                               
         ICM   R0,15,RCONBKAM                                                   
         A     R0,CURLSTF                                                       
         ST    R0,CURLSTF                                                       
         SPACE                                                                  
GCL080   BAS   RE,NEXTEL                                                        
         BE    GCL070                                                           
         DROP  R6                                                               
GCL090   L     R6,=A(LASTREC)                                                   
         LA    R6,LKEY-LRECD(R6)                                                
         TM    29(R6),X'80'        IF LAST RECORD IS DELETE                     
         BO    GCL120               LAST BUCKETS ARE ZERO                       
         SPACE                                                                  
         MVI   BYTE,0              SET FIRST TIME FLAG                          
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BNE   GCL120                                                           
         USING RCONBKEL,R6                                                      
GCL100   CLC   RCONBKYR(2),4(R2)   THIS LAST YR/MON                             
         BNE   GCL110                                                           
         SPACE                                                                  
         SPACE                                                                  
         CLI   BYTE,0              THIS FIRST TIME                              
         BNE   GCL104               NOPE                                        
         MVI   BYTE,1                                                           
         XC    CURLSTL,CURLSTL     ZERO OUT ORDERED                             
         SPACE                                                                  
GCL104   DS    0H                                                               
         ICM   R0,15,RCONBKAM                                                   
         A     R0,CURLSTL                                                       
         ST    R0,CURLSTL                                                       
         SPACE                                                                  
GCL110   BAS   RE,NEXTEL                                                        
         BE    GCL100                                                           
         DROP  R6                                                               
GCL120   L     RF,CURLSTL                                                       
         S     RF,CURLSTF                                                       
         BAS   RE,ROUND                                                         
         ST    RF,CURLST                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* CHECK HERE FOR CONFIRMED CONTRACT *                                           
         SPACE                                                                  
CKCF     NTR1                                                                   
         LR    R6,R3                                                            
         MVI   ELCODE,X'1F'        CHECK FOR CONFIRMED ELEM                     
         BAS   RE,GETEL                                                         
         BE    CKCF10                                                           
         SPACE                                                                  
         LR    R6,R3                                                            
         MVI   ELCODE,X'1D'        CHECK FOR DARE ELEM                          
         BAS   RE,GETEL                                                         
         BNE   CKCF20                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   =X'1D18',0(R6)      THIS THE DETROYED CONFIRMED ELEM             
         BNE   CKCF20                                                           
         SPACE                                                                  
         MVC   P+5(9),=C'NG1F REP='                                             
         MVC   P+14(2),2(R3)                                                    
         MVC   P+20(2),=C'C='                                                   
         UNPK  P+22(9),23(5,R3)                                                 
         MVI   P+30,C' '                                                        
         GOTO1 =V(PRINTER)                                                      
         SPACE                                                                  
CKCF10   TM    RCONCONF-RCONXEL(R6),X'40' CONFIRMED FLAG                        
         BO    CKCFEQ                      ACCEPT                               
         SPACE                                                                  
         TM    RCONCONF-RCONXEL(R6),X'20' PREV CONFIRMED FLAG                   
         BO    CKCFEQ                      ACCEPT                               
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    CKCF10                                                           
         SPACE                                                                  
CKCF20   LR    R6,R3                                                            
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'ACC-',RCONBUYR-RCONELEM(R6)                                   
*                                  ACCOUNTING CONTRACT?                         
         BE    CKCFEQ               YES                                         
         SPACE                                                                  
         TM    RCONMODR+1-RCONELEM(R6),X'80'    ACE CONTRACT?                   
         BO    CKCFNE                            NOT CONFIRMED                  
         TM    RCONMODR+1-RCONELEM(R6),X'40'    GRAPHNET CONTRACT?              
         BO    CKCFNE                            NOT CONFIRMED                  
         B     CKCFEQ                  - OTHER: CONSIDER 'CONFIRMED'            
CKCFNE   LTR   RB,RB                                                            
         B     EXIT                                                             
         SPACE                                                                  
CKCFEQ   CR    RF,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO GET STATION DETAILS                                                
*                                                                               
GETSTA   NTR1  ,                                                                
         XC    SVGROUP,SVGROUP                                                  
         XC    SVTVB,SVTVB                                                      
         XC    SVOWNER,SVOWNER                                                  
         XC    SVTEAM,SVTEAM                                                    
         MVI   SVSTATY,C'3'                                                     
         MVI   SVRANK,0                                                         
         XC    SVMKT,SVMKT                                                      
         XC    SVAFFIL,SVAFFIL                                                  
         L     R3,=A(SPACEND)                                                   
         SR    R1,R1               FIND STATION IN SPACEND                      
         SR    RE,RE                                                            
*                                                                               
*  IF REPCD = MASTER REP (CROSS-COMPANY), THE ORIGINAL REP CODE,                
*      STORED IN SUBREPCD, SHOULD BE USED FOR THE TABLE SEARCH.  THIS           
*      IS BECAUSE THERE ARE NO STATION RECORDS FOR A MASTER REP, THE            
*      ROUTINE WILL USE THE FIRST STATION FOUND, WHICH MAY NOT BE THE           
*      RIGHT ONE, AS THE STATION MAY BE ON FILE WITH SEVERAL COMPANIES.         
*      R2 POINTS TO THE CORRECT CODE TO USE.                                    
*                                                                               
         LA    R2,REPCD            SET TO USE PRIMARY REP CODE                  
         SPACE                                                                  
         OC    SUBREPCD,SUBREPCD   IS THERE A SUBSIDIARY REP                    
         BZ    GETSTA2                                                          
         SPACE                                                                  
         LA    R2,SUBREPCD         YES - USE ORIGINAL CODE FOR LOOKUP           
*                                                                               
GETSTA2  CLI   0(R3),X'FF'                                                      
         BE    GETSTAX             NOT FOUND                                    
         CLI   0(R3),2                                                          
         BNE   GETSTA4                                                          
         CLC   SVSTA,2(R3)                                                      
         BH    GETSTA4                                                          
         BE    *+14                                                             
         LTR   R3,RE                                                            
         BNZ   GETSTA6                                                          
         B     GETSTAX                                                          
         CLC   0(2,R2),29(R3)      TEST REPS MATCH                              
         BE    GETSTA6             YES-FINE                                     
         CLC   0(2,R2),=C'SJ'      NO-SJR MUST GET SJR RECORDS                  
         BE    GETSTA4                                                          
         CLC   29(2,R3),=C'SJ'     OTHERWISE SKIP SJR RECORDS                   
         BE    GETSTA4                                                          
         LTR   RE,RE               SAVE A(FIRST NON-SJR STATION)                
         BNZ   GETSTA4                                                          
         LR    RE,R3                                                            
*                                                                               
GETSTA4  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETSTA2                                                          
*                                                                               
GETSTA6  EQU   *                                                                
         CLI   TRACEFLG,C'B'       TRACE DISPLAY?                               
         BE    GETSTA6A             YES                                         
         CLI   TRACEFLG,C'S'       TRACE DISPLAY?                               
         BNE   GETSTA7             NO                                           
GETSTA6A EQU   *                                                                
*                                                                               
         CLC   TRACECTR,=F'1000'   DISPLAY FIRST 1000                           
         BH    GETSTA7                                                          
         L     RF,TRACECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TRACECTR                                                      
*                                                                               
         CLC   =C'IR',REPCD        ORIGINAL REP CODE 'IR'?                      
         BNE   GETSTA7             NO  - NO DISPLAY                             
         BAS   RE,STADISP          YES - SHOW WHAT WAS FOUND                    
GETSTA7  EQU   *                                                                
         MVC   SVGROUP,7(R3)       SET GROUP                                    
         MVC   SVSTATY,9(R3)           STATION TYPE                             
         MVC   SVTVB,16(R3)            TVB                                      
         MVC   SVOWNER,18(R3)          OWNER                                    
         MVC   SVRANK,21(R3)           MARKET RANK                              
         MVC   SVMKT,22(R3)            MARKET                                   
         MVC   SVAFFIL,26(R3)          AFFIL                                    
*                                                                               
GETSTAX  XIT1  REGS=(R3)           RETURN R3=A(SPACEND STATION ENTRY)           
         EJECT                                                                  
*                                                                               
*   STADISP:  DISPLAY INCOMING ARGUMENTS, AND RESULT OF SEARCH FOR              
*        'IR' RECORDS ONLY.                                                     
*                                                                               
STADISP  NTR1                                                                   
         MVC   P(2),REPCD                                                       
         MVC   P+3(2),SUBREPCD     ORIGINAL REP CODE                            
         MVC   P+10(31),0(R3)      INSERT TABLE ENTRY FOUND                     
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET STATION TYPE                                                   
*                                                                               
GETSTATY NTR1  ,                                                                
         BAS   RE,GETSTA           GET STATION ENTRY IN SPACEND                 
         CLI   0(R3),X'FF'         TEST NOT FOUND                               
         BE    GETSX               YES-ASSUME OLD                               
         CLI   SVSTATY,0           TEST STATION TYPE SET                        
         BNE   GETSX               YES                                          
         MVC   WORK(4),=X'0000FFFF'   NO-DETERMINE STATION TYPE                 
         OC    10(3,R3),10(R3)     START DATE                                   
         BZ    GETS6                                                            
         GOTO1 =V(DATCON),DMCB,(3,10(R3)),WORK+6                                
         GOTO1 =V(GETBROAD),(R1),WORK+6,WORK+12                                 
         CLC   WORK+6(6),WORK+12                                                
         BE    GETS4              STARTED ON 1ST DAY OF BROADCAST MONTH         
         MVC   WORK+12(6),WORK+18  ELSE GO TO NEXT MONTH                        
         GOTO1 =V(ADDAY),DMCB,WORK+12,WORK+18,15                                
*                                                                               
GETS4    GOTO1 =V(DATCON),DMCB,WORK+18,(3,WORK)                                 
         MVI   WORK+2,X'FF'        WORK(2) HAS FIRST MONTH                      
*                                                                               
GETS6    OC    13(3,R3),13(R3)     END DATE                                     
         BZ    GETS10                                                           
         GOTO1 =V(DATCON),DMCB,(3,13(R3)),WORK+6                                
         GOTO1 =V(GETBROAD),DMCB,WORK+6,WORK+12                                 
         CLC   WORK+6(6),WORK+18                                                
         BE    GETS8               ENDED ON LAST DAY OF BROADCAST MONTH         
         L     R5,=F'-15'           ELSE GO TO PREVIOUS MONTH                   
         GOTO1 =V(ADDAY),DMCB,WORK+12,WORK+18,(R5)                              
*                                                                               
GETS8    GOTO1 =V(DATCON),DMCB,WORK+18,(3,WORK+2)    WORK+2(2) HAS              
*                                                    LAST MONTH                 
GETS10   MVI   9(R3),C'3'          OLD                                          
         CLC   WORK+2(2),RRGEND    LOST BEFORE LAST REQUEST MONTH               
         BL    GETS12                                                           
         MVI   9(R3),C'2'          NEW                                          
         CLC   WORK(2),RRGSTART    STARTED AFTER FIRST PRIOR MONTH              
         BH    GETS12                                                           
         MVI   9(R3),C'1'          ELSE COMPARABLE                              
*                                                                               
GETS12   MVC   SVSTATY,9(R3)                                                    
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO GET TEAM CODE                                                      
*                                                                               
         SPACE 1                                                                
GETTEAM  NTR1  ,                                                                
         XC    SVTEAM,SVTEAM                                                    
         MVC   DMFILE,REPDIR                                                    
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPCD                                                   
         MVC   RSTAKSTA,SVSTA                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'RSTAKEY),RSTAKEY                                           
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   GETTX                                                            
         BAS   RE,GETSTAT                                                       
         LA    R1,RSTAELEM                                                      
         SR    R0,R0                                                            
*                                                                               
GETT2    CLI   0(R1),0                                                          
         BE    GETTX                                                            
         CLI   0(R1),4                                                          
         BNE   GETT4                                                            
         CLC   SVOFF,RSTAOTOF-RSTAOTEL(R1)                                      
         BNE   GETT4                                                            
         MVC   SVTEAM,RSTAOTTM-RSTAOTEL(R1)                                     
         B     GETTX                                                            
*                                                                               
GETT4    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETT2                                                            
*                                                                               
GETTX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        ROUTINE TO MOVE A SORTED RECOVERY RECORD INTO A RECORD AREA            
*        TEMPREC CONTAINS RECOVERY RECORD                                       
*        R0    CONTAINS ADDRESS OF DESTINATION                                  
*                                                                               
         SPACE                                                                  
MOVE     ST    RE,SAVERE                                                        
         L     RE,=A(TEMPREC)                                                   
         LH    RF,0(RE)            GET 'FROM' REC LEN                           
         LA    R1,2(RF)            SET 'TO' LEN = 'FROM' LEN + 2                
         MVCL  R0,RE               ** WHICH PUTS IN 2 X'00' AT EOR              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        GETMDAY ROUTINE RETURNS MONDAY DATE FOR WEEK GIVEN                     
*                INPUT = YYMMDD                                                 
*                OUTPUT= 2-BYTE BINARY                                          
         SPACE                                                                  
GETMDAY  NTR1                                                                   
         LM    R5,R6,0(R1)         R5=A(INPUT), R6=A(OUTPUT)                    
         CLC   0(6,R5),=C'870229'    ***IF LEAP YEAR 88***                      
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'910229'    ***IF LEAP YEAR 92***                      
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'950229'    ***IF LEAP YEAR 96***                      
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'990229'    ***IF LEAP YEAR 2000***                    
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'030229'    ***IF LEAP YEAR 2004***                    
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'070229'    ***IF LEAP YEAR 2008***                    
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'110229'    ***IF LEAP YEAR 2012***                    
         BE    GETM10                        OR                                 
         CLC   0(6,R5),=C'150229'    ***IF LEAP YEAR 2016***                    
         BNE   *+8                          THEN                                
GETM10   MVI   5(R5),C'8'       ***MAKE PRIOR YEAR THE 28TH***                  
         GOTO1 =V(GETDAY),DMCB,(R5),FULL                                        
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB             DAY NUMBER                                   
         BCTR  RE,R0                                                            
         LNR   RE,RE                                                            
         ST    RE,DMCB+8           PUT IN NUMBER OF DAYS TO SUBTRACT            
         CLI   SETUPDPC,C'Y'       DAILY PACING REP?                            
         BNE   GETM20              NO                                           
         XC    DMCB+8(4),DMCB+8    YES - CLEAR ADJUSTMENT VALUE                 
*                                                                               
GETM20   EQU   *                                                                
         GOTO1 =V(ADDAY),DMCB,(R5),DUB                                          
*                                                                               
         GOTO1 =V(DATCON),(R1),DUB,(2,(R6))                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
POSTMON  NTR1                                                                   
*                                                                               
*        ROUTINE TO POST A MONTH TABLE                                          
*        R3 = A(CONTRACT RECORD)                                                
*        R4 = A(MONTH TABLE)                                                    
*                                                                               
         LA    R5,24                                                            
         LR    RF,R4                                                            
*                                                                               
PM010    XC    8(32,RF),8(RF)                                                   
         LA    RF,40(RF)                                                        
         BCT   R5,PM010                                                         
*                                                                               
         CLI   NOPOSTMN,C'Y'                                                    
         BE    EXIT                                                             
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+16,C'P'        REQUEST RESULT IN PENNIES                    
*                                                                               
* NOTE - SOURCE BOOK FOR VALUEMON IS REREPVALM2                                 
*                                                                               
* R3 = FIRST REC, R4 = MONTAB1                                                  
* R3 = LAST REC,  R4 = MONTAB2                                                  
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0020                                                         
**       MVC   P+1(08),=C'IN VALUE'                                             
**       GOTO1 =V(PRINTER)                                                      
*                                                                               
*EST0020 EQU   *                                                                
*                                                                               
***      L     RF,0(R3)            SET A(KEY)                                   
***      ZICM  RE,27(RF),2         SET A(RECORD LENGTH)                         
***      AR    RF,RE               BUMP TO END OF RECORD                        
***      XC    0(2,RF),0(RF)       CLEAR TWO BYTES AT REC END                   
*                                                                               
*                                                                               
*   TEST                                                                        
***      MVC   P+01(14),=C'VALUEMON CALL:'                                      
***      MVC   P+18(64),0(R3)                                                   
***      GOTO1 =V(PRINTER)                                                      
***      MVC   P+18(64),0(R4)                                                   
***      GOTO1 =V(PRINTER)                                                      
***      MVC   P+18(32),MONINF                                                  
***      MVC   P+54(32),MONFORC                                                 
***      GOTO1 =V(PRINTER)                                                      
***      GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(VALUEMON),DMCB,(R3),(R4),MONINF,MONFORC                       
*                                                                               
*   TEST                                                                        
***      CLI   TESTFLAG,C'Y'                                                    
***      BNE   TEST0030                                                         
**       MVC   P+1(09),=C'OUT VALUE'                                            
**       GOTO1 =V(PRINTER)                                                      
*                                                                               
TEST0030 EQU   *                                                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE                                                 
*                                                                               
         SPACE                                                                  
HIGH     LA    RF,=C'DMRDHI'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
*                                                                               
SEQ      LA    RF,=C'DMRSEQ'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
*                                                                               
WRITE    LA    RF,=C'DMWRT '                                                    
         B     LINKDIR                                                          
*                                                                               
ADD      LA    RF,=C'DMADD '                                                    
         B     LINKDIR                                                          
*                                                                               
LINKDIR  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         LA    R3,KEY                                                           
         LA    R4,KEY                                                           
         CLC   0(6,RF),=C'DMWRT '                                               
         BNE   LD010                                                            
         LA    R3,RRGREC                                                        
         LA    R4,RRGREC                                                        
         B     LD012                                                            
*                                                                               
LD010    CLC   0(6,RF),=C'DMADD '                                               
         BNE   LD020                                                            
         LA    R3,SORT2REC                                                      
         LA    R4,SORT2REC                                                      
*                                                                               
LD012    CLI   TRACEFLG,C'B'                                                    
         BE    LD013                                                            
         CLI   TRACEFLG,C'Y'                                                    
         BNE   LD014                                                            
LD013    EQU   *                                                                
*                                                                               
         CLC   TRACECTR,=F'1000'   DISPLAY FIRST 1000                           
         BH    LD014                                                            
         L     RF,TRACECTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TRACECTR                                                      
*                                                                               
*                                                                               
*        CLC   0(6,RF),=C'DMADD '  TEMPX                                        
*        BE    *+12                                                             
*        CLI   2(R3),C'D'          TEMPX                                        
*        BNE   LD014               TEMPX                                        
*                                                                               
         MVC   P(6),0(RF)                                                       
         MVC   P+6(116),0(R3)                                                   
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARA,(R3),WORK,116,=C'SEP'                            
         MVC   P,SPACES                                                         
         MVC   P+6(116),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+6(116),WORK+116                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
LD014    CLI   WRITEFLG,C'N'                                                    
         BNE   LD030                                                            
         MVC   KEY,0(R4)                                                        
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
LD020    CLC   DMFILE,RRGON                                                     
         BNE   LD030                                                            
         LA    R4,RRGREC                                                        
*                                                                               
LD030    GOTO1 =V(DATAMGR),DMCB,,DMFILE,(R3),(R4),0                             
         CLC   DMFILE,RRGON                                                     
         BNE   DMCHECK                                                          
         MVC   KEY,0(R4)                                                        
         B     DMCHECK                                                          
*                                                                               
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETSTAT  LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETCAT   LA    RF,RCTGREC                                                       
         B     LINKFILE                                                         
*                                                                               
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         GOTO1 =V(DATAMGR),DMCB,(DMINBTS,=C'GETREC'),REPFIL,           X        
               KEY+28,(R2),(0,DMWORK)                                           
         B     DMCHECK                                                          
*                                                                               
DMCHECK  CLI   DMCB+8,X'80'                                                     
         BNE   DC010                                                            
         CLC   DMFILE,RRGON                                                     
         BNE   DC010                                                            
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
DC010    TM    DMCB+8,X'FD'                                                     
         BZ    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
ROUND    DS    0H                                                               
*                                                                               
*        ROUTINE TO ROUND PENNIES TO DOLLARS                                    
*        RF CONTAINS PENNIES ON ENTRY, DOLLARS ON EXIT                          
*                                                                               
         LTR   RF,RF                                                            
         BZR   RE                                                               
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* READ STATION RECORDS AND PUT STATION ATTRIBUTES INTO SPACEND                  
*                                                                               
READSTA  NTR1  ,                                                                
         MVC   DMFILE,REPDIR                                                    
         L     R4,=A(SPACEND)                                                   
         XC    KEY,KEY             GET STATION DETAILS IN SPACEND               
         MVI   KEY,X'82'                                                        
         BAS   RE,HIGH                                                          
*                                                                               
RDSTA2   EQU   *                                                                
         CLC   =C'AQ',KEY+RST2KREP-RST2KEY  ALLIED STATION?                     
         BE    RDSTA8              YES - SKIP IT                                
         CLC   KEY(1),KEYSAVE                                                   
         BNE   RDSTA10                                                          
         BAS   RE,GETSTAT                                                       
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+01(15),=C'STATION RECORD:'                                     
         MVC   P+18(27),KEY                                                     
         MVC   P+52(27),RSTAREC                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST                                                                        
*&&                                                                             
         MVC   0(2,R4),=X'021F'                                                 
         MVC   2(5,R4),RSTAKSTA                                                 
         MVC   7(2,R4),RSTAGRUP                                                 
         MVI   9(R4),0             INIT STATION TYPE                            
         MVC   10(3,R4),RSTASTRT                                                
         MVC   13(3,R4),RSTAEND                                                 
         MVC   16(2,R4),RSTATVB                                                 
         MVC   18(3,R4),RSTAOWN                                                 
         MVC   21(1,R4),RSTARANK                                                
         XC    22(4,R4),22(R4)     INIT MARKET                                  
         MVC   26(3,R4),RSTAAFFL                                                
         MVC   29(2,R4),RSTAKREP   REP                                          
         LA    RE,RSTAELEM                                                      
         SR    R0,R0                                                            
*                                                                               
RDSTA4   CLI   0(RE),0                                                          
         BE    RDSTA6                                                           
         CLI   0(RE),8                                                          
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     RDSTA4                                                           
         MVC   22(4,R4),RSTAMKTC-RSTAXXEL(RE)                                   
*                                                                               
RDSTA6   LA    R4,31(R4)                                                        
         C     R4,=A(SPACENDX)                                                  
         BL    RDSTA8                                                           
         DC    H'0'                NEED MORE SPACE                              
*                                                                               
RDSTA8   BAS   RE,SEQ                                                           
         B     RDSTA2                                                           
*                                                                               
RDSTA10  MVI   0(R4),X'FF'         MARK END OF SPACEND                          
         ST    R4,SPACENXT                                                      
*                                                                               
RDSTAX   B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VREGSAVE DC    V(REGSAVE)                                                       
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DC    C'**DMCB**'                                                      
DMCB     DS    6F                                                               
         DC    C'**PARA**'                                                      
PARA     DS    6F                                                               
AREC     DC    A(REC)                                                           
ASORTREC DS    A                                                                
SAVERE   DS    A                                                                
ACTBUD   DS    A                                                                
SPACENXT DS    A                                                                
SPACEREP DS    A                                                                
         DC    C'***KEY**'                                                      
KEY      DS    XL(L'ROKEY)                                                      
KEYSAVE  DS    XL(L'KEY)                                                        
WORK     DS    CL256                                                            
DMWORK   DS    XL96                                                             
DMFILE   DS    XL8                                                              
*                                                                               
WRITEFLG DS    CL1                 Y = WRITE OKAY (DEFAULT)                     
*                                  N = BYPASS WRITES                            
*                                                                               
TMPPRTSW DS    XL1                                                              
*                                                                               
TRACEFLG DS    CL1                 Y = HEX PRINT UPDATE RRG RECS                
*                                  S = GETSTA DISPLAY                           
*                                  B = BOTH                                     
*                                  T = TRACE SORTED RECORDS                     
TRACECTR DS    F                                                                
CONTYSW  DS    CL1                                                              
*                                                                               
DIRECTYP DS    CL1                 DIRECT TYPE FOR THIS REP                     
DIRCTFLG DS    CL1                 DIRECT CONTRACT Y OR N (1ST CON)             
CONFLAG  DS    CL1                                                              
QWDRCOFF DS    XL2                 YM OF AS-AT DATE FOR DIRECT                  
QWDRCOFP DS    XL2                 DIRECT $ CUTOFF YR/MO                        
TESTFLAG DS    XL1                 FOR DEBUGGING DISPLAYS                       
*                                                                               
         DS    0D                                                               
UTL      DC    F'0',X'0A'          INITIALIZE TO CONTROL SYSTEM                 
         DS    0D                                                               
SSB      DC    X'0000FF02'         INHIBIT RECOVERY                             
*SB      DC    F'2'                INHIBIT RECOVERY                             
         DC    XL252'00'                                                        
         SPACE                                                                  
FLIST    DS    0H                                                               
         DC    CL8'UREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'URRGNEW '                                                    
         DC    CL8'X       '                                                    
DMINBTS  DC    X'08'               (PASS BACK DELETED RECORDS)                  
REPDIR   DC    CL8'REPDIR  '                                                    
REPFIL   DC    CL8'REPFIL  '                                                    
RRGON    DC    CL8'RRGNEW  '                                                    
NOPOSTMN DS    C                                                                
LASTSW   DS    C                                                                
BUDYEAR  DS    C                                                                
ELCODE   DS    X                                                                
YRSTART  DS    CL2                 START YEAR-MONTH IS ASSUMED TO BE 1          
YREND    DS    CL2                                                              
RRGSTART DS    XL2                                                              
RRGEND   DS    XL2                                                              
REPCD    DC    CL2' '                                                           
SUBREPCD DC    CL2' '                                                           
MASTREP  DS    CL2                                                              
REPSW    DS    CL1                                                              
SVGROUP  DS    CL2                                                              
SVSTA    DS    CL5                                                              
SVSTATY  DS    CL1                                                              
SVTVB    DS    CL2                                                              
SVOWNER  DS    CL3                                                              
SVTEAM   DS    CL2                                                              
SVRANK   DS    CL1                                                              
SVMKT    DS    CL4                                                              
SVOFF    DS    CL2                                                              
SVADV    DS    CL4                                                              
SVAGY    DS    CL4                                                              
SVAFFIL  DS    CL3                                                              
SVCTGY   DS    CL2                                                              
SVCLASS  DS    CL2                                                              
SVID     DS    CL10                                                             
THISTYPE DS    C                                                                
TYPECT   DS    X                                                                
TYPEPTR  DS    A                                                                
TYPETAB  DS    CL4                 POS 1 A = ALL                                
*                                  POS 2 C = CONFIRMED ONLY                     
*                                  POS 3 U = UNCONFIRMED ONLY                   
*                                  POS 4 D = DIRECT ONLY (IMPLIED CONF)         
*                                                                               
REPDAILY DS    CL240               PROVIDE FOR 80 ENTRIES                       
*                                  POS 1-2  =  REP CODE                         
*                                  POS 3    =  DAILY PACING FLAG                
         DC    X'FFFF'             TABLE DELIMITER                              
LREPSLOT EQU   3                   LENGTH OF TABLE ENTRY                        
REPMASTR DS    CL2                 MASTER REP CODE                              
REPFOUND DS    CL1                 Y = REP ON RRGON                             
*                                  N = REP NOT ON RRGON                         
SAVEACON DS    A                                                                
SETUPREP DS    CL2                                                              
SETUPDPC DS    CL1                                                              
DAILYFLG DS    CL1                                                              
THISREP  DS    A                                                                
NEXTREP  DS    A                                                                
DATORIDE DS    XL3                 DATE OVERRIDE                                
THISMON  DS    XL2                 DATE TO BE PROCESSED                         
THISADJ  DS    XL2                 REGULAR ADJUSTED MONDAY DATE                 
THISDAY  DS    XL2                 DAILY PACING DAY DATE                        
THISWKF  DS    F                   DOLLARS THIS WK FIRST REC CURR YR            
THISWKL  DS    F                      "      "  "  LAST REC                     
         SPACE                                                                  
LASTWKF  DS    F                   DOLLARS THIS WK FIRST REC LAST YR            
LASTWKL  DS    F                      "      "  "  LAST REC                     
         SPACE                                                                  
CURLSTF  DS    F                   DOLLARS LAST YR, BUT CUR AS AT DATE          
CURLSTL  DS    F                                                                
         SPACE                                                                  
CHACNT   DS    PL4                                                              
ADDCNT   DS    PL4                                                              
TOTCHA   DS    PL4                                                              
TOTADD   DS    PL4                                                              
TOTNUL   DS    PL4                 RECORDS WITH NULL FIELDS BYPASSED            
MONINF   DS    XL192                                                            
MONFORC  DS    XL4                                                              
*                                                                               
         DS    0F                                                               
COLVALS  DS    24XL(COLVALX-COLVALD)                                            
*                                                                               
*                                                                               
REPTAB   DC    (MAXREPS)XL16'00'   +00(2) REP CODE                              
         DC    X'FF'               +02(2) START MONTH                           
*                                  +04(2) END MONTH                             
*                                  +06(2) MASTER REP                            
*                                  +08(1) A = ALL DOLLARS                       
*                                  +09(1) C = CONFIRMED                         
*                                  +10(1) U = UNCONFIRMED                       
*                                  +11(1) D = DIRECT DOLLARS                    
*                                  +15(1) DIRECT CODE                           
*                                                                               
MAXREPS  EQU   20                  MAXIMUM N'REPS                               
MAXTYPS  EQU   360                 MAXIMUM N'RECORD TYPES                       
         EJECT                                                                  
SORT2REC DS    CL(L'ROREC)                                                      
*                                                                               
RRGREC   DS    CL(L'ROREC)                                                      
*                                                                               
         EJECT                                                                  
* REGENREP                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* REGENOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* REGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* REGENCTG                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENCTG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RSORTKEY DS    0XL18               ************************************         
*                                  * SPECIAL NOTE:                    *         
RSORTREP DS    CL2                 * IF YOU CHANGE LENGTH OF RSORTKEY,*         
RSORTSUB DS    CL2                 * DO NOT FORGET TO CHANGE SORTCARD,*         
RSORTTYP DS    X                   * RECCARD, AND THE LRECL AND       *         
RSORTBUD DS    XL9                 * BLKSIZE IN THE TEMP DCB          *         
         ORG   RSORTBUD            ************************************         
RSORTCON DS    XL4                                                              
         ORG                                                                    
RSORTSEQ DS    XL4                                                              
*                                                                               
RSPARE   DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 2                                                                
RKEY     DS    0XL27               IOAREA FOR RECOVERY RECORD                   
         DS    4048X                                                            
*                                                                               
RECORDL  EQU   *-RLEN              TOTAL RECORD LENGTH                          
*                                  960 - 24 MONTHS, 40 BYTE ENTRIES             
         DS    D                                                                
MONTAB1  DS    960X                24 X 40 (10 COUNTERS)                        
         DS   2D                                                                
MONTAB2  DS    960X                                                             
         DS    D                                                                
         SPACE 2                                                                
         DS    0D                                                               
CTBUDTAB DS    (MAXCTBUD)XL50      CONTRACT TYPE BUDGETS TABLE                  
*                                  +0(1)=CONTRACT TYPE                          
*                                  +1(1)=INDICATOR                              
*                                  +2(48)=BUDGETS                               
MAXCTBUD EQU   16                  MAX N'CONTRACT TYPES PER BUDGET REC          
         SPACE 2                                                                
         DS    0D                                                               
LASTREC  DS    (RECORDL)X          LAST RECORD AREA                             
         SPACE 3                                                                
         DS    0D                                                               
TEMPREC  DS    (RECORDL)X          TEMP RECORD AREA                             
         SPACE 3                                                                
CARD     DS    CL80                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4048'                                  
*                                                                               
SORTCD2  DC    CL80'SORT FIELDS=(1,40,A),FORMAT=BI,WORK=1'                      
RECCD2   DC    CL80'RECORD TYPE=F,LENGTH=116'                                   
*                                                                               
* 1 - TYPE(ALL/CONF/DIR) 2-4 KEY TYPES 5-6 START YR/MO 7-8 END YR/MO            
*                                                                               
RECTAB   DC    (MAXTYPS)XL8'00'                                                 
         DC    X'FF'                                                            
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        +        
               MACRF=GM,EODAD=IN100                                             
*                                                                               
TEMP     DCB   DDNAME=RRGTEMP,DSORG=PS,RECFM=VB,                       +        
               BLKSIZE=27404,MACRF=(GM,PM),EODAD=UT030                          
REC      DS    4000C               IOAREA                                       
*                                                                               
SPACEND  DS    800000X             800K FOR SPACEND                             
SPACENDX EQU   *                                                                
         EJECT                                                                  
LRECD    DSECT                                                                  
LLEN     DS    H                                                                
         DS    H                                                                
LSORTKEY DS    CL(L'RSORTKEY)                                                   
LSPARE   DS    CL4                                                              
*                                                                               
LRECVHDR DS    CL24                                                             
*                                                                               
LKEY     DS    0CL34                                                            
         EJECT                                                                  
COLVALD  DSECT                                                                  
COLYRMN  DS    H                                                                
COLPYRM  DS    H                                                                
CUREST   DS    F                   CURRENT ESTIMATED                            
PRIEST   DS    F                   PRIOR EST                                    
PRIFIN   DS    F                   PRIOR FINAL                                  
CURWK    DS    F                                                                
LSTWK    DS    F                                                                
CURLST   DS    F                                                                
COLVALX  EQU   *                                                                
         SPACE 3                                                                
* VALUEMON TABLE                                                                
         SPACE                                                                  
       ++INCLUDE REREPVALD                                                      
         SPACE 3                                                                
* RECORD TYPES AND DATA TYPES ON RRGON FILE                                     
         SPACE                                                                  
RPTD     DSECT                                                                  
RPTYPE   DS    C                                                                
RPTYP    DS    XL3                                                              
RPSTYR   DS    X                                                                
RPSTMN   DS    X                                                                
RPENYR   DS    X                                                                
RPENMN   DS    X                                                                
RPTX     EQU   *                                                                
         EJECT                                                                  
RORECD   DSECT                                                                  
       ++INCLUDE REGENRRGOD                                                     
         EJECT                                                                  
RCOND    DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RBUDD    DSECT                                                                  
       ++INCLUDE REGENBUD                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
CSECT2   CSECT                                                                  
PRINTREP NTR1  BASE=*,LABEL=*                                                   
         MVC   P(16),=C'**** TOTALS ****'                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'RRGON RECORDS'                                          
         MVC   P+14(7),=C'CHGED ='                                              
         OI    TOTCHA+3,X'0F'                                                   
         UNPK  P+24(7),TOTCHA                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'RRGON RECORDS'                                          
         MVC   P+14(7),=C'ADDED ='                                              
         OI    TOTADD+3,X'0F'                                                   
         UNPK  P+24(7),TOTADD                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'RRGON RECORDS'                                          
         MVC   P+14(7),=C'NULLS ='                                              
         OI    TOTNUL+3,X'0F'                                                   
         UNPK  P+24(7),TOTNUL                                                   
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* CHECK IF KATZ RADIO, IF SO, CK BACK BILLING                                   
         SPACE                                                                  
CHKBB    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RKEY             CONTRACT REC                                 
         USING RCOND,R2                                                         
         LA    R0,VKATZLN                                                       
         LA    R1,VKATZBB                                                       
*                                                                               
*   ALL ORDERS WILL NOW BE CHECKED FOR BACKBILLING.  IF ORDER IS BB,            
*        IT WILL BE SKIPPED.  NO REP CHECKING WILL BE DONE.                     
*        UNCONDITIONAL BRANCH BYPASSES REP LOOKUP.                              
*                                                                               
         B     CHKBB40             UNCONDITIONAL BRANCH                         
*                                                                               
CHKBB20  CLC   RCONKREP,0(R1)      CHECK THE REP CODE                           
         BE    CHKBB40                                                          
         LA    R1,2(,R1)                                                        
         BCT   R0,CHKBB20                                                       
CHKBB30  CR    RB,RB               SET TO EQ CC                                 
         XIT1                                                                   
         SPACE                                                                  
CHKBB40  DS    0H                                                               
         LA    R6,RKEY                                                          
         MVI   ELCODE,X'01'        CHECK FOR DIRECT                             
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'ACC-BB',RCONBUYR BACK BILLING?                                
         BNE   CHKBB30              NO                                          
         LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R6                                                            
         SPACE                                                                  
VKATZBB  DC    C'BFCREAKFKUK3K4K6RSS3'                                          
VKATZLN  EQU   (*-VKATZBB)/2                                                    
         LTORG                                                                  
         EJECT                                                                  
CARDPROC NTR1  BASE=*,LABEL=*                                                   
         L     R6,=A(CARD)                                                      
*                                                                               
RG02     GOTO1 =V(CARDS),DMCB,(R6),=C'RE00'                                     
         CLC   =C'/*',0(R6)                                                     
         BE    RG10                                                             
         CLC   =C'WRITE=N',0(R6)                                                
         BNE   RG04                                                             
         MVI   WRITEFLG,C'N'                                                    
         MVI   FLIST,C' '                                                       
         MVI   FLIST+16,C' '                                                    
         B     RG08                                                             
*                                                                               
RG04     CLC   =C'DDSIO=',0(R6)                                                 
         BNE   RG040020                                                         
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),6(R6)                                                    
         B     RG08                                                             
*                                                                               
RG040020 CLC   =C'DATE=',0(R6)                                                  
         BNE   RG05                                                             
         GOTO1 =V(DATVAL),DMCB,(0,5(R6)),WORK                                   
         OC    DMCB,DMCB           ERROR?                                       
         BZ    BADDATE             YES                                          
         GOTO1 =V(DATCON),DMCB,(0,WORK),(3,DATORIDE)                            
         B     RG08                                                             
*                                                                               
RG05     CLC   =C'TRACE=',0(R6)                                                 
         BNE   RG06                                                             
         MVC   TRACEFLG,6(R6)      SAVE TRACE FLAG CHARACTER                    
*                                  Y  =  ORIGINAL TRACE                         
*                                  S  =  GETSTA DISPLAY                         
*                                  B  =  BOTH                                   
*                                  T  =  TRACE SORTED RECS                      
*                                  D  =  TRACE BUDGET RECS INTO SORTER          
         B     RG08                                                             
*                                                                               
RG06     CLC   =C'UPDID=',0(R6)                                                 
         BNE   RG07                                                             
         GOTO1 =V(DATAMGR),DMCB,=C'UPDID'                                       
         L     R1,12(R1)                                                        
         MVC   0(2,R1),6(R6)                                                    
*                                                                               
*   TEST UPDID                                                                  
         MVC   P+1(15),=C'UPDID FROM DMGR'                                      
         MVC   P+20(2),6(R6)                                                    
         GOTO1 =V(PRINTER)                                                      
         B     RG08                                                             
*                                                                               
RG07     CLC   =C'ID=',0(R6)                                                    
         BNE   INCANCEL                                                         
         MVC   SVID,3(R6)                                                       
*                                                                               
RG08     MVC   P(80),0(R6)                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     RG02                                                             
*                                                                               
BADDATE  MVC   P(19),=C'***DATE INVALID ***'                                    
         B     INCAN2                                                           
INCANCEL MVC   P(23),=C'*** UNKNOWN PARM CD ***'                                
INCAN2   GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),0(R6)                                                      
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
RG10     DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
OFFPRNT1 NTR1  LABEL=*,BASE=*                                                   
         USING RCOND,R3                                                         
         MVC   P+1(33),=C'CONTRACT WENT FROM CONF TO UNCONF'                    
         GOTO1 =V(PRINTER)                                                      
         SPACE                                                                  
         MVC   P+1(4),=C'REP='                                                  
         MVC   P+5(2),RCONKREP                                                  
         MVC   P+9(3),=C'GS='                                                   
         MVC   P+12(2),RCONKGRP                                                 
         MVC   P+16(4),=C'STA='                                                 
         MVC   P+20(5),RCONKSTA                                                 
         MVC   P+27(4),=C'OFF='                                                 
         MVC   P+31(2),RCONKOFF                                                 
         MVC   P+36(6),=C'AGYOF='                                               
         MVC   P+42(4),RCONKAGY                                                 
         MVC   P+47(2),RCONKAOF                                                 
         MVC   P+51(4),=C'ADV='                                                 
         MVC   P+55(4),RCONKADV                                                 
         XC    DMCB(24),DMCB                                                    
         MVC   P+62(9),=C'CONTRACT='                                            
         GOTO1 =V(HEXOUT),DMCB,RCONKCON-RCOND+RKEY,P+71,4                       
         GOTO1 =V(PRINTER)                                                      
         DROP  R3                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETSORT  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
GETSORT2 GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         ST    RF,ASORTREC                                                      
         LTR   RF,RF                                                            
         BZ    GETSORTX                                                         
         LA    R7,SORT2REC                                                      
         USING RORECD,R7                                                        
         CLC   ROKEY,0(RF)                                                      
         BNE   GETSORTX                                                         
         SPACE                                                                  
         LA    R0,ROCTRS                                                        
         LA    R0,19                                                            
         LA    RE,RODPER                                                        
         LA    RF,RODPER-ROREC(RF)                                              
         L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,*-20                                                          
         B     GETSORT2                                                         
*                                                                               
GETSORTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* GET THIS WEEK'S BOOKING BY COMPARING FIRST REC TO LAST REC *                  
* IF DOING CONFIRMED OR DIRECT, WILL ONLY COME HERE IF LAST  *                  
* REC IS CONFIRMED, HOWEVER, FIRST REC MAY NOT BE CONFIRMED  *                  
         SPACE 2                                                                
         USING COLVALD,R3                                                       
GETWK    NTR1  LABEL=*,BASE=*                                                   
         MVC   THISMON,THISADJ     SET ADJUSTED MONDAY DATE                     
         L     R2,THISREP          SET A(REP CODE/DAILY FLAG TABLE)             
GETW0010 EQU   *                                                                
         CLI   0(R2),X'FF'         DELIMITER REACHED?                           
         BE    GETW0020            YES - NOT IN TABLE????                       
*                                                                               
         CLC   RCONKREP-RCOND+RKEY(2),0(R2)                                     
*                                  CONTRACT REP FOUND IN TABLE?                 
         BE    GETW0015            YES - COMPARE FLAG                           
         LA    R2,LREPSLOT(R2)     NO  - BUMP TO NEXT SLOT                      
         B     GETW0010            GO BACK FOR NEXT                             
GETW0015 EQU   *                                                                
         CLC   2(1,R2),=C'Y'       DAILY PACING REP?                            
         BNE   GETW0020            NO                                           
****     MVC   THISMON,THISDAY     SET TODAY'S DATE                             
*                                                                               
*   LEAVE THISMON AS MONDAY'S DATE.  THIS WILL BE A RANGE TEST,                 
*        FROM THISMON TO THISDAY, INCLUSIVE.  IF IN THIS RANGE,                 
*        FIGURES WILL BE PROCESSED.                                             
*                                                                               
GETW0020 EQU   *                                                                
*                                                                               
*   TEST PRINT                                                                  
*        CLC   =C'J0',RCONKREP-RCOND+RKEY                                       
*        BNE   TEST0020                                                         
*        MVC   P+1(06),=C'GETWK:'                                               
*        MVC   P+8(03),0(R2)       INSERT REP+FLAG FROM TABLE                   
*        GOTO1 =V(DATCON),DMCB,(2,THISMON),(5,P+12)                             
*        MVC   P+24(02),RCONKREP-RCOND+RKEY                                     
*        GOTO1 =V(PRINTER)                                                      
*EST0020 EQU   *                                                                
*   TEST PRINT END                                                              
*                                                                               
         XC    THISWKF,THISWKF                                                  
         XC    THISWKL,THISWKL                                                  
         SPACE                                                                  
         XC    LASTWKF,LASTWKF                                                  
         XC    LASTWKL,LASTWKL                                                  
         SPACE                                                                  
         MVI   ELCODE,X'03'                                                     
         SPACE                                                                  
         CLI   RRECTY,3            IF FIRST RECORD IS ADD                       
         BE    GW030                IGNORE FIRST REC                            
         SPACE                                                                  
         CLI   THISTYPE,C'C'       DOING CONFIRMED                              
         BE    GW000                YES                                         
         SPACE                                                                  
         CLI   THISTYPE,C'D'       DOING DIRECT (CONFIRMED IMPLIED)             
         BNE   GW006                NO                                          
         SPACE                                                                  
*  IF BLAIR, FOX, OR PETNY DIRECT, NO CONFIRMED CK                              
         SPACE                                                                  
         CLC   =C'BL',RCONKREP-RCOND+RKEY                                       
         BE    GW006                YES                                         
         CLC   =C'FN',RCONKREP-RCOND+RKEY                                       
         BE    GW006                YES                                         
         CLC   =C'PV',RCONKREP-RCOND+RKEY                                       
         BE    GW006                YES                                         
         SPACE                                                                  
GW000    CLI   CONFLAG,C'N'        NOT CONFIRMED                                
         BE    GW030                IGNORE FIRST REC                            
         SPACE                                                                  
GW006    LA    R6,RKEY                                                          
         BAS   RE,GETEL                                                         
         BNE   GW030                                                            
         USING RCONBKEL,R6                                                      
GW010    CLC   COLYRMN,RCONBKYR    THIS THIS YR/MON                             
         BNE   GW014                                                            
         CLC   RCONBKWK,THISMON    THIS THIS WEEK: MONDAY                       
         BL    GW014               PRE-MONDAY:  EXIT                            
         CLC   RCONBKWK,THISDAY    THIS THIS WEEK: TODAY                        
         BH    GW014               AFTER TODAY: EXIT                            
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,THISWKF                                                       
         ST    R0,THISWKF                                                       
         SPACE                                                                  
GW014    CLC   COLPYRM,RCONBKYR    THIS LAST YR/MON                             
         BNE   GW020                                                            
         CLC   RCONBKWK,THISMON    THIS LAST WEEK: MONDAY                       
         BL    GW020               PRE-MONDAY - EXIT                            
         CLC   RCONBKWK,THISDAY    THIS LAST WEEK: TODAY                        
         BH    GW020               AFTER TODAY - EXIT                           
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,LASTWKF                                                       
         ST    R0,LASTWKF                                                       
         SPACE                                                                  
GW020    BAS   RE,NEXTEL                                                        
         BE    GW010                                                            
         DROP  R6                                                               
GW030    L     R6,=A(LASTREC)                                                   
         LA    R6,LKEY-LRECD(R6)                                                
         TM    29(R6),X'80'        IF LAST RECORD IS DELETE                     
         BO    GW060                LAST BUCKETS ARE ZERO                       
         SPACE                                                                  
         BAS   RE,GETEL                                                         
         BNE   GW060                                                            
         USING RCONBKEL,R6                                                      
GW040    CLC   COLYRMN,RCONBKYR    THIS THIS YR/MON                             
         BNE   GW044                                                            
         CLC   RCONBKWK,THISMON    THIS THIS WEEK                               
         BL    GW044               PRE-MONDAY - EXIT                            
         CLC   RCONBKWK,THISDAY    THIS THIS WEEK                               
         BH    GW044               AFTER TODAY - EXIT                           
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,THISWKL                                                       
         ST    R0,THISWKL                                                       
         SPACE                                                                  
GW044    CLC   COLPYRM,RCONBKYR    THIS LAST YR/MON                             
         BNE   GW050                                                            
         CLC   RCONBKWK,THISMON    THIS THIS WEEK                               
         BL    GW050               PRE-MONDAY: EXIT                             
         CLC   RCONBKWK,THISDAY    THIS THIS WEEK                               
         BH    GW050               AFTER TODAY: EXIT                            
         SPACE                                                                  
         ICM   R0,15,RCONBKAM                                                   
         A     R0,LASTWKL                                                       
         ST    R0,LASTWKL                                                       
         SPACE                                                                  
GW050    BAS   RE,NEXTEL                                                        
         BE    GW040                                                            
         DROP  R6                                                               
GW060    L     RF,THISWKL                                                       
         S     RF,THISWKF                                                       
         BAS   RE,ROUND                                                         
         ST    RF,CURWK                                                         
         SPACE                                                                  
         L     RF,LASTWKL                                                       
         S     RF,LASTWKF                                                       
         BAS   RE,ROUND                                                         
         ST    RF,LSTWK                                                         
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090RERRGONUP 10/22/14'                                      
         END                                                                    
