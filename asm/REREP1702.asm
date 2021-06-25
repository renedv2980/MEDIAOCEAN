*          DATA SET REREP1702  AT LEVEL 036 AS OF 04/29/04                      
*          DATA SET REREP1702  AT LEVEL 016 AS OF 04/02/96                      
*PHASE RE1702C,*                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE BINSRCH2                                                               
         MACRO                                                                  
&TAG     ADD1  &COUNTER                                                         
.*                                                                              
.*  ADD 1 TO FULLWORD COUNTER                                                   
.*                                                                              
.*  NOTE: THIS USES RE                                                          
.*                                                                              
&TAG     L     RE,&COUNTER                                                      
         LA    RE,1(RE)                                                         
         ST    RE,&COUNTER                                                      
         MEND                                                                   
         TITLE 'REREP1702 - RE1702 - STA ADV COMPARISON REPORT'                 
*                                                                               
***********************************************************************         
*                                                                     *         
*- REREP1702 --- STATION ADVERTISER COMPARISON REPORT                 *         
*                 (A.K.A.  THE DOUGHNUT REPORT)                       *         
*                                                                     *         
***********************************************************************         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  02/12/90  PJS -- ORIGINAL DEVELOPMENT                              *         
*                                                                     *         
*  MAR05/90 (MRR) --- CLEAR WORK AREA FOR 2ND + REQUEST               *         
*                     FORCE STATS TO A NEW PAGE                       *         
*                                                                     *         
*  MAR12/90 (MRR) --- CHANGE REPORT NAME, PRINT STATION MARKET NAME,  *         
*                      OTHER MINOR CLEAN-UP                           *         
*                                                                     *         
*  MAY02/90 (MRR) --- PUT REP CODE IN THE ADV KEY READ FOR INTEREP    *         
*                      REPS                                           *         
*                                                                     *         
*  MAR26/92 (MRR) --- >REPLACE TABLE DS WITH COVAIL CALL              *         
*                                                                     *         
*  OCT21/96 (BU ) --- >CORRECT # OF CARDS TEST FROM '=2' TO '>1'      *         
*                                                                   * *         
* JAN22/98 (JRD) --- 4K CONTRACTS                                   * *         
*                                                                   * *         
***********************************************************************         
*                                                                               
* READ ALL CONTRACTS FOR REP, ADV, AND STATION.  IF WITHIN INCLUSION            
* CRITERIA (STATION, DATE, & {MONEY, NO MONEY, OR EITHER}), TURN ON             
* BYTE REPRESENTING STATION AND LIST OFFICE AND AGENCY (MEANINGLESS)            
* IN TABLE, WHICH MIGHT LOOK LIKE THIS:                                         
*     ---------------------------------------------                             
*     | ADV CODE |OFC|AGENCY| 0 1 2 3 4 5 6 7 8 9 |                             
*     |   (4)    |(2)|  (6) |         (10)        |                             
*     |----------|---|------|---------------------|                             
*     |    PG    |NY |BLAIR | 1 0 1 0 1 0 0 1 0 1 |                             
*     |    GF    |ST |ITDONT| 0 0 1 0 0 0 0 0 1 0 |                             
*     |    XY    |UP |MATTER| 0 0 0 0 1 0 0 1 0 1 |                             
*     |    ZZ    |ID |SOIPUT| 0 0 0 1 0 0 0 0 1 1 |                             
*     |    AB    |FI |ANYTHI| 1 1 0 0 1 0 0 1 0 0 |                             
*     |    DL    |ELD|NGHERE| 1 0 0 0 1 0 0 0 1 1 |                             
*     ---------------------------------------------                             
* IF A '1' APPEARS IN A COL/ROW, THIS MEANS THAT AN ADVERTISER HAS              
* A CONTRACT ON THAT STATION WHICH MEETS THE INCLUSION CRITERIA.                
* IF AN AD IS RUNNING ON 'DEPTH' OR MORE STATIONS (APPEARS DEPTH OR             
* MORE TIMES IN ANY ROW), THEN WE TEST IF IT IS ALSO RUNNING ON THE             
* TARGET STATION, AND IF NOT, REPORT ON IT.                                     
*                                                                               
* IE:  WITH DEPTH=4, AND THE ABOVE TABLE, IF PG, AB, OR DL DON'T                
*      ADVERTISE ON THE TARGET STATION, THEN SUCH SHOULD BE                     
*      REPORTED, BUT NOT SO WITH GF, XY, OR ZZ SINCE THEY APPEAR                
*      ON LESS THAN 4 STATIONS.                                                 
*                                                                               
***********************************************************************         
*  CARD INPUT (2 CARDS):                                                        
*                                                                               
*  CARD 1                                                                       
*  ------                                                                       
*  QSTATION - TARGET STATION                                                    
*  QCATY    - CATEGORY                                                          
*  QSTART   - START DATE, YYMM                                                  
*  QEND     - END DATE,   YYMM                                                  
*  QOPTION1 - OPTION 1                                                          
*              M = MONEY                                                        
*              A = AVAIL                                                        
*              B = BOTH                                                         
*  QOPTION2 - 'T' = TRACE MODE  *SECRET OPTION*                                 
*                                                                               
*  CARD 2                                                                       
*  ------                                                                       
*  Q2DEPTH  - DEPTH. (THRESHOLD)                                                
*  Q2NUMSTA - STATION LIST COUNT                                                
*  Q2STALST - STATION LIST. UP TO 10 5-BYTE STATION CODES                       
*                                                                               
*  ALL PROCESSING DONE IN REQ FIRST MODE.                                       
*                                                                               
***********************************************************************         
*                                                                               
RE1702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1702,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
*                                                                               
         STM   R2,RC,SAVEREGS      SAVE REGS 2-C FOR HEADLINE                   
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   MAIN                                                             
*                                                                               
         L     RE,ADCONLST                                                      
         USING ADCONSD,RE                                                       
         L     RF,COVAIL                                                        
         DROP  RE                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
         OC    P2,P2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ADVTBL,P2                                                        
         L     RF,=F'1000000'                                                   
         L     RE,ADVTBL                                                        
         LR    R2,RE                                                            
         XCEF                                                                   
         A     R2,=F'1000000'                                                   
         ST    R2,ADVTBLX                                                       
*                                                                               
EXXMOD   XMOD1                                                                  
*                                                                               
MAIN     DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXXMOD                                                           
         BAS   RE,INITIAL          STARTUP                                      
         BNZ   ERROR                                                            
*                                                                               
**       MVC   P+1(12),=C'INITIAL DONE'                                         
**       GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,BLDTBL           READ ADVERTISERS, BUILD TBL.                 
         BNZ   ERROR                                                            
*                                                                               
**       MVC   P+1(12),=C'BLDTBL  DONE'                                         
**       GOTO1 REPORT                                                           
*                                                                               
*- FOR TARGET STATION AND EACH STATION IN STATION LIST,                         
*        READ CONTRACTS USING X'CC' KEY                                         
*        APPLY DATE RANGE FILTERS                                               
*        STATION LIST ONLY: APPLY MONEY/NO MONEY/BOTH TEST                      
*        MARK ADV TABLE ENTRY WHEN QUALIFYING CONTRACT FOUND.                   
*                                                                               
         MVC   FINDSTA,QSTATION    FIND TARGET                                  
         MVI   TARGET,1            THIS IS THE TARGET STATION                   
         BAS   RE,READCON                                                       
         BNZ   ERROR                                                            
*                                                                               
         LA    R1,CARD2                                                         
         USING CARD2D,R1                                                        
         LA    R2,Q2STALST         LIST OF STATIONS                             
         DROP  R1                                                               
         ZIC   R3,NUMSTA                                                        
         MVI   TARGET,0            NOT TARGET                                   
         SR    R4,R4               INDEX COUNTER                                
*                                                                               
MAIN100  EQU   *                                                                
         MVC   FINDSTA,0(R2)       STATION TO FIND                              
*                                                                               
         STC   R4,STAINDEX                                                      
*                                                                               
         BAS   RE,READCON          READ CONTRACTS                               
         BNZ   ERROR                                                            
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R2,5(R2)            NEXT STATION IN LIST                         
         BCT   R3,MAIN100                                                       
*                                                                               
*- TABLE NOW COMPLETELY FILLED IN.                                              
*  ANALYZE TABLE AND PRODUCE REPORT.                                            
         BAS   RE,DOREPORT                                                      
*                                                                               
*- PRINT OUT RUN COUTS                                                          
         BAS   RE,COUNTS                                                        
*                                                                               
*- ALL DONE, NO ERRORS                                                          
OKEXIT   GOTO1 REPORT                                                           
         MVC   P(L'OKMSG),OKMSG                                                 
         GOTO1 REPORT                                                           
         B     EXXMOD                                                           
*                                                                               
*- ALL DONE, ERRORS FOUND                                                       
ERROR    GOTO1 REPORT                                                           
         MVC   P(L'ERRMSG),ERRMSG                                               
         GOTO1 REPORT                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         ZICM  RF,=AL1(CARD2L),1                                                
         LA    RE,CARD2                                                         
         EX    RF,INITXC                                                        
         ZICM  RF,=AL1(COMMANDL),1                                              
         LA    RE,COMMAND                                                       
         EX    RF,INITXC                                                        
         L     RF,=F'1000000'                                                   
         L     RE,ADVTBL                                                        
         XCEF                                                                   
*                                                                               
         MVI   TRACE,0                                                          
         CLI   QOPTION2,C'T'       TRACE ?                                      
         BNE   *+8                                                              
         MVI   TRACE,1                                                          
*                                                                               
         LA    R0,HHOOK                                                         
         ST    R0,HEADHOOK                                                      
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    RE,IOAREA                                                        
         ST    RE,AIOAREA                                                       
*                                                                               
*        READ STATION RECORD TO GET MARKET NAME                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),QREP                                                   
         MVC   KEY+22(5),QSTATION                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INITERR                                                          
         GOTO1 GREC                                                             
         MVC   MKTNAME(20),RSTAMKT                                              
*                                                                               
*- CONVERT PERIOD START/END YYMM INTO BROADCAST DATES                           
         GOTO1 =V(GETBROAD),DMCB,(0,QSTART),IOAREA,0,0                          
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                BAD DATE                                     
         GOTO1 DATCON,DMCB,(0,IOAREA),(3,PERDSTRT),0                            
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(0,QEND),IOAREA,0,0                            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                BAD DATE                                     
         GOTO1 DATCON,DMCB,(0,IOAREA+6),(3,PERDEND),0                           
*                                                                               
*                                                                               
*- PICK UP 2ND REQUEST CARD                                                     
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),1             NEED 2 OR MORE CARDS.                        
         BH    INIT20                                                           
         MVC   P(L'CARDMISS),CARDMISS                                           
         B     INITERR                                                          
*                                                                               
INIT20   EQU   *                                                                
         L     R4,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R4)                                                     
         DROP  R5                                                               
*                                                                               
*- CONVERT EBCDIC DEPTH/STATION LIST COUNTS TO BINARY                           
         LA    R5,CARD2                                                         
         USING CARD2D,R5                                                        
         GOTO1 =V(NUMVAL),DMCB,(0,Q2DEPTH),2                                    
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                REQUEST CORRUPTED.                           
         MVC   DEPTH,DMCB+4+3                                                   
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,(0,Q2NUMSTA),2                                   
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                REQUEST CORRUPTED.                           
         MVC   NUMSTA,DMCB+4+3                                                  
         DROP  R5                                                               
*                                                                               
INITOK   SR    R0,R0               GOOD CC                                      
         B     TESTEXIT                                                         
*                                                                               
INITERR  LA    R0,1                BAD CC                                       
         B     TESTEXIT                                                         
         SPACE 3                                                                
INITXC   XC    0(0,RE),0(RE)                                                    
         EJECT                                                                  
*                                                                               
*- READ ADVERTISERS ON FILE AND PUT INTO TABLE.                                 
*                                                                               
BLDTBL   NTR1                                                                   
         L     R2,ADVTBL                                                        
         USING ADVTBLD,R2                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+25(2),QREP                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     BLDTBL20                                                         
*                                                                               
BLDTBL10 EQU   *                                                                
         MVC   KEY+25(2),QREP         REMIND THE SYSTEM WHO WE ARE              
         GOTO1 SEQ                                                              
*                                                                               
BLDTBL20 CLI   KEY,X'08'           END OF ADVERTISER KEYS?                      
         BNE   BLDTBL90                                                         
*                                                                               
*- KEY FILTER: MUST BE REQUEST REP OR 'ZZ' (GENERIC) REP                        
*  IF ZZ REP, ONLY ACCEPT IF DIFFERENT THAN LAST ADV                            
*     (PREVENT DUPLICATE ADV CODE IN TABLE)                                     
*                                                                               
         CLC   QREP,KEY+25                                                      
         BE    BLDTBL30                                                         
         CLC   =C'ZZ',KEY+25                                                    
         BNE   BLDTBL10                                                         
*                                                                               
         CLC   LASTADV,KEY+21                                                   
         BE    BLDTBL10            ALREADY HAVE THIS CODE                       
*                                                                               
BLDTBL30 EQU   *                                                                
         ADD1  NUMADVR             COUNT 1 READ                                 
*                                                                               
*- RECORD PASSES FILTERS.  ADD TO TABLE.                                        
         L     RE,ADVTBLX                                                       
         CR    R2,RE                                                            
         BL    BLDTBL50                                                         
*                                                                               
         MVC   P(L'ADVMAX),ADVMAX  TABLE OVERFLOW                               
         GOTO1 REPORT                                                           
         B     BLDTBLER                                                         
*                                                                               
BLDTBL50 ADD1  NUMADV              COUNT 1 ADDED TO TABLE                       
*                                                                               
         XC    ATENTRY(ATLNTRY),ATENTRY                                         
*                                                                               
         MVC   RADVKEY(27),KEY                                                  
*                                                                               
         MVC   ATADV,RADVKADV      ADV CODE                                     
         MVC   ATADDR,KEY+28       DISK ADDRESS                                 
*                                                                               
         MVC   LASTADV,RADVKADV    SAVE CURRENT ADV                             
*                                                                               
         LA    R2,ATLNTRY(R2)      NEXT TBL ENTRY                               
         B     BLDTBL10                                                         
*                                                                               
*- DONE.                                                                        
BLDTBL90 EQU   *                                                                
         SR    R0,R0               GOOD CC                                      
         B     TESTEXIT                                                         
*                                                                               
BLDTBLER EQU   *                   BAD CC                                       
         LA    R0,1                BAD CC                                       
         B     TESTEXIT                                                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- READCON -- READ CONTRACTS FOR STATION CALL LETTERS IN 'FINDSTA'.             
*             MARK ADVTBL WHEN QUALIFIED CONTRACT FOUND.                        
*                                                                               
*  NOTE: WHEN AN ADV CODE IS UNQUALIFIED, IT'S OK TO SKIP TO NEXT               
*        ADV CODE.  WHEN A CONTRACT IS TOSSED FOR OTHER REASONS                 
*        (LIKE DATE FILTERS), MUST READ SEQUENTIAL TO NOT MISS ANY              
*        OTHER CONTRACTS WHICH MAY PASS FILTERS.                                
*                                                                               
READCON  NTR1                                                                   
*                                                                               
*- BUILD 1ST KEY                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'CC'           CONTRACT 'CC' PASSIVE POINTER                
         MVC   KEY+1(2),QREP                                                    
         MVC   KEY+3(5),FINDSTA    CALL LETTERS TO FIND.                        
*                                                                               
CON050   GOTO1 HIGH                1ST AND SKIP READ                            
         B     CON120                                                           
*                                                                               
CON100   GOTO1 SEQ                 NEXT CONTRACT                                
*                                                                               
CON120   EQU   *                                                                
         CLC   KEY(8),KEYSAVE      = THRU CALL LETTERS?                         
         BE    CON130                                                           
*                                                                               
         SR    R0,R0               EXIT W/GOOD CC                               
         B     TESTEXIT                                                         
*                                                                               
*- CONTRACT KEY FILTERING.                                                      
*        ADV MUST BE IN ADVTBL                                                  
*        MUST BE NO ACTIVITY ON TARGET STATION                                  
*                                                                               
CON130   EQU   *                                                                
         L     R3,ADVTBL                                                        
         USING ADVTBLD,R3                                                       
         L     R4,NUMADV           # ENTRIES IN TABLE                           
         MVC   RCONKEY,KEY                                                      
*                                                                               
*- ADV CODE IN THE TABLE?                                                       
CON140   EQU   *                                                                
         LA    R5,RCONSADV-1       A(KEY TO MATCH ON LESS DISPLACEMENT)         
         GOTO1 =V(BINSRCH),DMCB,(0,(R5)),(R3),(R4),ATLNTRY,(1,4),(R4)           
         CLI   DMCB,X'01'          NOT FOUND?                                   
         BE    CON300                                                           
*                                                                               
         L     R3,DMCB             A(RECORD)                                    
*                                                                               
*- IF ADV HAD ACTIVITY ON TARGET STATION, SKIP TO NEXT ADV.                     
CON150   EQU   *                                                                
         CLI   ATGTSTA,1           USED BY TARGET?                              
         BE    CON300              YES. REJECT THIS ADV.                        
*                                                                               
*- RECORD FILTERING                                                             
*        CATEGORY MUST MATCH RQST CATEGORY (IF ANY)                             
*        CONTRACT MUST BE WITHIN REQUEST START/END DATES                        
*        NON-TARGET STATIONS: CHECK FOR MONEY/NONE/BOTH FILTER                  
*                                                                               
*  IF CONTRACTS FAIL RECORD FILTERING WE MUST READ (SEQ) NEXT                   
*     CONTRACT ON FILE. DO NOT (!!!) SKIP TO NEXT ADV.                          
*                                                                               
         GOTO1 GREC                                                             
*                                                                               
         CLC   QCATY,SPACES                                                     
         BE    CON160                                                           
         CLC   QCATY,RCONCTGY      CATEGORY                                     
         BNE   CON100                                                           
*                                                                               
CON160   EQU   *                                                                
         CLC   PERDEND,RCONDATE     RQST END -VS- CONTRACT START                
         BL    CON100                                                           
*                                                                               
         CLC   PERDSTRT,RCONDATE+3  RQST START -VS- CONTRACT END                
         BH    CON100                                                           
*                                                                               
         CLI   TARGET,1            ARE WE THE TARGET STATION?                   
         BE    CON240              YES.                                         
*                                                                               
         CLI   QOPTION1,C'B'       TAKING BOTH?                                 
         BE    CON240                                                           
*                                                                               
         BAS   RE,ANYMONEY         MONEY ON CONTRACT.                           
         BZ    CON220              NO.                                          
*                                                                               
*- CONTRACT HAS MONEY....DO WE WANT IT?                                         
         CLI   QOPTION1,C'A'                                                    
         BE    CON100              NO.                                          
         B     CON240                                                           
*                                                                               
*- CONTRACT HAS NO MONEY....DO WE WANT IT?                                      
CON220   CLI   QOPTION1,C'M'                                                    
         BE    CON100              NO.                                          
*                                                                               
*- CONTRACT PASSES RECORD FILTERS.                                              
*  MARK ADVTBL ENTRY FOR TARGET OR STATION LIST ENTRY                           
*  SKIP TO NEXT ADVERTISER CODE.                                                
CON240   EQU   *                                                                
         CLI   TARGET,1                                                         
         BNE   CON260                                                           
         MVI   ATGTSTA,1           ACTIVITY ON TARGET                           
         B     CON280                                                           
*                                                                               
CON260   ZIC   R1,STAINDEX         0-9 BYTE INDEX                               
         LA    R1,ATCOUNT(R1)                                                   
         MVI   0(R1),1             ACTIVITY ON THIS STATION                     
*                                                                               
*- PICK UP INFO FIELDS FROM CONTRACT                                            
CON280   EQU   *                                                                
         MVC   ATOFF,RCONKOFF      REP OFFICE CODE                              
         MVC   ATAGY,RCONKAGY      AGENCY CODE                                  
         MVC   ATAGYOFF,RCONKAOF   AGENCY OFFICE CODE                           
*                                                                               
*- SET UP FOR SKIP-READ TO NEXT ADVERTISER CODE                                 
CON300   EQU   *                                                                
         MVC   KEY+(RCONSAGY-RCONREC)(4),=4X'FF'                                
         B     CON050                                                           
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*- ANYMONEY - DETERMINE IF CONTRACT HAS ANY MONEY                               
*                                                                               
*  MONEY DEFINED AS AN X'03' OR X'04' ELEMENT PRESENT ON RECORD.                
*                                                                               
*  CC: 0 = NO MONEY,  ^0 = MONEY                                                
*                                                                               
ANYMONEY NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    ANY$YES                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    ANY$YES                                                          
*                                                                               
ANY$NO   SR    R0,R0               NO MONEY                                     
         B     TESTEXIT                                                         
ANY$YES  LA    R0,1                YES MONEY                                    
         B     TESTEXIT                                                         
         SPACE 2                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*- DOREPORT - ANALYZE TABLE, PRINT REPORT                                       
*                                                                               
*  AFTER THIS ROUTINE, ACTIVITY COUNTERS #OVER, #UNDER, #ONTGT                  
*  ARE SET TO REFLECT NUMBER OF ENTRIES OVER DEPTH (PRINTED),                   
*  UNDER DEPTH (SUPPRESSED), OR ACTIVE ON TARGET STATION (EXCLUDED)             
*                                                                               
DOREPORT NTR1                                                                   
         L     R2,ADVTBL                                                        
         USING ADVTBLD,R2                                                       
         L     R3,NUMADV           LOOP FOR # OF ENTRIES                        
*                                                                               
*- ENTRY SELECTION FILTER                                                       
*        1. EXCLUDE IF ACTIVITY ON TARGET STATION                               
*        2. EXCLUDE IF NO ACTIVITY ON STATION LIST                              
*        3. EXCLUDE IF ACTIVITY DEPTH LESS THAN REQUEST DEPTH                   
*                                                                               
RPT100   EQU   *                                                                
         CLI   ATGTSTA,1           TARGET STA ACTIVITY?                         
         BE    RPT150                                                           
*                                                                               
         BAS   RE,ADDEMUP          DETERMINE ACTIVITY DEPTH                     
*                                                                               
         CLI   ACTVDPTH,0          NO ACTIVITY?                                 
         BE    RPT200                                                           
*                                                                               
         CLC   ACTVDPTH,DEPTH      ACTIVITY -VS- RQST DEPTH                     
         BL    RPT160                                                           
*                                                                               
         ADD1  #OVER               COUNT 1 OVER DEPTH                           
*                                                                               
         BAS   RE,PRINTIT          FORMAT & PRINT                               
         B     RPT200                                                           
*                                                                               
RPT150   ADD1  #ONTGT              COUNT 1 ON TARGET STATION                    
         B     RPT200                                                           
*                                                                               
RPT160   ADD1  #UNDER              COUNT 1 UNDER DEPTH                          
         B     RPT200                                                           
*                                                                               
*- LOOK AT NEXT ADVTBL ENTRY                                                    
RPT200   EQU   *                                                                
         LA    R2,ATLNTRY(R2)                                                   
         BCT   R3,RPT100                                                        
         DROP  R2                                                               
*                                                                               
         CLC   #OVER,=F'0'         DID WE PRINT ANYTHING?                       
         BNE   RPTEXT                                                           
*                                                                               
         MVC   P(L'NODETAIL),NODETAIL                                           
         GOTO1 REPORT                                                           
*                                                                               
RPTEXT   EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 2                                                                
*                                                                               
*- ADDEMUP -- COUNT NUMBER OF STATIONS WITH ACTIVITY IN ADVTBL ENTRY            
*                                                                               
*  INPUT:  R2 = A(ADVTBL ENTRY)                                                 
*  RETURN: ACTVDPTH = 1 BYTE BINARY ACTIVITY COUNT.                             
*                                                                               
ADDEMUP  NTR1                                                                   
         USING ADVTBLD,R2                                                       
         LA    R2,ATCOUNT                                                       
         DROP  R2                                                               
         SR    R3,R3               COUNT GOES HERE                              
         LA    R0,10               UP TO 10 STATIONS IN GRID                    
ADDEM10  CLI   0(R2),1                                                          
         BNE   ADDEM20                                                          
         LA    R3,1(R3)            COUNT 1                                      
ADDEM20  LA    R2,1(R2)                                                         
         BCT   R0,ADDEM10                                                       
         STC   R3,ACTVDPTH         PASS BACK COUNT                              
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 2                                                                
*                                                                               
*- PRINTIT -- FORMAT ADVTBL ENTRY AT R2 AND PRINT.                              
PRINTIT  NTR1                                                                   
         USING ADVTBLD,R2                                                       
*                                                                               
         MVC   KEY+28(4),ATADDR    READ IN ADV REC FOR NAME                     
         GOTO1 GREC                                                             
*                                                                               
         LA    R4,P                                                             
         USING LINE1D,R4           LINE 1                                       
*                                                                               
         MVC   L1ADV,ATADV                                                      
         MVI   L1DASH,C'-'                                                      
         MVC   L1ADVNAM,RADVNAME                                                
         GOTO1 REPORT                                                           
*                                                                               
         USING LINE2D,R4           LINE 2                                       
*                                                                               
         MVC   LOFF,ATOFF                                                       
         LA    R0,4                                                             
         LA    R1,ATAGY                                                         
         LA    RF,LAGY                                                          
         BAS   RE,MOVENB           UP TO 4 BYTE AGY.                            
         CLC   ATAGYOFF,SPACES                                                  
         BE    PRNT100                                                          
         MVI   0(RF),C'-'                                                       
         MVC   1(2,RF),ATAGYOFF    OPTIONAL AGENCY OFFICE                       
*                                                                               
PRNT100  EQU   *                                                                
         LA    RE,LSTAR            PRINT ACTIVITY GRID                          
         LA    RF,ATCOUNT                                                       
         ZIC   R0,NUMSTA                                                        
*                                                                               
PRNT120  CLI   0(RF),1             ACTIVTY FOR THIS STA?                        
         BNE   PRNT140                                                          
         MVI   0(RE),C'*'          MARK AS ACTIVE                               
PRNT140  LA    RE,L'LLIST(RE)      NEXT PRINT AREA                              
         LA    RF,1(RF)            NEXT ADVTBL COUNT                            
         BCT   R0,PRNT120                                                       
*                                                                               
         DROP  R2,R4                                                            
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- COUNTS - PRINT OUT RUN COUNTS                                                
*                                                                               
*  COUNTS PRINTED:                                                              
*                                                                               
*        NUMBER OF ACTIVE ADVERTISERS (TGT OR LIST ACTIVITY)                    
*                                                                               
*        NUMBER OF ADVERTISERS OVER, UNDER DEPTH                                
*                                                                               
*        NUMBER OF ADVERTISERS ON TARGET STATION                                
*                                                                               
*        NUMBER OF ADVERTISERS ACTIVE ON EACH STATION IN LIST                   
*          (COLUMN TOTALS)                                                      
*                                                                               
*        NUMBER OF ADVERTISERS ACTIVE ON 1, 2, 3,...10, ALL                     
*          STATIONS (ALL = EACH STATION IN LIST + TARGET)                       
*          (ROW TOTALS)                                                         
*                                                                               
COUNTS   NTR1                                                                   
         L     R2,ADVTBL                                                        
         USING ADVTBLD,R2                                                       
         L     R3,NUMADV           LOOP FOR # OF ENTRIES                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
COUNT100 EQU   *                                                                
         BAS   RE,ADDEMUP          DETERMINE ACTIVITY DEPTH                     
         ZIC   RE,ACTVDPTH                                                      
         ZIC   RF,ATGTSTA                                                       
         AR    RE,RF                                                            
         BZ    COUNT500            SKIP ADV WITH NO ACTIVITY                    
*                                                                               
*- DUMP OUT TABLE ENTRIES WITH ANY ACTIVITY FOR TRACE                           
         CLI   TRACE,0             PRINT TABLE ON TRACE                         
         BE    COUNT120                                                         
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   P,C'*'              ACTIVE ON TARGET                             
         BAS   RE,PRINTIT                                                       
COUNT120 EQU   *                                                                
*                                                                               
*- COUNT NUMBER OF ACTIVE ADVTBL ENTRIES                                        
         ADD1  #ACTIVE                                                          
*                                                                               
*- COUNT BY ACTIVITY LEVEL (ROW TOTALS)                                         
         ZIC   R4,ACTVDPTH                                                      
         SLL   R4,2                ACTIVITY COUNT * 4                           
         LA    RF,STAACTIV-4       A(COUNTER FOR ACTIVITY LVL)                  
         AR    RF,R4                                                            
         ADD1  0(RF)                                                            
*                                                                               
*- ADD TO COUNTS BY STATION (COLUMN TOTALS)                                     
         LA    R4,ATCOUNT          ACTIVITY INDICATOR BYTES                     
         LA    R5,STACOUNT         COUNTERS                                     
         LA    R0,10               UP TO                                        
*                                                                               
COUNT200 CLI   0(R4),1                                                          
         BNE   COUNT220            NOT ACTIVE                                   
         ADD1  0(R5)                                                            
COUNT220 LA    R4,1(R4)            NEXT INDICATOR                               
         LA    R5,4(R5)            NEXT COUNTER                                 
         BCT   R0,COUNT200                                                      
*                                                                               
*- LOOK AT NEXT ADVTBL ENTRY                                                    
COUNT500 EQU   *                                                                
         LA    R2,ATLNTRY(R2)                                                   
         BCT   R3,COUNT100                                                      
         DROP  R2                                                               
         SPACE 2                                                                
*                                                                               
*- ALL COUNTERS DETERMINED.                                                     
*  PRINT OUT COUNTS ON REPORT.                                                  
         GOTO1 REPORT              BLANK LINE                                   
*                                                                               
*- ACTIVE ON THE COMPETITION (COLUMN TOTALS)                                    
         MVC   P(L'COLTOT),COLTOT                                               
         LA    R2,STACOUNT                                                      
         ZIC   R3,NUMSTA                                                        
         LA    R4,P                                                             
         USING LINE2D,R4                                                        
         LA    R4,LLIST                                                         
*                                                                               
COUNT520 L     R5,0(R2)                                                         
         EDIT  (R5),(6,(R4)),COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFT                 
         LA    R2,4(R2)                                                         
         LA    R4,L'LLIST(R4)                                                   
         BCT   R3,COUNT520                                                      
         DROP  R4                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
*- ACTIVITY LEVELS                                                              
         MVC   P(L'ACTLVL),ACTLVL                                               
         LA    R2,STAACTIV                                                      
         ZIC   R3,NUMSTA           # COMPETING STATIONS                         
         LA    R3,1(R3)                                                         
         LA    R6,1                                                             
*                                                                               
COUNT540 EQU   *                                                                
         LA    R4,P+3+L'ACTLVL     ACTIVE ON NN STATIONS                        
         EDIT  (R6),(2,(R4))                                                    
         LA    R4,3(R4)                                                         
         MVC   0(8,R4),=C'STATIONS'                                             
         CH    R6,=H'1'                                                         
         BNE   COUNT545                                                         
         MVI   7(R4),C' '                                                       
COUNT545 EQU   *                                                                
         LA    R4,12(R4)                                                        
*                                                                               
         L     R5,0(R2)                                                         
         EDIT  (R5),(6,(R4)),COMMAS=YES,ZERO=NOBLANK                            
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,4(R2)            NEXT TOTAL COUNTE                            
         LA    R6,1(R6)            NEXT LEVEL INDEX                             
         BCT   R3,COUNT540                                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'TOTADV),TOTADV                                               
         EDIT  (4,NUMADV),(12,P+30),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'NUMACT),NUMACT                                               
         EDIT  (4,#ACTIVE),(12,P+30),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'NUMOVR),NUMOVR                                               
         EDIT  (4,#OVER),(12,P+30),COMMAS=YES,ZERO=NOBLANK                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'NUMUND),NUMUND                                               
         EDIT  (4,#UNDER),(12,P+30),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'NUMONT),NUMONT                                               
         EDIT  (4,#ONTGT),(12,P+30),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF            ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF               A(SAVED REGISTERS)                           
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
*- LINE 3 - START/END DATES                                                     
         MVC   HEAD3+54(6),=C'PERIOD'                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(9,HEAD3+61)                              
         MVI   HEAD3+68,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,QEND),(9,HEAD3+70)                                
*                                                                               
*- LINE 4.  LEFT=TARGET STATION, RIGHT=OPTION 1                                 
*                                                                               
         MVC   HEAD4+1(7),=C'STATION'                                           
         LA    R0,4                                                             
         LA    R1,QSTATION                                                      
         LA    RF,HEAD4+11                                                      
         BAS   RE,MOVENB                                                        
         CLI   QSTATION+4,C' '                                                  
         BE    HHOOK410                                                         
         MVI   0(RF),C'-'          -BAND                                        
         MVC   1(1,RF),QSTATION+4                                               
         LA    RF,2(RF)                                                         
HHOOK410 EQU   *                                                                
         MVC   2(20,RF),MKTNAME                                                 
*                                                                               
         LA    RE,ALLCON           INCLUDE ALL CONTRACTS                        
         LA    RF,L'ALLCON                                                      
         CLI   QOPTION1,C'B'                                                    
         BE    HHOOK420                                                         
*                                                                               
         LA    RE,CON$             CONTRACTS WITH MONEY                         
         LA    RF,L'CON$                                                        
         CLI   QOPTION1,C'M'                                                    
         BE    HHOOK420                                                         
*                                                                               
         LA    RE,AVLONLY          AVAILS ONLY                                  
         LA    RF,L'AVLONLY                                                     
         CLI   QOPTION1,C'A'                                                    
         BE    HHOOK420                                                         
*                                                                               
         B     HHOOK500            UNKNOWN OPTION                               
*                                                                               
HHOOK420 BCTR  RF,0                                                             
         EX    RF,HHOOK430                                                      
         B     HHOOK500                                                         
HHOOK430 MVC   HEAD4+RIGHT(0),0(RE)                                             
*                                                                               
*- LINE 5 -- LEFT=CATEGORY; RIGHT=DEPTH                                         
*                                                                               
HHOOK500 EQU   *                                                                
         CLC   QCATY,SPACES                                                     
         BE    HHOOK510                                                         
         MVC   HEAD5+1(08),=CL08'CATEGORY'                                      
         MVC   HEAD5+11(02),QCATY                                               
*                                                                               
HHOOK510 EQU   *                                                                
         MVC   HEAD5+RIGHT(5),=C'DEPTH'                                         
         LA    RE,CARD2                                                         
         USING CARD2D,RE                                                        
         MVC   HEAD5+RIGHT+7(2),Q2DEPTH                                         
         DROP  RE                                                               
*                                                                               
*- LINE 7                                                                       
*  DISPLAY STATION LIST                                                         
HHOOK700 LA    R2,HEAD7                                                         
         USING LINE2D,R2                                                        
         LA    R2,LLIST                                                         
         DROP  R2                                                               
*                                                                               
         LA    R4,CARD2                                                         
         USING CARD2D,R4                                                        
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,(0,Q2NUMSTA),2                                   
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                REQUEST CORRUPTED.                           
         L     R3,DMCB+4                                                        
*                                                                               
         LA    R4,Q2STALST                                                      
         DROP  R4                                                               
HHOOK730 EQU   *                                                                
         LA    R0,4                MOVE CALL LETTERS                            
         LR    R1,R4                 FROM CARD 2                                
         LR    RF,R2                 TO   PRINT LINE                            
         BAS   RE,MOVENB                                                        
         CLI   4(R4),C' '                                                       
         BE    HHOOK740                                                         
         MVI   0(RF),C'-'          -BAND                                        
         MVC   1(1,RF),4(R4)                                                    
         LA    RF,2(RF)                                                         
*                                                                               
*- UNDERLINE STATION WE JUST DISPLAYED                                          
HHOOK740 EQU   *                                                                
         SR    RF,R2               # CHARS MOVED                                
         BCTR  RF,0                                                             
         LA    R1,HEAD8-HEAD7(R2)  A(SAME COLUMN, NEXT HEADLINE)                
         EX    RF,HHOOK745                                                      
         B     HHOOK750                                                         
HHOOK745 MVC   0(0,R1),UNDERLIN                                                 
*                                                                               
HHOOK750 EQU   *                                                                
         LA    R4,5(R4)            NEXT STATION ON RQST CARD                    
         LA    R2,8(R2)            NEXT PRINT AREA                              
         BCT   R3,HHOOK730                                                      
*                                                                               
HHOOKEXT EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         SPACE 2                                                                
SAVEREGS DS    11F                 REGS 2 THRU C FOR HEADHOOK                   
         SPACE 2                                                                
*                                                                               
*- MOVENB -- MOVE NON-BLANKS                                                    
*                                                                               
*  INPUT R0 = NUMBER OF CHARACTERS TO MOVE  (BINARY 1, 2, 3, ETC)               
*        R1 = A(SOURCE)  'FROM'                                                 
*        RE = LINK REG                                                          
*        RF = A(TARGET)  'TO'.  POINTS TO NEXT AVAILABLE BYTE AT END            
MOVENB   EQU   *                                                                
         CLI   0(R1),C' '          STOP ON BLANK                                
         BER   RE                                                               
         MVC   0(1,RF),0(R1)       MOVE SOURCE TO TARGET                        
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,MOVENB           LOOP BACK FOR NEXT BYTE                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        COMMON ROUTINES                                                        
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
       ++INCLUDE RGENIO            DATA MANAGER INTERFACE                       
         EJECT                                                                  
RELO     DS    F                                                                
ADVTBL   DS    F                                                                
ADVTBLX  DS    F                                                                
*                                                                               
ONE      DC    F'1'                                                             
OKMSG    DC    C'** END OF REPORT **'                                           
ERRMSG   DC    C'** ERRORS FOUND.  REPORT TERMINATED **'                        
CARDMISS DC    C'SECOND REQUEST CARD MISSING'                                   
ADVMAX   DC    C'ADVERTISER TABLE OVERFLOW'                                     
ALLCON   DC    C'INCLUDES ALL CONTRACTS'                                        
CON$     DC    C'CONTRACTS WITH MONEY ONLY'                                     
AVLONLY  DC    C'AVAIL HEADERS ONLY'                                            
UNDERLIN DC    10C'-'                                                           
TOTADV   DC    C'TOTAL NUMBER OF ADVERTISERS'                                   
NUMACT   DC    C'NUMBER OF ACTIVE ADVERTISERS'                                  
NUMOVR   DC    C'ADVERTISERS MEETING DEPTH'                                     
NUMUND   DC    C'ADVERTISERS UNDER DEPTH'                                       
NUMONT   DC    C'ADVERTISERS ON TARGET STATION'                                 
COLTOT   DC    C'ADVERTISERS ON COMPETITION'                                    
ACTLVL   DC    C'ADVERTISERS ACTIVE ON'                                         
NODETAIL DC    C'NO ADVERTISERS QUALIFY AT CURRENT DEPTH LEVEL.'                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DC    C'** START OF MY WORK AREA **'                                   
*                                                                               
         DC    C'**CARD2**'                                                     
CARD2    DS    CL80                2ND  RQST CARD                               
CARD2X   EQU   *                                                                
CARD2L   EQU   CARD2X-CARD2                                                     
*                                                                               
         DC    C'**COMMAND**'                                                   
COMMAND  DS    CL8                 FOR RGENIO                                   
AIOAREA  DS    A                   A(IO BUFFER)                                 
*                                                                               
NUMADVR  DS    F                   NUMBER ADV RECS READ                         
NUMADV   DS    F                   NUMBER ADV ENTRYIES IN TBL                   
*                                                                               
STACOUNT DS    10F                 # ADV ON STATIONS IN STALIST.                
*                                                                               
STAACTIV DS    10F                 # ADV ACTIV ON 1,2...10 COMP STA'S           
*                                                                               
#UNDER   DS    F                   NUMBER ADV ENTRIES UNDER/OVER                
#OVER    DS    F                     DEPTH NUMBER (THRESHOLD)                   
#ONTGT   DS    F                   NUMBER ADV'S ON TGT STATION                  
#ACTIVE  DS    F                   NUMBER OF ACTIVE ADVERTISERS                 
*                                                                               
*                                                                               
DEPTH    DS    X                   BINARY DEPTH VALUE                           
NUMSTA   DS    X                   BINARY NUMBER STATION IN STALIST.            
*                                                                               
TRACE    DS    X                   ^0 = TRACE MODE                              
*                                                                               
LASTADV  DS    CL4                 USED BY BLDTBL .                             
*                                                                               
FINDSTA  DS    CL5                 STATION CALL LETTERS FOR                     
*                                    CONTRACT READING ROUTINE                   
TARGET   DS    X                   1 = LOOKING FOR TARGET STATION               
*                                                                               
STAINDEX DS    X                   INDEX FOR STATION LIST (0-9)                 
*                                                                               
ELCODE   DS    X                   FOR GETEL                                    
*                                                                               
ACTVDPTH DS    X                   # ACTIVE STATIONS IN ADVTBL ENTRY            
*                                                                               
PERDSTRT DS    XL3                 YMD START                                    
PERDEND  DS    XL3                 YMD END                                      
*                                                                               
MKTNAME  DS    CL20                TARGET STATION MARKET                        
*                                                                               
COMMANDX EQU   *                                                                
COMMANDL EQU   COMMANDX-COMMAND                                                 
*                                                                               
         DC    C'**IO**'                                                        
IOAREA   DS    6048X               IO BUFFER                                    
IOAREAX  EQU   *                                                                
IOAREAL  EQU   IOAREAX-IOAREA                                                   
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
*                                                                               
         ORG                                                                    
*                                                                               
VDATAMGR EQU   DATAMGR                                                          
*                                                                               
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         SPACE 2                                                                
       ++INCLUDE REXADDRD                                                       
         EJECT                                                                  
*                                                                               
*- ADVERTISER TABLE DSECT.                                                      
*                                                                               
ADVTBLD  DSECT                                                                  
ATENTRY  DS    0C                                                               
ATGTSTA  DS    XL1                 1=CONTRACTS FOR TARGET STATION               
ATADV    DS    CL4                 ADVERTISER CODE                              
ATADDR   DS    CL4                 ADV REC DISK ADDRESS                         
ATOFF    DS    CL2                 REP OFFICE CODE                              
ATAGY    DS    CL4                 AGENCY                                       
ATAGYOFF DS    CL2                 AGENCY OFFICE                                
ATCOUNT  DS    10X                 1 BYTE PER STATION/UP TO 10 STA.             
*                                  1=CONTRACTS FOR ENTRY. 0=NONE                
ATLNTRY  EQU   *-ATENTRY           ENTRY LENGTH                                 
         SPACE 2                                                                
*                                                                               
*- REQUEST CARD 2 DSECT.                                                        
*                                                                               
CARD2D   DSECT                                                                  
Q2DEPTH  DS    CL2                 DEPTH. 01-10                                 
Q2NUMSTA DS    CL2                 NUMBER STATIONS IN LIST. 01-10.              
Q2STALST DS    10CL5               STATION LIST                                 
         SPACE 2                                                                
*                                                                               
*- REPORT PRINT LINE DSECT                                                      
*                                                                               
LINE1D   DSECT                     LINE 1                                       
         DS    C                                                                
L1ADV    DS    CL4                                                              
         DS    CL1                                                              
L1DASH   DS    CL1                                                              
         DS    CL1                                                              
L1ADVNAM DS    CL20                                                             
         SPACE                                                                  
LINE2D   DSECT                     LINE 2                                       
         DS    C                                                                
         DS    CL8                                                              
LOFF     DS    CL2                                                              
         DS    CL5                                                              
LAGY     DS    CL4                                                              
LAGYOFF  DS    CL3                                                              
         DS    CL4                                                              
LLIST    DS    CL8                                                              
         ORG   LLIST                                                            
         DS    CL2                                                              
LSTAR    DS    CL1                                                              
         DS    CL5                                                              
         SPACE 2                                                                
*                                                                               
RIGHT    EQU   98                  COLUMN # FOR RIGHT SIDE OF REPORT            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036REREP1702 04/29/04'                                      
         END                                                                    
