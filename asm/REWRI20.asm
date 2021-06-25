*          DATA SET REWRI20    AT LEVEL 053 AS OF 01/14/13                      
*          DATA SET REWRI20    AT LEVEL 218 AS OF 07/07/98                      
*PHASE T82120A,*                                                                
*INCLUDE REPIO                                                                  
*INCLUDE REGENBUC                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE PRNTBL                                                                 
*                                                                               
*********************************************************************           
*                                                                   *           
*          REWRI20 (T82120) - AUR REPORT                            *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 10JUN09    KUI -- INCREASE STATION TABLE SIZE                     *           
* 14JUL04    BU  -- NEW STATION FILTRATION                          *           
* 22SEP98    BOB -- NEW DOWNLOAD VERSION                            *           
* 14JUL98    BU  -- ALL-STATION FINAL FIX                           *           
* 24JUN98    BU  -- EXPAND ITEM TABLE                               *           
* 09APR98 30 EFJ -- ALLOW UP TO 10 DEMOS FOR SPECDOWN               *           
* 07APR98 29 EFJ -- REMOVE 4 MONTH LIMIT WITH DEMOS FOR AURDOWN     *           
* 27MAR98 28 EFJ -- ALMOST HAD L27...                               *           
* 26MAR98 27 EFJ -- CHANGE FLPRT LOGIC TO NO PRT                    *           
* 26MAR98 26 EFJ -- PASS P3 TO DAYVAL (IT'S VERY UNHAPPY W/OUT IT)  *           
* 25MAR98 25 EFJ -- FETCH NEEDS 2K IO AREAS                         *           
* 17MAR98 24 EFJ -- SET FLPRT IN HBUY S.T. HEADS PRINT ON DOWNLOAD  *           
* 12MAR98 23 EFJ -- SET OPTION FOR SPECIAL DOWNLOAD                 *           
* 27FEB98 22 EFJ -- ADD INVENTORY EFFECTIVE DATES                   *           
* 25FEB98 21 EFJ -- USE AVAIL DATE IF VALID                         *           
* 30JAN98 20 EFJ -- BREAK STACK INTO COLS FOR SPECIAL DOWNLOAD      *           
* 07JAN98 19 EFJ -- SUPPORT MARKET IN HEADER                        *           
*                -- PRINT STATION & MKT NAME IF ONLY ONE STA        *           
* 17JUN97 18 EFJ -- SUPPORT ALL STATION REQUEST                     *           
* 04JUN97 17 EFJ -- GET FIRST/PRIMARY DEMO ON BUY AS MIDLINE        *           
* 03JUN97 16 EFJ -- SUPPORT STREAMLINED REPORT                      *           
* 16APR97 15 EFJ -- DON'T DIE ON OVERFLOW                           *           
* 13FEB97 14 EFJ -- SUPPORT DEMOS                                   *           
* 12FEB97 13 EFJ -- MOVE AVG CPS UNDER NUMBER OF SPOTS              *           
* 08JUL96 12 EFJ -- CHANGE REFETCHDB TO REFETCHD                    *           
* 01JUL96 11 EFJ -- INCLUDE REFETCHDB FOR YET ANOTHER NEW FETCH     *           
* 19JUN96 10 EFJ -- OPTION TO SUPPORT SATELLITES                    *           
* 04JUN96 09 EFJ -- TRY INCLUDING REFETCHD INSTEAD OF REFETCHDA     *           
*                   FOR NEW SF VERSION OF FETCH                     *           
* 08MAY96 08 EFJ -- INV NUMBERS CAN BE ANY LENGTH & ALPHA           *           
* 03MAY96 07 EFJ -- SUBTRACT SPOTS FOR NEGATIVE $$$                 *           
* 25APR96 06 EFJ -- MOVE COVAIL CALLS TO PREP                       *           
* 24APR96 05 EFJ -- FIX LOGIC FOR TIME IF NO END TIME               *           
* 22APR96 04 EFJ -- FIX DATES FOR TOTAL COL FOR TSPOTS              *           
* 22APR96 03 EFJ -- DEFAULT TO WITHIN FOR MATCH TYPE                *           
* 19APR96 02 EFJ -- FIX DATES USED FOR TOTAL COLUMN                 *           
*                -- CAN BE NEGATIVE RBUYTSPT                        *           
* 06FEB96 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*********************************************************************           
         TITLE 'T82120 - AUR'                                                   
T82120   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T82120,R7,R8,RR=R2,CLEAR=YES                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         MVC   MYPRINT,VPRINT                                                   
         DROP  RF                                                               
*                                                                               
         ST    R2,MYRELO           SAVE RELOCATION FACTOR                       
*                                                                               
         CLI   MODE,RUNFRST        RUNFIRST                                     
         BE    FRST                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PRINT THE REPORT                             
         BNE   *+12                                                             
         BAS   RE,PREP                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,RUNLAST        RUNLAST                                      
         BE    LAST                                                             
*                                                                               
         ANSR  X=N                                                              
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUNFIRST                                                            *         
***********************************************************************         
         SPACE 1                                                                
FRST     DS    0H                                                               
FRSTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUNLAST - AGENCY SUMMARY REPORT                                     *         
***********************************************************************         
         SPACE 1                                                                
LAST     DS    0H                                                               
LASTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     NTR1                                                                   
         MVC   REQPRI2,=C'3'       SET PRIORITY FOR SOON JOBS                   
         GOTO1 USERVAL             GET REP NAME & ADDRESS                       
*                                  VALIDATE OPTIONS BEFORE OTHERS               
         LA    R2,AUROPTH                                                       
         GOTO1 VALOPTS                                                          
         TM    OPTIND1,AURWEEK     WEEKLY ONLY VALID FOR BUY ACTIVITY           
         BZ    *+20                                                             
         TM    OPTIND1,AURBYACT                                                 
         BNZ   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                  VALIDATE REGION                              
*         LA    R2,AURREGH         NOT SUPPORTED                                
*         GOTO1 VALIREG                                                         
*                                  VALIDATE OFFICE                              
         LA    R2,AUROFFH                                                       
         GOTO1 VALIOFF                                                          
*                                  VALIDATE SALESPERSON                         
         LA    R2,AURSALH                                                       
         GOTO1 VALISAL                                                          
*                                  VALIDATE DIV/TEAM                            
         LA    R2,AURTEMH                                                       
         GOTO1 VALIDT                                                           
*                                  VALIDATE ADVERTISER                          
         LA    R2,AURADVH                                                       
         GOTO1 VALIADV                                                          
*                                  VALIDATE AGENCY                              
         LA    R2,AURAGYH                                                       
         GOTO1 VALIAGY                                                          
*                                  VALIDATE CONTRACT CLASS                      
         LA    R2,AURCLSH                                                       
         GOTO1 VALICLS                                                          
*                                  VALIDATE CATEGORY                            
         LA    R2,AURCATH                                                       
         GOTO1 VALICAT                                                          
*                                  VALIDATE PRODUCT                             
         LA    R2,AURPRDH                                                       
         GOTO1 VALIPRD                                                          
*                                                                               
VR60     LA    R2,AURPERH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         ST    R2,APERFLD                                                       
         BAS   RE,VALPER                                                        
         MVC   REQSTDT(4),WORK                                                  
         MVC   REQSTDT2,REQSTDT                                                 
         LA    R4,WORK+4                                                        
         USING PERVALD,R4                                                       
         GOTO1 DATCON,DMCB,(0,PVALESTA),(6,REQPSTRM)                            
         GOTO1 (RF),(R1),(0,PVALEEND),(6,REQPENDM)                              
         DROP  R4                                                               
*                                                                               
         ZIC   R1,REQSTDT2         SUBTRACT 1 YEAR FROM START DATE              
         LR    R0,R1                                                            
         SRL   R1,1                ISOLATE YEAR                                 
         BCTR  R1,0                SUBTRACT 1                                   
         SLL   R1,1                                                             
         SLL   R0,31               ISOLATE PART OF MONTH                        
         SRL   R0,31                                                            
         OR    R1,R0                                                            
         STC   R1,REQSTDT2                                                      
*                                                                               
         TM    OPTIND1,AURBYACT                                                 
         BNZ   *+8                                                              
         BAS   RE,SETDATE                                                       
*                                                                               
         LA    R2,AURADTEH         ACTIVITY PERIOD                              
         CLI   5(R2),0                                                          
         BNE   VR70                                                             
         TM    OPTIND1,AURBYACT    USING ACT DATE OPTION?                       
         BZ    VR75                 NO                                          
         MVI   ERROR,MISSING        YES - ACT DATE REQ'D                        
         B     TRAPERR                                                          
*                                                                               
VR70     ST    R2,AACTFLD                                                       
         BAS   RE,VALPER                                                        
         MVC   REQACST(4),WORK                                                  
*                                                                               
* NEED ACTIVITY DATES IN BINARY YMD                                             
         GOTO1 DATCON,DMCB,(2,WORK),(3,REQASTDT)                                
         GOTO1 (RF),(R1),(2,WORK+2),(3,REQAEDDT)                                
         GOTO1 (RF),(R1),(2,WORK),(0,REQAASTR)                                  
         GOTO1 (RF),(R1),(2,WORK+2),(0,REQAAEND)                                
*                                                                               
         TM    OPTIND1,AURBYACT    USING ACTIVITY DATE OPTION?                  
         BZ    VR75                 NO                                          
         BAS   RE,SETDATE                                                       
*                                                                               
*                                  VALIDATE STATION                             
VR75     EQU   *                                                                
         XC    GSGFILT,GSGFILT                                                  
*                                                                               
         LA    R2,AURGRPH                                                       
*                                                                               
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    VR80                NO  - NO GROUP SPECIFIED                     
         CLI   5(R2),1             SINGLE CHARACTER ENTERED?                    
         BE    VR76                YES - MUST ENTER GRP/SUBGRP                  
         CLI   8(R2),C'*'          GROUP SET NAME ENTERED?                      
         BNE   VR77                NO                                           
         GOTO1 GROUPSET            YES - VALIDATE GROUP SET                     
         BZ    VR80                ACCEPTED                                     
*                                                                               
*   SKIP STATION SETUP WHEN GROUP SET ENTERED.                                  
*                                                                               
VR76     EQU   *                                                                
         MVC   CONHEAD(L'ERRGRPS),ERRGRPS                                       
         B     TRAPERR2                                                         
ERRGRPS  DC    C'INVALID GRP/SUBGRP OR GROUP SET NAME'                          
         DS    0H                                                               
VR77     EQU   *                                                                
         GOTO1 VALIGS                                                           
         MVC   GSGFILT(2),REFGRP   INSERT INTO GROUP SET                        
*                                                                               
*   ERROR ON VALIDATION DOESN'T RETURN FROM BASE                                
*                                                                               
*                                                                               
*   SKIP STATION SETUP WHEN GROUP/SUB ENTERED.                                  
*                                                                               
VR80     EQU   *                                                                
         LA    R2,AURSTAH                                                       
         CLI   5(R2),0             ANYTHING ENTERED IN STATION?                 
         BE    VR800100            NO                                           
         OC    GSGFILT(2),GSGFILT  YES - GROUP/SUBGROUP ALSO ENTERED?           
         BZ    VR800010            NO                                           
         MVC   CONHEAD(L'ERRGRP2),ERRGRP2                                       
         B     TRAPERR2                                                         
ERRGRP2  DC    C'CANNOT ENTER BOTH GP/SUBGP AND STATION'                        
         DS    0H                                                               
VR800010 EQU   *                                                                
*                                                                               
*   STATION OPTIONS:                                                            
*        1.    STATION CALL LETTERS (GREATER THAN 2 CHARS)                      
*        2.    'K' OR 'W' - ONLY PROCESS K OR W STATIONS                        
*                           ONE-CHARACTER INPUT                                 
*        3.    'K-' OR 'W-' PROCESS ALL BUT K OR W STATIONS                     
*                           TWO-CHARACTER INPUT                                 
*                                                                               
         XC    STAFILT,STAFILT     CLEAR STATION FILTER                         
         CLI   5(R2),2             YES - TWO OR LESS CHARS?                     
         BH    VR800100            NO                                           
         BE    VR800060            TWO-CHARACTER INPUT                          
         CLI   8(R2),C'K'          ONE-CHARACTER: ONLY K STATIONS               
         BE    VR800030            YES - ACCEPT IT                              
         CLI   8(R2),C'W'          ONE-CHARACTER: ONLY W STATIONS               
         BNE   VR800090            NO  - ERROR                                  
VR800030 EQU   *                                                                
         MVC   STAFILT(1),8(R2)    SAVE FILTER REQUEST                          
         B     VR800120                                                         
VR800060 EQU   *                                                                
         CLC   8(2,R2),=C'K-'      TWO-CHARACTER: EXCLUDE K STATIONS            
         BE    VR800080            YES - ACCEPT IT                              
         CLC   8(2,R2),=C'W-'      TWO-CHARACTER: EXCLUDE W STATIONS            
         BNE   VR800090            NO  - ERROR                                  
VR800080 EQU   *                                                                
         MVC   STAFILT(2),8(R2)    SAVE FILTER REQUEST                          
         B     VR800120                                                         
VR800090 EQU   *                                                                
         MVC   CONHEAD(L'ERRFILT),ERRFILT                                       
         B     TRAPERR2                                                         
ERRFILT  DC    C'INVALID STATION FILTER'                                        
         DS    0H                                                               
VR800100 EQU   *                                                                
         GOTO1 VALISTA                                                          
VR800120 EQU   *                                                                
         OC    REFSTA,REFSTA       ALL STATION REQ?                             
         BNZ   VR82                                                             
         CLI   NMONTHS,3            YES - ONE MONTH ONLY                        
         BNH   *+12                                                             
         MVI   ERROR,INVDRNGE                                                   
         B     TRAPERR                                                          
         CLI   WHEN,X'10'          OV ONLY                                      
         BE    *+12                                                             
         MVI   ERROR,POPTNTOK                                                   
         B     TRAPERR                                                          
*                                                                               
VR82     EQU   *                                                                
         LA    R2,AURFILTH         VALIDATE FILTERS                             
         GOTO1 VALFILT                                                          
*                                                                               
         MVI   REQDPT,0            VALIDATE DAYPART                             
         LA    R2,AURDPTH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR90                 YES                                         
         CLI   AURINVH+5,0         IS THERE AN INV #?                           
         BNE   VR100                YES                                         
         CLI   AURINV2H+5,0        IS THERE AN INV #?                           
         BNE   VR100                YES                                         
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
* READ FOR DAYPART RECORD                                                       
VR90     MVC   AIO,AIO1                                                         
         LA    R4,KEY                                                           
         USING RRDPRECD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,AGENCY                                                  
         MVC   RRDPKDPT,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   REQDPT,RRDPKDPT                                                  
         DROP  R4                                                               
*                                                                               
VR100    SR    R4,R4               VALIDATE INVENTORY NUMBER                    
         LA    R3,MAXINV                                                        
         L     R5,=A(SCANBLK)                                                   
         A     R5,MYRELO           RELOCATE ADDRESS                             
         MVI   ENTRIES,0                                                        
         LA    R2,AURINVH                                                       
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         GOTO1 SCANNER,DMCB,(R2),((R3),(R5)),C',=,-'                            
         ZICM  R4,4(R1),1                                                       
         BNZ   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         SR    R3,R4               NUMBER OF ENTRIES LEFT                       
         SLA   R4,5                * 32                                         
         AR    R5,R4               R5 = NEXT ENTRY POS IN BLK                   
         SRA   R4,5                RESTORE R4                                   
*                                                                               
VR110    LA    R2,AURINV2H                                                      
         CLI   5(R2),0                                                          
         BE    VR120                                                            
         GOTO1 SCANNER,DMCB,(R2),((R3),(R5)),C',=,-'                            
         ZICM  R3,4(R1),1                                                       
         BNZ   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         AR    R4,R3               R4 = TOTAL ENTRIES                           
VR120    STC   R4,ENTRIES                                                       
*                                                                               
VR150    XC    REQISTDT(4),REQISTDT   INVENTORY PERIOD                          
         LA    R2,AURIPERH                                                      
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         BAS   RE,VALPER                                                        
         MVC   REQISTDT(4),WORK                                                 
*                                                                               
VR160    LA    R2,AURMATH          MATCH TYPE                                   
         LA    RE,MATCHTAB                                                      
         ZICM  RF,AURMATH+5,1                                                   
         BZ    VR190               DEFAULT - 1ST ENTRY                          
         BCTR  RF,0                                                             
VR170    CLI   0(RE),X'FF'                                                      
         BE    VR180                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),8(R2)                                                    
         BE    VR190                                                            
         LA    RE,9(RE)                                                         
         B     VR170                                                            
*                                                                               
VR180    MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VR190    MVC   8(8,R2),0(RE)                                                    
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
         MVC   MATCHTYP,8(RE)                                                   
*                                                                               
VR195    NI    FLAGS,X'FF'-FLSAT   SATELLITE OPTION                             
         LA    R2,AURSATH                                                       
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         CLI   8(R2),C'N'                                                       
         BE    VR200                                                            
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         OI    FLAGS,FLSAT                                                      
*                                                                               
VR200    LA    R2,AURBOOKH         VALIDATE BOOK                                
         GOTO1 VALIBOK                                                          
*                                                                               
         LA    R5,ELEM                                                          
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
*                                                                               
         LA    R2,AURDEMOH         VALIDATE DEMOS                               
         CLI   5(R2),0                                                          
         BE    VR220                                                            
*                                                                               
         TM    OPTIND1,AURDOWN     TEST FOR SPECIAL DOWNLOAD                    
         BNZ   VR202               SKIP LENGTH CHECK                            
         CLI   NMONTHS,4           NO MORE THAN 4 MONTHS/WEEKS                  
         BNH   *+12                                                             
         MVI   ERROR,INVDRNGE                                                   
         B     TRAPERR                                                          
*                                                                               
VR202    GOTO1 DEMOVAL,DMCB,(3,AURDEMOH),(20,WORK2),(0,(R5))                    
         CLI   4(R1),0                                                          
         BNE   VR210                                                            
         MVI   ERROR,INVALID                                                    
         L     R2,0(R1)            A(FIELD ERROR)                               
         B     TRAPERR                                                          
*                      ELIMINATE EOL MARKER (X'FF')                             
VR210    DS    0H                                                               
         ZIC   R2,DMCB+4                                                        
         STC   R2,NDEMS                                                         
*                                                                               
         TM    OPTIND1,AURDOWN     TEST FOR SPECIAL DOWNLOAD                    
         BNZ   VR212               SKIP NDEMS CHECK                             
         CLI   NDEMS,3                                                          
         BNH   VR212                                                            
         LA    R2,AURDEMOH         POINT TO DEMOS FIELD                         
         MVI   ERROR,TOOMUCH                                                    
         B     TRAPERR                                                          
*                                                                               
VR212    MH    R2,=H'3'                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   AURDEMS(0),WORK2                                                 
*                                  GET DEMO NAMES                               
         GOTO1 DEMOCON,DMCB,(NDEMS,AURDEMS),(2,DEMNAMES),(0,DBLOCK),0           
*                                                                               
VR220    DS    0H                                                               
         LA    R2,AURCDEMH         VALIDATE CONTRACT DEMO                       
         CLI   5(R2),0                                                          
         BE    VR240                                                            
         CLI   5(R2),1                                                          
         BNE   *+20                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   CONDEMO,C'Y'                                                     
         B     VR240                                                            
*                                                                               
         GOTO1 DEMOVAL,DMCB,(1,AURCDEMH),(1,WORK2),(C'Y',DBLOCK)                
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   CONDEMO,WORK2       STRIP OFF X'FF' EOL MARKER                   
         DROP  R5                                                               
*                                                                               
VR240    DS    0H                                                               
*                                                                               
         GOTO1 INITDRON                                                         
         GOTO1 WRAPDRON                                                         
VRECX    B     XIT                                                              
*                                                                               
MATCHTAB DC    CL8'WITHIN  ',AL1(WITHIN)     ***MUST BE 1ST ENTRY***            
         DC    CL8'OVERLAP ',AL1(OVERLAP)                                       
         DC    CL8'EXACT   ',AL1(EXACT)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* INPUT:  R2 = A(FIELD HEADER)                                                  
* OUTPUT: S/E (COMPRESSED) DATES IN WORK                                        
*         PERVAL DATA RETURNED IN WORK+4                                        
*         NOTE: PVALESTA & PVALEEND SUBJECT TO CHANGE                           
VALPER   NTR1                                                                   
         ZIC   R4,5(R2)                                                         
         GOTO1 PERVAL,DMCB,((R4),8(R2)),(0,WORK+4)                              
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R4,WORK+4                                                        
         USING PERVALD,R4                                                       
         TM    PVALASSM,PVALASD    START DAY ASSUMED?                           
         BNZ   VALP10               YES                                         
         MVC   WORK(2),PVALCSTA                                                 
         B     VALP20                                                           
*                                                                               
VALP10   MVC   PVALESTA+4(2),=C'14'  MAKE SURE IT STARTS IN B'CAST MON          
         GOTO1 QGTBROAD,DMCB,(1,PVALESTA),WORK2,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK2),(2,WORK)                                   
*                                                                               
VALP20   TM    PVALASSM,PVALAED    END DAY ASSUMED?                             
         BNZ   *+14                 YES                                         
         MVC   WORK+2(2),PVALCEND                                               
         B     VALPX                                                            
         MVC   PVALEEND+4(2),=C'16'  MAKE SURE IT ENDS IN B'CAST MON            
         GOTO1 QGTBROAD,DMCB,(1,PVALEEND),WORK2,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK2+6),(2,WORK+2)                               
*                                                                               
VALPX    B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
* BUILD TABLE OF PERIODS                                                        
SETDATE  NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,WORK),(0,WORK+4)                                  
         GOTO1 (RF),(R1),(2,WORK+2),(0,WORK+10)                                 
         XC    WORK2,WORK2                                                      
         MVC   WORK2(4),QGTBROAD                                                
         MVC   WORK2+4(4),ADDAY                                                 
         MVC   WORK2+8(4),GETDAY                                                
         MVC   WORK2+12(4),DATCON                                               
         MVI   DATETYPE,0          DEFAULT B'CAST MONTHS                        
         TM    OPTIND1,AURWEEK     WEEKLY OPTION?                               
         BZ    *+8                                                              
         MVI   DATETYPE,5                                                       
*                                                                               
         GOTO1 =V(MOBILE),DMCB,('MAXPERS+1',WORK+4),(DATETYPE,MONTAB), X        
               WORK2,0,RR=YES                                                   
         CLI   0(R1),MAXPERS                                                    
         BNH   *+12                                                             
         MVI   ERROR,INVDRNGE                                                   
         B     TRAPERR                                                          
*                                                                               
         MVC   NMONTHS,0(R1)       NUMBER OF MONTHS                             
         B     XIT                                                              
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
TRAPERR2 EQU   *                                                                
         MVI   ERROR,X'FE'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PREP     NTR1                                                                   
         BAS   RE,GETTAB           GET STORAGE TABLES                           
         BAS   RE,DRIVINIT         INITIALIZE DRIVER                            
*                                                                               
* BUILD STATION TABLE                                                           
         L     R3,ASTATAB          SET A(STATION TABLE)                         
         USING STATABD,R3                                                       
         LR    R5,R3               SET A(END OF TABLE)                          
         AH    R5,=Y(STATABSZ)     A(EOT)                                       
         ST    R3,ATHISSTA         SET A(STATION IN PROCESS)                    
         XC    KEY,KEY             CYCLE THROUGH STATION RECS                   
         LA    R2,KEY                                                           
         USING STARECD,R2                                                       
         MVI   RSTAKTYP,X'02'      INSERT STATION REC TYPE                      
         MVC   RSTAKREP,AGENCY     INSERT REP CODE                              
         MVC   RSTAKSTA,REFSTA     INSERT STATION FILTER (IF ANY)               
         GOTO1 HIGH                RETRIEVE STATION KEY                         
PREP0020 CLC   KEY(22),KEYSAVE     SAME AGY?                                    
         BNE   PREP0080            NO                                           
         LA    R2,KEY              SET DSECT TO KEY                             
         OC    STAFILT,STAFILT     ANY SPECIAL STATION FILTER?                  
         BZ    PREP0036            NO                                           
         CLI   STAFILT+1,C'-'      EXCLUDE SET?                                 
         BE    PREP0030            YES                                          
*                                                                               
*   INCLUDE ALL STATIONS BEGINNING WITH FILTER LETTER - THERE                   
*        IS NO CHARACTER IN STAFILT+1                                           
*                                                                               
         CLC   RSTAKSTA(1),STAFILT NO  - 1ST STA CHAR = FILT?                   
         BNE   PREP0070            NO  - SKIP THIS STATION                      
         B     PREP0036                                                         
PREP0030 EQU   *                                                                
*                                                                               
*   EXCLUDE ALL STATIONS BEGINNING WITH FILTER LETTER - THERE                   
*        IS A "-" CHARACTER IN STAFILT+1                                        
*                                                                               
         CLC   RSTAKSTA(1),STAFILT 1ST STA CHAR = FILT?                         
         BE    PREP0070            YES - SKIP THIS STATION                      
         B     PREP0036                                                         
PREP0036 EQU   *                                                                
         CLI   RSTAKSTA+4,C' '     TV STATION?                                  
         BE    PREP0040            YES                                          
         CLI   RSTAKSTA+4,C'T'     TV STATION?                                  
         BNE   PREP0040            NO  - PUT OUT RADIO STATION                  
         MVI   RSTAKSTA+4,C' '     YES - CLEAR OUT MEDIA FOR TV                 
PREP0040 EQU   *                                                                
         GOTO1 GETREC              RETRIEVE STATION REC                         
PREP0042 EQU   *                                                                
         L     R2,AIO              SET DSECT TO RECORD RETURNED                 
         OC    GSGFILT,GSGFILT     ANY GROUP SET ENTERED?                       
         BZ    PREP0048            NO                                           
         LA    RF,GSGFILT          SET A(START OF GROUP)                        
PREP0044 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE:                                
         BE    PREP0070            YES - REC GSG NOT IN TABLE                   
*                                     SKIP IT                                   
                                                                                
         CLC   RSTAGRUP,0(RF)      YES - REC GROUP = DESIRED GROUP?             
         BE    PREP0048            YES - ACCEPT THIS STATION                    
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     PREP0044            GO BACK FOR NEXT STATION                     
PREP0048 EQU   *                                                                
*                                  RECORD IS ACCEPTED                           
*                                                                               
         MVC   STSTA,RSTAKSTA      SAVE STATION CALLS                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        RETRIEVE DESCRIPTOR ELT                      
         BAS   RE,GETEL                                                         
         BNE   *+10                NOT FOUND(??)                                
         MVC   STMKT,RSTAMKT-RSTAELEM(R6)                                       
*                                  GET MARKET NAME                              
         LA    R3,STATABLN(R3)     BUMP TO NEXT ENTRY                           
         CR    R3,R5               END OF TABLE REACHED?                        
         BL    *+6                 NO                                           
         DC    H'0'                YES - STATAB TOO SMALL                       
PREP0060 OC    REFSTA,REFSTA       STATION FILTER ENTERED?                      
         BNZ   PREP0080            YES - PROCESS SINGLE STATION                 
PREP0070 EQU   *                                                                
         GOTO1 SEQ                 NO  - GET NEXT STATION                       
         B     PREP0020            GO BACK FOR NEXT                             
         DROP  R3                                                               
*                                                                               
PREP0080 MVI   0(R3),X'FF'         SET STATION DELIMITER EOT                    
*                                                                               
*   TEMPORARY:  KILL JOB TO CHECK TABLE                                         
*                                                                               
****     L     RF,ASTATAB                                                       
****     DC    H'0'                                                             
*                                                                               
*   TEMPORARY:  KILL JOB TO CHECK TABLE                                         
*                                                                               
*                                                                               
PREP0100 DS    0H                  IF DOING SATELLITES, DO 2 PASSES             
*                                                                               
*        POINT TO START OF STATION TABLE                                        
*                                                                               
         TM    OPTIND1,AURDOWN     IF NOT DOING SPECIAL DOWNLOAD                
         BO    *+16                                                             
         TM    FLAGS,FLSAT            IF DOING SATELLITES                       
         BNO   *+8                                                              
         OI    FLAGS,FLSATRUN            DO SATELLIES NOW                       
*                                                                               
         L     RF,ASTATAB          SET A(STATION TABLE)                         
         ST    RF,ATHISSTA         SET A(STATION IN PROCESS)                    
*                                                                               
* SET UP FOR FETCH CALL                                                         
*                                                                               
PREP0120 DS    0H                                                               
         L     RE,=A(FETCHBLK)                                                  
         A     RE,MYRELO           RELOCATE ADDRESS                             
         LA    RF,L'FETCHBLK                                                    
         XCEFL                                                                  
         L     R2,=A(FETCHBLK)                                                  
         A     R2,MYRELO           RELOCATE ADDRESS                             
         USING RFTBLKD,R2                                                       
*                                                                               
         MVC   RFTACOM,ACOMFACS                                                 
         MVC   RFTAIO1,AIO1                                                     
         L     RF,=A(AURIO)                                                     
         A     RF,MYRELO           RELOCATE ADDRESS                             
         STCM  RF,15,RFTAIO2                                                    
         L     RF,=A(FETCHWRK)                                                  
         A     RF,MYRELO           RELOCATE ADDRESS                             
         ST    RF,RFTAWRK                                                       
         LA    RF,FETCHOOK                                                      
         ST    RF,RFTHOOKA                                                      
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADER DATA                          
         OC    AURDEMS,AURDEMS     ANY DEMOS?                                   
         BZ    PREP0140                                                         
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMO DATA                            
         MVI   RFTCSRC,C'N'                                                     
         MVC   RFTCDEMS(60),AURDEMS  PASS ALL DEMOS                             
         MVC   RFTCBKS(3),REFBOOK                                               
         MVI   RFTCBKFL,RFTCBKFI                                                
         MVC   RFTCBKSV,REFBOOK+3                                               
*                                                                               
PREP0140 XC    RFTRETRN,RFTRETRN                                                
         MVC   RFTCREP,AGENCY                                                   
         L     RF,ATHISSTA                                                      
         USING STATABD,RF                                                       
         MVC   RFTCSTAT(5),STSTA                                                
****     MVI   RFTCSTAT+4,C' '                                                  
         DROP  RF                                                               
*                                                                               
*       GOTO1 =V(PRNTBL),DMCB,=C'FETCH  ',RFTCSTAT,C'DUMP',5,=C'1D',  X         
*              (C'P',MYPRINT)                                                   
*                                                                               
         TM    FLAGS,FLSATRUN      SATELLITE PASS?                              
         BNO   *+8                  NO                                          
         MVI   RFTCSTAT+4,C'1'      YES - PASS BAND AS -1                       
*                                                                               
         XC    RFTCDCTL,RFTCDCTL                                                
         XC    RFTCINV,RFTCINV                                                  
         MVC   RFTCDTDP,REQDPT                                                  
         MVC   RFTCEFST(4),REQSTDT    DEFAULT TO REQ PERIOD                     
         OC    REQISTDT(4),REQISTDT   INVENTORY PERIOD?                         
         BZ    *+10                    NO                                       
         MVC   RFTCEFST(4),REQISTDT                                             
         MVI   RFTCDTDY,0                                                       
         XC    RFTCDTST(4),RFTCDTST                                             
*                                                                               
         MVI   RFTAMODE,RFTAMSTQ   FETCH VIA MASTER RECORDS                     
         ZICM  R3,ENTRIES,1        ANY INVENTORY ENTRIES?                       
         BNZ   *+16                 NO                                          
         MVI   RFTAMODE,RFTAINVQ   FETCH VIA DAYPART RECORDS                    
         LA    R3,1                                                             
         B     PREP0180                                                         
*                                                                               
         L     R5,=A(SCANBLK)                                                   
         A     R5,MYRELO           RELOCATE ADDRESS                             
PREP0160 MVC   RFTCINV,12(R5)                                                   
         XC    RFTCINVL,RFTCINVL                                                
         CLI   1(R5),0                                                          
         BE    *+10                                                             
         MVC   RFTCINVL,22(R5)                                                  
PREP0180 L     RF,=A(FETCHBLK)                                                  
         A     RF,MYRELO           RELOCATE ADDRESS                             
         GOTO1 REFETCH,DMCB,(RF)                                                
         LA    R5,32(R5)                                                        
         BCT   R3,PREP0160                                                      
*                                                                               
* READ CONTRACTS IN REPIO                                                       
PREP0200 DS    0H                                                               
         L     RE,AINVTAB                                                       
         ZICM  RF,0(RE),1          RF = # ENTRIES                               
         BZ    PREP0460                                                         
         L     R2,=A(REPIOBLK)                                                  
         A     R2,MYRELO           RELOCATE ADDRESS                             
         XC    0(L'REPIOBLK,R2),0(R2)                                           
         USING REPIOD,R2                                                        
         MVC   RIPIOARE,AIO1                                                    
         MVC   RIPDTMGR,DATAMGR                                                 
         MVC   RIPREP,AGENCY                                                    
         MVC   RIPDATS,REQSTDT                                                  
         MVC   RIPDATE,REQEDDT                                                  
         MVC   RIPDATSS,REQSTDT2                                                
* FILTERS                                                                       
         MVC   RIPOFF,REFOFF       SALESPERSON OFFICE                           
         L     RF,ATHISSTA                                                      
         USING STATABD,RF                                                       
         MVC   RIPSTA(5),STSTA                                                  
         DROP  RF                                                               
****     MVI   RIPSTA+4,C' '                                                    
         MVC   RIPSAL,REFSAL       SALESPERSON                                  
         MVC   RIPTEAM,REFDT       DIVISION/TEAM                                
         MVC   RIPADV,REFADV       ADVERTISER                                   
         MVC   RIPAGY,REFAGY       AGENCY                                       
         MVC   RIPCAT,REFCTG       CATEGORY                                     
*                                                                               
PREP0220 OC    CONDEMO,CONDEMO     CONTRACT DEMOS REQ'D?                        
         BNZ   PREP0240             YES - ALWAYS NEED GETREC                    
         OC    REFPRD,REFPRD       ANY PRODUCT FILTER?                          
         BNZ   PREP0240             YES                                         
         OI    RIPSTAT,RIPNOGR      NO  - DON'T NEED GETREC                     
*                                                                               
PREP0240 EQU   *                                                                
         GOTO1 =V(REPIO),DMCB,A(REPIOBLK)                                       
         TM    RIPSTAT,RIPRQERR                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    RIPSTAT,RIPENDF                                                  
         BNZ   PREP0340                                                         
* FILTER ON PRODUCT - REPIO DOESN'T                                             
         L     R6,AIO1                                                          
         USING RCONREC,R6                                                       
         OC    REFPRD,REFPRD       ANY PRODUCT FILTER?                          
         BZ    *+14                 NO                                          
         CLC   RCONPRD,REFPRD      DO THEY MATCH?                               
         BNE   PREP0220             NO                                          
* FILTER ON CONTRACT DEMO (IF REQUESTED)                                        
         OC    CONDEMO,CONDEMO     ANY CON DEMOS?                               
         BZ    PREP0320             NO                                          
         CLI   CONDEMO,C'Y'        ALL DEMOS                                    
         BE    PREP0320             YES                                         
         MVI   ELCODE,X'12'        CHECK FOR SPECIFIC DEMO                      
         BAS   RE,GETEL                                                         
         BNE   PREP0220            NO SAR ELEM - SKIP BUY                       
         USING RSARXEL,R6                                                       
         TM    CONDEMO,X'40'       PRIMARY DEMO CHECK                           
         BZ    PREP0280             NO - ANY MENTION IS OK                      
         TM    PROFILES+0,X'10'    PRIMARY DEMO REQUIRED?                       
         BNZ   PREP0260             YES - FIND IT                               
         MVC   WORK(3),RSARXDEM     NO - IT'S FIRST DEMO                        
         OI    WORK,X'40'          TURN ON PRIMARY DEMO (IF OFF)                
         CLC   CONDEMO,WORK                                                     
         BNE   PREP0220                                                         
         B     PREP0320                                                         
*                                                                               
PREP0260 LA    R1,RSARXDEM                                                      
         LA    R0,8                                                             
         TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    *+16                                                             
         LA    R1,3(R1)                                                         
         BCT   R0,*-12                                                          
         B     PREP0220            SKIP IF NO PRIMARY DEMO FOUND                
         CLC   CONDEMO,0(R1)       PRIMARY DEMO                                 
         BNE   PREP0220                                                         
         B     PREP0320                                                         
*                                                                               
PREP0280 LA    R1,RSARXDEM                                                      
         LA    R0,8                                                             
PREP0300 MVC   WORK(3),0(R1)                                                    
         NI    WORK,X'FF'-X'40'    TURN OFF PRIMARY (IF ON)                     
         CLC   CONDEMO,WORK        THIS DEMO?                                   
         BE    PREP0320                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,PREP0300                                                      
         B     PREP0220            SKIP IF NO DEMO MATCH                        
         DROP  R6                                                               
*                                                                               
PREP0320 L     R5,ACONTAB                                                       
         L     RE,0(R5)            +0 = N'RECORDS IN TABLE SO FAR               
         LA    R1,1(RE)            INC # ENTRIES IN TABLE                       
         ST    R1,0(R5)                                                         
         L     R0,4(R5)            A(EOT)                                       
         LA    R5,8(R5)            RF = A(1'ST ENTRY)                           
         SLA   RE,3                N'ENTRIES * 8                                
         AR    R5,RE               RF = A(NEW ENTRY)                            
         CR    R5,R0                                                            
         BL    *+6                                                              
         DC    H'0'                CONTAB TOO SMALL                             
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
         MVO   WORK(8),RIPKEY+(RCON8CON-RCON8TYP)(4)                            
         ZAP   DUB,=P'99999999'                                                 
         SP    DUB,WORK(8)                                                      
         UNPK  WORK(8),DUB                                                      
         OI    WORK+7,X'F0'                                                     
         PACK  DUB(5),WORK(9)                                                   
         PACK  0(1,R5),DUB+3(1)    REVERSE THE COMPLEMENT                       
         PACK  1(1,R5),DUB+2(1)                                                 
         PACK  2(1,R5),DUB+1(1)                                                 
         PACK  3(1,R5),DUB(1)                                                   
         SPACE 2                                                                
* GET DEMO FROM SAR ELEMENT                                                     
         CLI   CONDEMO,C'Y'                                                     
         BE    *+14                                                             
         MVC   4(3,R5),CONDEMO                                                  
         B     PREP0220                                                         
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PREP0220            NO SAR ELEM - LEAVE DEMO BLANK               
         USING RSARXEL,R6                                                       
         MVC   4(3,R5),RSARXDEM    DEFAULT FIRST DEMO                           
         TM    PROFILES+0,X'10'    PRIMARY DEMO REQUIRED?                       
         BZ    PREP0220             NO - USE FIRST DEMO                         
         LA    R1,RSARXDEM                                                      
         LA    R0,8                                                             
*                                                                               
         TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    *+16                                                             
         LA    R1,3(R1)                                                         
         BCT   R0,*-12                                                          
         B     PREP0220            LEAVE FIRST IF NO PRIMARY FOUND              
*                                                                               
         MVC   4(3,R5),0(R1)       PRIMARY DEMO                                 
         B     PREP0220                                                         
         DROP  R6                                                               
*                                                                               
PREP0340 XC    DMCB(24),DMCB                                                    
         L     RF,ACONTAB                                                       
         ICM   RE,15,0(RF)         # RECORDS                                    
         BZ    PREP0460             NONE                                        
         LA    RF,8(RF)                                                         
         STCM  RF,7,DMCB+1         A(RECORDS)                                   
         ST    RE,DMCB+4           # RECORDS                                    
         MVI   DMCB+11,8           L'RECORD                                     
         MVI   DMCB+15,4           L'KEY                                        
         GOTO1 QSORT,DMCB                                                       
*                                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
* READ BUYS                                                                     
         MVC   AIO,AIO1                                                         
         L     R2,ACONTAB                                                       
         ICM   R0,15,0(R2)         # RECORDS                                    
         BZ    PREP0460                                                         
         LA    R2,8(R2)            A(1ST K NUM)                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),AGENCY                                                 
PREP0360 MVC   KEY+18(4),0(R2)                                                  
         GOTO1 HIGH                                                             
         B     PREP0400                                                         
PREP0380 GOTO1 SEQ                                                              
PREP0400 CLC   KEY(22),KEYSAVE                                                  
         BNE   PREP0440                                                         
         CLC   =X'FFFF',KEY+25     SKIP PLANS                                   
         BE    PREP0380                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,34(R6)                                                        
         USING RBUYELEM,R6                                                      
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BE    PREP0380             YES - SKIP BUY                              
         OC    RBUYCOS,RBUYCOS     ZERO $$$ SPOTS?                              
         BZ    PREP0380             YES - SKIP BUY                              
*                                                                               
         OC    REFLEN,REFLEN       FILTER ON LENGTH?                            
         BZ    *+14                 NO                                          
         CLC   RBUYDUR,REFLEN      DO THEY MATCH                                
         BNE   PREP0380             NO                                          
*                                                                               
         OC    REFCLS,REFCLS       FILTER ON CLASS?                             
         BZ    *+14                 NO                                          
         CLC   RBUYCLS,REFCLS      DO THEY MATCH?                               
         BNE   PREP0380             NO - SKIP BUY                               
*                                                                               
         OC    REQASTDT,REQASTDT   ACTIVITY DATES?                              
         BZ    PREP0420                                                         
         CLC   RBUYCREA,REQASTDT   BUY CREATED BEFORE ACTIVITY START?           
         BL    PREP0380             YES                                         
         CLC   RBUYCREA,REQAEDDT   BUY CREATED AFTER ACTIVITY END?              
         BH    PREP0380             YES                                         
*                                                                               
PREP0420 DS    0H                                                               
         ICM   RE,7,4(R2)                                                       
         STCM  RE,7,BUYDEMO                                                     
         BAS   RE,CKDT                                                          
         B     PREP0380            NEXT BUY                                     
*                                                                               
PREP0440 LA    R2,8(R2)                                                         
         BCT   R0,PREP0360         NEXT K                                       
*                                                                               
PREP0460 L     RF,ATHISSTA                                                      
         LA    RF,STATABLN(RF)                                                  
         ST    RF,ATHISSTA                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BE    PREP0480             YES                                         
         BAS   RE,CLRTAB           CLEAR INVTAB AND CONTAB                      
         B     PREP0120                                                         
*                                                                               
PREP0480 EQU   *                                                                
*                                                                               
PREP0500 DS    0H                  CONTINUE IF SATELLITE PASS NEEDED            
*                                                                               
         TM    OPTIND1,AURDOWN     DONE IF NOT DOING SPECIAL DOWNLOAD           
         BNO   PREP0520                                                         
*                                                                               
         TM    FLAGS,FLSAT         SKIP IF SATELLITE PASS NOT NEEDED            
         BNO   PREP0520                                                         
*                                                                               
         TM    FLAGS,FLSATRUN      DONE IF SATELLITE PASS JUST DONE             
         BO    PREP0520                                                         
*                                                                               
         OI    FLAGS,FLSATRUN      SET FOR SATELLITE RUN                        
         BAS   RE,CLRTAB           CLEAR INVTAB AND CONTAB                      
         B     PREP0100                                                         
*                                                                               
PREP0520 DS    0H                                                               
*                                                                               
         NI    FLAGS,X'FF'-FLSATRUN TURN OFF SATELLITE PASS INDICATOR           
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         DROP  R4                                                               
*                                                                               
PREP0900 BAS   RE,FREETAB          FREE UP STORAGE                              
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* CKDT:  SEE IF BUY FITS INTO ANY DAYPART DEFN IN TABLE               *         
*        EXIT CC EQ IF GOOD                                           *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
* ---------------                                                     *         
* R0 = BUY START TIME                                                 *         
* R1 = BUY END TIME                                                   *         
* R2 = LOOP COUNTER                                                   *         
* R3 = LOOP COUNTER                                                   *         
* R5 = RDA RECORD                                                     *         
* R6 = BUY X'02' ELEM (DATE ELEM)                                     *         
* RE = DAYPART DEFN START TIME                                        *         
* RF = DAYPART DEFN END TIME                                          *         
*                                                                     *         
***********************************************************************         
CKDT     NTR1                                                                   
         L     RE,AINVTAB          RESET ALL 'USED' FLAGS                       
         ZICM  RF,0(RE),1          RF = # ENTRIES                               
         BZ    CKX                                                              
         LA    RE,1(RE)                                                         
         USING INVTABD,RE                                                       
         NI    ITFLAG,X'FF'-ITUSED                                              
         LA    RE,ITLEN(RE)                                                     
         BCT   RF,*-8                                                           
         DROP  RE                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,34(R6)           R6 WILL HAVE BUY X'02'                       
*                                                                               
CK01     CLI   0(R6),X'02'                                                      
         BE    CK20                                                             
CK10     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),X'00'                                                      
         BNE   CK01                                                             
         B     CKX                                                              
         USING RBUYDYEL,R6                                                      
*                                                                               
* SET UP BUY TIMES                                                              
CK20     EQU   *                                                                
         L     R5,AINVTAB                                                       
         ZICM  R3,0(R5),1                                                       
         BZ    CKX                                                              
         LA    R5,1(R5)                                                         
         USING INVTABD,R5                                                       
*                                                                               
*   TEST PRNTB                                                                  
*                                                                               
*       GOTO1 =V(PRNTBL),DMCB,=C'INVTAB ',(R5),C'DUMP',300,=C'1D',    X         
*              (C'P',MYPRINT)                                                   
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         CLC   =C'NONE',RBUYDYT1                                                
         BE    CK10                TRY NEXT DAY/TIME EL                         
         CLC   =C'VARY',RBUYDYT1                                                
         BE    CK10                TRY NEXT DAY/TIME EL                         
*                                                                               
         ZICM  R0,RBUYDYT1,2                                                    
*                                                                               
         CH    R0,=H'0500'                                                      
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
*                                                                               
         ZICM  R1,RBUYDYT2,2                                                    
         BZ    *+16                DON'T ADD IF NO END TIME                     
         CH    R1,=H'0500'                                                      
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
*                                                                               
CK30     TM    ITFLAG,ITUSED       PREV USED BY THIS BUY?                       
         BNZ   CK60                 YES - DON'T USE AGAIN                       
*                                                                               
         MVC   MASK,ITDAY                                                       
         TM    MATCHTYP,EXACT      WANT EXACT MATCH?                            
         BZ    CK32                 YES                                         
         CLC   ITDAY,RBUYDAYS                                                   
         BE    CK40                                                             
         B     CK60                NO EXACT MATCH ON DAYS                       
*                                                                               
CK32     TM    MATCHTYP,WITHIN                                                  
         BZ    CK34                                                             
         NC    MASK,RBUYDAYS                                                    
         CLC   MASK,RBUYDAYS                                                    
         BE    CK40                                                             
         B     CK60                BUY NOT COMPLETELY WITHIN PER                
*                                                                               
CK34     TM    MATCHTYP,OVERLAP                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         NC    MASK,RBUYDAYS                                                    
         OC    MASK,MASK                                                        
         BZ    CK60                NO DAYS OVERLAP                              
*                                                                               
CK40     DS    0H                                                               
         ZICM  RE,ITSTTM,2                                                      
         ZICM  RF,ITENTM,2                                                      
*                                                                               
         CH    RE,=H'0500'         ADJUST FOR 5AM B'CAST DAY START              
         BNL   *+8                                                              
         AH    RE,=H'2400'                                                      
         CH    RF,=H'0500'                                                      
         BNL   *+8                                                              
         AH    RF,=H'2400'                                                      
*                                                                               
         TM    MATCHTYP,EXACT                                                   
         BZ    CK42                                                             
         OR    R1,R1               SINGLE TIME?                                 
         BZ    CK41                                                             
         CR    R0,RE               BUY START EQ INV START?                      
         BNE   CK60                 NO                                          
         CR    R1,RF               BUY END EQ INV END?                          
         BE    MATCH                YES                                         
         B     CK60                 NO                                          
*                                                                               
CK41     CR    R0,RE               SINGLE TIME EQ INV START?                    
         BE    MATCH                YES                                         
         CR    R0,RF               SINGLE TIME EQ INV END?                      
         BE    MATCH                YES                                         
         B     CK60                                                             
*                                                                               
CK42     TM    MATCHTYP,WITHIN                                                  
         BZ    CK44                                                             
         CR    R0,RE               BUY START LT DAYPART START?                  
         BL    CK60                 YES                                         
         OR    R1,R1               SINGLE TIME?                                 
         BNZ   *+10                 NO                                          
         CR    R0,RF               BUY START LT DAYPART END?                    
         BH    CK60                 YES                                         
         CR    R1,RF               BUY END GT DAYPART END?                      
         BH    CK60                 YES                                         
         B     MATCH                                                            
*                                                                               
CK44     TM    MATCHTYP,OVERLAP                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CR    R0,RE               BUY START LT DAYPT START?                    
         BL    CK50                 YES - SEE IF END TIME GT START TIME         
         SH    RF,=H'15'           SUB 1 QUARTER HOUR FROM END TIME             
         CR    R0,RF               IS BUY START LT/EQ DAYPT END?                
         BNH   MATCH                                                            
         B     CK60                                                             
*                                                                               
CK50     AH    RE,=H'15'           ADD 1 QUARTER HOUR TO START TIME             
         OR    R1,R1               SINGLE TIME?                                 
         BNZ   *+10                 NO                                          
         CR    R0,RE               START TIME GT/EQ DAYPART START               
         BNL   MATCH                                                            
         CR    R1,RE               END TIME GT/EQ START TIME?                   
         BNL   MATCH                                                            
*                                                                               
CK60     LA    R5,ITLEN(R5)                                                     
         BCT   R3,CK30             NEXT DAYPART DEFN                            
         B     CK10                TRY NEXT DAY/TIME EL                         
*                                                                               
* SET UP & CALL DRIVER FOR INPUT                                                
MATCH    DS    0H                                                               
         ST    R5,AITENT                                                        
         ST    R6,ABUYELEM                                                      
         OC    ITNUM2AD,ITNUM2AD   ANY INVTAB2 ADDRESS?                         
         BNZ   MATCH020            YES - DON'T RESET                            
*                                                                               
*   CALCULATE NEXT INVTAB2 ADDRESS, INSERT INTO ENTRY                           
*                                                                               
                                                                                
         L     RF,AINVTAB2         YES - SET A(INVTAB2)                         
         CLI   0(RF),0             ANY ENTRY IN TABLE ALREADY?                  
         BNZ   MATCH010            YES - CALCULATE NEXT ENTRY                   
         MVI   0(RF),1             NO  - SET TO 'YES', USE                      
*                                     1ST ENTRY SLOT                            
         LA    RF,3(RF)            SKIP FLAG BYTE + COUNTER                     
         ST    RF,ITNUM2AD         SAVE A(THIS INVTAB2)                         
         ST    RF,AITENTHI         SAVE HIGH TABLE ADDRESS                      
         B     MATCH020                                                         
MATCH010 EQU   *                                                                
         L     RF,AITENTHI         SET A(HIGHEST ENTRY)                         
         LA    RF,ITLEN2(RF)       ADD L(ENTRY)                                 
         ST    RF,ITNUM2AD         SAVE A(THIS INVTAB2)                         
         ST    RF,AITENTHI         SAVE HIGH TABLE ADDRESS                      
*                                                                               
MATCH020 EQU   *                                                                
         MVC   AITENT2,ITNUM2AD    SET A(2ND INVENTORY TABLE)                   
*                                                                               
*                                                                               
*   TEST PRINT                                                                  
*        ST    R1,SVR1             SAVE/RESTORE R1                              
*        XC    TESTWORK,TESTWORK                                                
*        MVC   TESTWORK+1(04),=C'ITN='                                          
*        MVC   TESTWORK+05(4),ITNUM2AD                                          
*        MVC   TESTWORK+10(04),=C'ATN='                                         
*        MVC   TESTWORK+15(4),AITENT                                            
*        MVC   TESTWORK+20(04),=C'AT2='                                         
*        MVC   TESTWORK+25(4),AITENT2                                           
*       GOTO1 =V(PRNTBL),DMCB,=C'MATCH20',TESTWORK,C'DUMP',32,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*        L     R1,SVR1             SAVE/RESTORE R1                              
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
         BAS   RE,CALLDRIV         CAN'T UPSET R1...                            
         B     CK60                THEN SEE IF IT FITS ANOTHER DAYPART          
*                                                                               
CKX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
CALLDRIV NTR1                                                                   
         MVC   WORK+00(4),QGTBROAD                                              
         MVC   WORK+04(4),GETDAY                                                
         MVC   WORK+08(4),ADDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         GOTO1 =V(REGENBUC),DMCB,AIO,BUCKETS,WORK                               
         CLC   BUCKETS(2),=H'2'    ANY DATA?                                    
         BE    XIT                  NO                                          
         OI    ITFLAG,ITUSED       SET PREVIOUSLY USED FLAG                     
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
*                                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* FETCH HOOK                                                          *         
***********************************************************************         
         SPACE 1                                                                
FETCHOOK NTR1                                                                   
         L     R5,=A(FETCHBLK)     ADDRESS FETCH BLOCK                          
         A     R5,MYRELO           RELOCATE ADDRESS                             
         USING RFTBLKD,R5                                                       
*                                                                               
*   TEST DAYPART                                                                
*        MVC   WORK(08),=C'DAYPART:'                                            
*        MVC   WORK+10(10),RFTCDTMS                                             
*        MVI   WORK+12,C'/'                                                     
*        MVC   WORK+22(10),RFTFDTM                                              
*        DC    H'0'                                                             
*   TEST DAYPART END                                                            
*                                                                               
         TM    RFTMODE,RFTNINVQ    IS THIS AN INVENTORY?                        
         BZ    FETCH10              NO                                          
         CLI   RFTFINV+3,0         OLD STYLE INVENTORY REC?                     
         BE    FETCHX               YES - SKIP IT                               
*                                                                               
*   LOW-LEVEL INVENTORY TABLE:  GROWS THROUGHOUT PROGRAM RUN                    
*                                                                               
         L     RE,AINVTAB2                                                      
         CLC   1(2,RE),=X'FFFF'    COUNTER AT MAX?                              
         BNE   *+6                                                              
         DC    H'0'                NEED BIGGER INVTAB2: 64K NOT ENUF?           
*                                                                               
         ZICM  R1,2(RE),2          GET N'ENTRIES                                
         LA    RF,1(R1)            NEW # ENTRIES                                
         STCM  RF,3,2(RE)          SAVE TWO CHARACTERS                          
         LA    RE,2(RE)            BUMP TO A(1ST ENTRY IN TABLE)                
         MH    R1,=Y(ITLEN2)       MULTIPLY BY L(ENTRY)                         
         AR    RE,R1               SET DISPLACEMENT INTO TABLE                  
         USING INVTAB2D,RE                                                      
         ST    RE,ATHISIN2                                                      
         STCM  RF,3,ITNUM2         SAVE TWO-CHARACTER COUNT                     
*                                                                               
         DROP  RE                                                               
*                                                                               
*   STATION HIGH-LEVEL INVENTORY TABLE:  REFRESHED ON STATION CHG               
*                                                                               
         L     RE,AINVTAB                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                NEED BIGGER INVTAB                           
*                                                                               
         ZIC   R1,0(RE)            GET N'ENTRIES                                
         LA    RF,1(R1)            NEW # ENTRIES                                
         STC   RF,0(RE)                                                         
         LA    RE,1(RE)                                                         
         MH    R1,=Y(ITLEN)                                                     
         AR    RE,R1                                                            
         USING INVTABD,RE                                                       
         ST    RE,ATHISINV                                                      
         STC   RF,ITNUM                                                         
*                                                                               
         MVC   ITDPT,RFTFDPTS                                                   
         MVC   ITDAY(5),RFTFDTMS                                                
*                                                                               
*   TEST                                                                        
*        LA    R5,RFTFHDR                                                       
*       GOTO1 =V(PRNTBL),DMCB,=C'INVENT ',(R5),C'DUMP',012,=C'1D',    X         
*              (C'P',MYPRINT)                                                   
*        L     RE,ATHISINV                                                      
*   TEST END                                                                    
*                                                                               
*                                                                               
         DROP  RE                                                               
*                                                                               
* IF AVAIL DATE/TIME IS THERE, USE IT IF IT'S VALID                             
         OC    RFTFAVLS,RFTFAVLS   ANY AVAIL DAY/TIME?                          
         BZ    FETCH08              NO                                          
*                                                                               
         LA    RE,11                                                            
         LA    RF,RFTFAVDY                                                      
         ST    RF,DMCB                                                          
         LA    RF,10(RF)                                                        
         CLI   0(RF),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RE,DMCB             # CHARS IN DAY STRING                        
         GOTO1 DAYVAL,DMCB,,BYTE,WORK                                           
         CLI   BYTE,0              IS DAY VALID?                                
         BE    FETCH08              NO                                          
*                                                                               
         LA    RE,11                                                            
         LA    RF,RFTFAVTM                                                      
         ST    RF,DMCB                                                          
         LA    RF,10(RF)                                                        
         CLI   0(RF),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RE,DMCB             # CHARS IN TIME STRING                       
         GOTO1 TIMVAL,DMCB,,FULL                                                
         CLI   0(R1),X'FF'         IS TIME VALID?                               
         BE    FETCH08              NO                                          
*                                                                               
         L     RE,ATHISINV                                                      
*                                                                               
         USING INVTABD,RE                                                       
*                                                                               
         MVC   ITDAY,BYTE                                                       
         MVC   ITSTTM(4),FULL                                                   
*                                                                               
FETCH08  DS    0H                                                               
         L     RE,ATHISINV                                                      
         MVC   ITPGM,RFTFPGMS                                                   
         MVC   ITINV,RFTFINV                                                    
         MVC   ITINVDT,RFTFEFST                                                 
*                                                                               
         DROP  RE                                                               
*                                                                               
         L     RE,ATHISIN2                                                      
         USING INVTAB2D,RE                                                      
         XC    ITHIGHT,ITHIGHT                                                  
         MVC   ITLOWT,=X'7FFFFFFF'                                              
         XC    ITHIGH(MAXPERS*L'ITHIGH),ITHIGH                                  
         LA    R0,MAXPERS                                                       
         LA    R1,ITLOW                                                         
*                                                                               
         MVC   0(4,R1),=X'7FFFFFFF'  MAXINT                                     
         LA    R1,L'ITLOW(R1)                                                   
         BCT   R0,*-10                                                          
         B     FETCHX                                                           
*                                                                               
         DROP  RE                                                               
*                                                                               
*                                                                               
FETCH10  TM    RFTMODE,RFTNBKQ     IS THIS A NEW BOOK?                          
         BZ    FETCHX               NO                                          
         L     RE,ATHISINV                                                      
         USING INVTABD,RE                                                       
         MVC   ITDEMOS(80),RFTFDEMS                                             
*                                                                               
*                                                                               
         DROP  RE                                                               
*                                                                               
FETCHX   EQU   *                                                                
*                                                                               
*                                                                               
*   TEST PRNTB                                                                  
*                                                                               
*        L     R5,ATHISINV                                                      
*       GOTO1 =V(PRNTBL),DMCB,=C'INVTAB ',(R5),C'DUMP',300,=C'1D',    X         
*              (C'P',MYPRINT)                                                   
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET AND FREE STORAGE FOR TABLES                                     *         
***********************************************************************         
GETTAB   NTR1                                                                   
         CLI   OFFLINE,C'Y'        OFFLINE ONLY                                 
         BNE   GETTX                                                            
         L     R2,=A(BUFFSIZE)     GET STORAGE FOR TABLE                        
         A     R2,MYRELO           RELOCATE ADDRESS                             
         ST    R2,DMCB+4                                                        
         ST    R2,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET',RR=MYRELO                                 
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         LR    RE,RF                                                            
         ST    RE,MCUSRDMP         GET BUFFER IN DUMP                           
         LA    RE,0(R2,RE)                                                      
         ST    RE,MCUSRDMP+4                                                    
         DROP  R1                                                               
*                                                                               
         ST    RF,AINVTAB                                                       
         MVI   0(RF),0             SET COUNT TO ZERO                            
*                                                                               
         A     RF,=A(INVTABSZ)     INCREMENT BY L(TABLE)                        
         ST    RF,AINVTAB2                                                      
         MVI   0(RF),0             SET COUNT TO ZERO                            
*                                                                               
         A     RF,=A(INVTB2SZ)     INCREMENT BY L(TABLE)                        
         ST    RF,ACONTAB                                                       
         LR    R1,RF                                                            
         A     R1,=A(CONTABSZ)                                                  
         XC    0(4,RF),0(RF)       +0 = N'RECORDS IN TABLE SO FAR               
         ST    R1,4(RF)            +4 = A(END OF TABLE)                         
*                                                                               
         A     RF,=A(CONTABSZ)                                                  
         ST    RF,ASTATAB                                                       
*                                                                               
GETTX    B     XIT                                                              
*                                                                               
*                                                                               
FREETAB  NTR1                                                                   
         L     R2,=A(BUFFSIZE)     FREE STORAGE FOR TABLE                       
         A     R2,MYRELO           RELOCATE ADDRESS                             
         ST    R2,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'FREE',AINVTAB,RR=MYRELO                        
FREEX    B     XIT                                                              
*                                                                               
* CLEAR INVENTORY & CONTRACT TABLE                                              
CLRTAB   NTR1                                                                   
         L     RE,AINVTAB                                                       
         L     RF,=A(INVTABSZ)                                                  
         XCEFL                                                                  
         L     RE,ACONTAB                                                       
         L     RF,=A(CONTABSZ)                                                  
         XCEFL                                                                  
         L     RF,ACONTAB                                                       
         LR    R1,RF                                                            
         A     R1,=A(CONTABSZ)                                                  
         XC    0(4,RF),0(RF)       +0 = N'RECORDS IN TABLE SO FAR               
         ST    R1,4(RF)            +4 = A(END OF TABLE)                         
CLRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR DRIVER                                           *         
***********************************************************************         
         SPACE 1                                                                
DRIVINIT NTR1  ,                                                                
         GOTO1 CALLOV,DMCB,(X'60',0),0    LOAD DPG PHASE                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADPGPROG,0(R1)                                                   
         CLI   DOWNOPT,0                                                        
         BNE   DRIN10                                                           
         MVI   WIDEOPT,C'Y'        WIDE REPORT                                  
         MVI   LEFTOPT,C'Y'                                                     
         MVI   MYFIRSTH,10         FIRST HEADING ON LINE 10                     
*                                                                               
DRIN10   DS    0H                                                               
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVC   GLOPTS+3(1),NMONTHS  NUMBER OF MONTHS                            
         MVC   GLOPTS+4(1),NDEMS   NUMBER OF DEMOS                              
*                                                                               
**>>     OC    REFSTA,REFSTA       IF ALL STA REQ, MUST BE STREAMLINED          
**>>     BNZ   *+8                                                              
**>>     OI    OPTIND1,AURSHORT                                                 
*                                                                               
         TM    OPTIND1,AURSHORT    STREAMLINED REPORT?                          
         BZ    *+8                  NO                                          
         MVI   GLOPTS+5,C'Y'                                                    
*                                                                               
         OC    CONDEMO,CONDEMO     REPORT CONTRACT DEMOS?                       
         BZ    *+8                                                              
         MVI   GLOPTS+6,C'Y'                                                    
*                                                                               
         TM    OPTIND1,AURDOWN     TEST FOR SPECIAL DOWNLOAD                    
         BZ    *+8                                                              
         MVI   GLOPTS+7,C'Y'       SET DOWNLOAD FORMAT                          
*                                                                               
         LA    R1,DRHOOK           DRIVER'S APPLICATION HOOK                    
         ST    R1,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   NTR1                                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPUTSRT     ABOUT TO PUT A RECORD TO SORT                
         BE    PUTSORT                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
GROUPSET NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
         LA    R3,KEY                                                           
         USING RSETRECD,R3                                                      
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,=C'GS'     GROUP/SUBGROUP SET                           
         MVC   RSETKID,9(R2)       TAKE NAME AFTER '*'                          
         OC    RSETKID,=C'    '    OR IN SPACES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   GSET0900            NO  - RETURN CC NOT ZERO                     
         GOTO1 GETREC              RETRIEVE RECORD                              
         L     R3,AIO              SET DSECT TO AIO AREA                        
         USING RSETRECD,R3                                                      
         CLI   RSET1DES,X'01'      BASIC DESCRIPTOR ELEMENT?                    
         BNE   GSET0020            NO  - NOT PRESENT                            
*                                                                               
*   OLDER RECORDS DON'T HAVE AN X'01' ELEMENT.  THESE CAN'T BE                  
*        SET OF SETS, SO FOLLOWING TEST IS SKIPPED.                             
*                                                                               
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BO    GSET0900            YES - DON'T PERMIT                           
GSET0020 EQU   *                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GSET0900            NO MEMBERS:  DON'T PERMIT                    
         USING RSETMEMD,R6                                                      
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SH    RF,=H'4'            SUB CODE/LEN/MEM LEN/1 FOR EX                
         EX    RF,GSET0060         MOVE MEMBERS BY LENGTH                       
         B     GSET0080                                                         
GSET0060 EQU   *                                                                
         MVC   GSGFILT(0),3(R6)    MOVE BY LENGTH                               
GSET0080 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     GSET0960            EXIT CC ZERO                                 
GSET0900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
GSET0960 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RESOLVE ROUTINE ADDRESSES                                           *         
***********************************************************************         
         SPACE 1                                                                
RESOLVE  LA    R1,ROUTLIST         TEST ROUTINE IN THIS OVERLAY                 
RESOLVE2 CLI   0(R1),FF                                                         
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
*                                                                               
RESOLVEX B     DRHOOKX                                                          
         SPACE 2                                                                
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    CL8'INUM    ',AL4(INUM)                                          
         DC    CL8'ONUM    ',AL4(ONUM)                                          
         DC    CL8'INUM2   ',AL4(INUM2)                                         
         DC    CL8'ONUM2   ',AL4(ONUM2)                                         
         DC    CL8'IDPTCD  ',AL4(IDPTCD)                                        
         DC    CL8'ODPTCD  ',AL4(ODPTCD)                                        
         DC    CL8'IDAYTM  ',AL4(IDAYTM)                                        
         DC    CL8'ODAYTM  ',AL4(ODAYTM)                                        
         DC    CL8'IDAY    ',AL4(IDAY)                                          
         DC    CL8'ODAY    ',AL4(ODAY)                                          
         DC    CL8'IPROG   ',AL4(IPROG)                                         
         DC    CL8'OPROG   ',AL4(OPROG)                                         
         DC    CL8'IINV    ',AL4(IINV)                                          
         DC    CL8'OINV    ',AL4(OINV)                                          
         DC    CL8'IBUY    ',AL4(IBUY)                                          
         DC    CL8'IBUYTWO ',AL4(IBUYTWO)                                       
         DC    CL8'OBUY    ',AL4(OBUY)                                          
         DC    CL8'OBUYSPT ',AL4(OBUYSPT)                                       
         DC    CL8'OBUYCPS ',AL4(OBUYCPS)                                       
         DC    CL8'OBUYHI  ',AL4(OBUYHI)                                        
         DC    CL8'OBUYLO  ',AL4(OBUYLO)                                        
         DC    CL8'OBUYGRS ',AL4(OBUYGRS)                                       
         DC    CL8'HBUY    ',AL4(HBUY)                                          
         DC    CL8'IDEM    ',AL4(IDEM)                                          
         DC    CL8'ODEM    ',AL4(ODEM)                                          
         DC    CL8'ODEMDEM ',AL4(ODEMDEM)                                       
         DC    CL8'ODEMCPS ',AL4(ODEMCPS)                                       
         DC    CL8'ODEMHI  ',AL4(ODEMHI)                                        
         DC    CL8'ODEMLO  ',AL4(ODEMLO)                                        
         DC    CL8'ODEMGRS ',AL4(ODEMGRS)                                       
         DC    CL8'HDEM    ',AL4(HDEM)                                          
         DC    CL8'IBUYDEM ',AL4(IBUYDEM)                                       
         DC    CL8'OBUYDEM ',AL4(OBUYDEM)                                       
         DC    CL8'IREP    ',AL4(IREP)                                          
         DC    CL8'ISAT    ',AL4(ISAT)                                          
         DC    CL8'ISTA    ',AL4(ISTA)                                          
         DC    CL8'IMKT    ',AL4(IMKT)                                          
         DC    CL8'IINVDT  ',AL4(IINVDT)                                        
         DC    CL8'OINVDT  ',AL4(OINVDT)                                        
         DC    CL8'OBUYLSP ',AL4(OBUYLSP)                                       
         DC    CL8'OBUYHSP ',AL4(OBUYHSP)                                       
         DC    CL8'IXTEST  ',AL4(IXTEST)                                        
         DC    CL8'OXTEST  ',AL4(OXTEST)                                        
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                        *         
***********************************************************************         
         SPACE 1                                                                
PRINT    EQU   *                                                                
***>>>                                                                          
         TM    OPTIND1,AURDOWN     DOWNLOAD REQUEST?                            
         BZ    PRNT0100            NO  - SKIP 'REQUEST DETAILS' O/P             
         CLI   FIRSTYM,C'Y'        FIRST TIME THROUGH?                          
         BNE   PRNT0100            NO                                           
         MVI   FIRSTYM,C'N'        YES - SET OFF                                
         GOTO1 DATCON,DMCB,(5,TESTWORK),(X'20',TODAYDAT)                        
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P(08),=C'"AURHDR"'                                               
         LA    RE,P+9                                                           
         MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
         MVC   1(6,RE),TODAYDAT    INSERT DATE OF JOB                           
         MVI   7(RE),C'"'          INSERT FIELD END   INDICATOR                 
         LA    RE,9(RE)            BUMP TO NEXT POSITION                        
         MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
         MVC   1(2,RE),AGENCY      INSERT REP CODE                              
         MVI   3(RE),C'"'          INSERT FIELD END INDICATOR                   
         LA    RE,5(RE)            BUMP TO NEXT FIELD                           
         MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         L     RF,APERFLD          SET A(PERIOD FIELD)                          
         ZIC   R5,5(RF)            GET LENGTH OF INPUT                          
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,PRNTMOVE         MOVE PERIOD BY LENGTH                        
         LA    R5,1(R5)            ADD -1 BACK                                  
         AR    RE,R5                                                            
         MVI   0(RE),C'"'          INSERT FIELD END INDICATOR                   
         LA    RE,2(RE)            POSITION TO NEXT FIELD                       
         MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
         LA    RF,AURADTEH         ACTIVITY PERIOD                              
         CLI   5(RF),0             ANYTHING IN FIELD?                           
         BNE   PRNT0020            YES                                          
         MVI   1(RE),C' '          NO  - SPACE FILL                             
         MVI   2(RE),C'"'          INSERT FIELD END   INDICATOR                 
         LA    RE,4(RE)            BUMP TO NEXT FIELD                           
         B     PRNT0040                                                         
PRNT0020 EQU   *                                                                
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         ZIC   R5,5(RF)            GET LENGTH OF INPUT                          
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,PRNTMOVE         MOVE BY LENGTH                               
         LA    R5,1(R5)            ADD -1 BACK                                  
         AR    RE,R5                                                            
         MVI   0(RE),C'"'          INSERT FIELD END INDICATOR                   
         LA    RE,2(RE)            POSITION TO NEXT FIELD                       
PRNT0040 EQU   *                                                                
         MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
         LA    RF,AURBOOKH         BOOK FIELD                                   
         CLI   5(RF),0             ANYTHING IN FIELD?                           
         BNE   PRNT0060            YES                                          
         MVI   1(RE),C' '          NO  - SPACE FILL                             
         MVI   2(RE),C'"'          INSERT FIELD END   INDICATOR                 
         LA    RE,4(RE)            BUMP TO NEXT FIELD                           
         MVI   0(RE),X'5E'         INSERT SEMICOLON AT EOL                      
         B     PRNT0080                                                         
PRNT0060 EQU   *                                                                
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         ZIC   R5,5(RF)            GET LENGTH OF INPUT                          
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,PRNTMOVE         MOVE BY LENGTH                               
         LA    R5,1(R5)            ADD -1 BACK                                  
         AR    RE,R5                                                            
         MVI   0(RE),C'"'          INSERT FIELD END INDICATOR                   
         MVI   1(RE),C' '                                                       
         MVI   2(RE),X'5E'         INSERT SEMICOLON AT EOL                      
***      LA    RE,2(RE)            POSITION TO NEXT FIELD                       
***      MVI   0(RE),C'"'          INSERT FIELD START INDICATOR                 
PRNT0080 EQU   *                                                                
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
         B     DRHOOKX                                                          
***      B     PRNT0100                                                         
*                                                                               
PRNTMOVE MVC   0(0,RE),8(RF)       MOVE DATE BY LENGTH                          
*                                                                               
PRNT0100 EQU   *                                                                
***>>>                                                                          
         TM    FLAGS,FLPRTIT       PRINT THIS LINE?                             
         BZ    PRNT0110             NO  - SET TO DEFAULT: DON'T PRINT           
         NI    FLAGS,X'FF'-FLPRTIT  YES - RESET FLAG TO DEFAULT                 
         B     DRHOOKX              GO PRINT LINE                               
PRNT0110 EQU   *                                                                
         MVI   GLHOOK,GLDONT                                                    
         L     RE,GLAP1                                                         
         LA    R0,4                                                             
         MVC   0(198,RE),BLANKS                                                 
         LA    RE,198(RE)                                                       
         BCT   R0,*-10                                                          
         B     DRHOOKX                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                        *         
***********************************************************************         
         SPACE 1                                                                
PUTSORT  TM    FLAGS,FLSORT        ANY VALID DATA?                              
         BZ    *+12                 NO                                          
         NI    FLAGS,X'FF'-FLSORT                                               
         B     DRHOOKX                                                          
*                                                                               
         MVI   GLHOOK,GLDONT                                                    
         B     DRHOOKX                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* EXECUTING ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXEC     LA    R1,ROUTLIST         TEST ROUTINE IS IN THIS OVERLAY              
*                                                                               
EXEC2    CLI   0(R1),FF                                                         
         BE    EXECX                                                            
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     EXEC2                                                            
         L     R2,GLAIFLD          YES-R2=A(INPUT)                              
         L     R3,GLAOFLD              R3=A(OUTPUT)                             
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC4                                                            
*                                                                               
EXEC4    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
*                                                                               
EXECX    B     DRHOOKX                                                          
EXECIX   B     DRHOOKX             INPUT ROUTINE COMMON EXIT                    
EXECOX   B     DRHOOKX             OUTPUT ROUTINE COMMON EXIT                   
         EJECT                                                                  
***********************************************************************         
* DRIVER INPUT ROUTINES                                               *         
***********************************************************************         
INUM     L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(1,R2),ITNUM                                                    
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
INUM2    L     R5,AITENT           SET A(BASIC ITEM)                            
         USING INVTABD,R5                                                       
         MVC   0(4,R2),ITNUM2AD    SAVE ADDRESS INVTAB2 ENTRY                   
*                                                                               
*   TEST PRNTB                                                                  
*                                                                               
*        L     R3,ASPOOLD                                                       
*        USING SPOOLD,R3                                                        
*        MVC   P+1(05),=C'INUM2'                                                
*        GOTO1 HEXOUT,DMCB,ITNUM2AD,P+10,4,=C'TOG'                              
*        ZICM  RF,ITNUM2AD,4                                                    
*        MVC   P+20(70),0(RF)                                                   
*        GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
*        DROP  R3                                                               
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IDPTCD   L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(6,R2),ITDPT                                                    
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IDAYTM   L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(4,R2),ITINV                                                    
         MVC   4(5,R2),ITDAY                                                    
         MVC   9(27,R2),ITPGM                                                   
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IDAY     L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(5,R2),ITDAY                                                    
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IPROG    L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(27,R2),ITPGM                                                   
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IINV     L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(4,R2),ITINV                                                    
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
IINVDT   L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         MVC   0(4,R2),ITINVDT                                                  
         B     EXECIX                                                           
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
IXTEST   EQU   *                                                                
         B     EXECIX                                                           
*                                                                               
OXTEST   EQU   *                                                                
         B     EXECOX                                                           
*                                                                               
IBUY     DS    0H                                                               
         MVI   MAINFLAG,X'00'      SET FLAG OFF                                 
         B     IBUY0020                                                         
*                                                                               
IBUYTWO  EQU    *                                                               
         MVI   MAINFLAG,X'01'      SET FLAG ON                                  
*                                                                               
IBUY0020 EQU    *                                                               
*                                                                               
*  GET PERIOD FOR THIS COLUMN                                                   
         CLI   GLARGS,255                                                       
         BNE   IBUY0040                                                         
         LA    R6,REQACST          USE ACTIVITY PER                             
         TM    OPTIND1,AURBYACT    IF REPORTING BY ACTIVITY                     
         BNZ   IBUY0060                                                         
*                                                                               
* FOR CORRECT B'CAST YYMM DATES, WANT END OF FIRST PERIOD (NOT REQSTDT)         
* TO END OF LAST PERIOD (SAME AS REQEDDT)                                       
         GOTO1 DATCON,DMCB,(2,MONTAB+2),(3,WORK2)                               
         GOTO1 (RF),(R1),(2,REQEDDT),(3,WORK2+3)                                
         B     IBUY0080                                                         
*                                                                               
IBUY0040 ZIC   R6,GLARGS           PERIOD NUMBER                                
         BCTR  R6,0                                                             
         SLA   R6,2                *4                                           
         LA    R6,MONTAB(R6)                                                    
*                                                                               
* GET B'CAST S/E YMD IN WORK2                                                   
IBUY0060 GOTO1 DATCON,DMCB,(2,0(R6)),(3,WORK2)                                  
         GOTO1 (RF),(R1),(2,2(R6)),(3,WORK2+3)                                  
*                                                                               
IBUY0080 TM    OPTIND1,AURBYACT    USE TOTALS IF BY ACTIVITY DATE               
         BZ    IBUY0100                                                         
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         CLC   RBUYCREA,WORK2      CREATED BEFORE B'CAST START?                 
         BL    IBUY0560                                                         
         CLC   RBUYCREA,WORK2+3    CREATED AFTER B'CAST END?                    
         BH    IBUY0560                                                         
         MVC   HALF,RBUYTSPT                                                    
         LH    R1,HALF                                                          
         ST    R1,0(R2)                                                         
         MVC   4(4,R2),RBUYTCOS                                                 
         B     IBUY0200                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
IBUY0100 DS    0H                                                               
         LA    R6,BUCKETS                                                       
         LA    R6,2(R6)            R6=A(1ST BUCKET)                             
         SR    R0,R0                                                            
*                                                                               
IBUY0120 CLI   GLARGS,255          TOTAL COLUMN?                                
         BNE   IBUY0140             NO                                          
         CLC   2(2,R6),WORK2       WITHIN  B'CAST PER?                          
         BL    IBUY0160                                                         
         CLC   2(2,R6),WORK2+3                                                  
         BH    IBUY0200                                                         
         B     IBUY0180                                                         
IBUY0140 CLC   2(2,R6),WORK2+3     BUCKET IN B'CAST YM?                         
         BE    IBUY0180                                                         
         BH    IBUY0200            NO BUCKET FOR THIS PER                       
*                                                                               
IBUY0160 IC    R0,1(R6)            NEXT BUCKET                                  
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   IBUY0120                                                         
         B     IBUY0200                                                         
*                                                                               
IBUY0180 ICM   RF,15,4(R2)         RUNNING TOTAL                                
         A     RF,6(R6)            BUCKET AMOUNT                                
         STCM  RF,15,4(R2)                                                      
         CLI   GLARGS,255          THIS A TOTAL COL?                            
         BNE   IBUY0200             NO - ONLY ONE BUCKET                        
         B     IBUY0160                                                         
*                                                                               
IBUY0200 L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         NI    FLAGS,X'FF'-FLNEG                                                
         TM    RBUYCOS,X'80'       NEGATIVE?                                    
         BZ    IBUY0210            NO  -                                        
         OI    FLAGS,FLNEG         YES - SET NEGATIVE FLAG                      
         B     IBUY0320            DON'T APPLY AGAINST HIGH/LOW                 
*                                     TAKES CARE OF CREDITS ALSO                
IBUY0210 EQU   *                                                                
*                                                                               
* GET HIGHS AND LOWS FOR PERIOD                                                 
         OC    4(4,R2),4(R2)       IF NO $$ IN PER, NO HIGH & LOW               
         BZ    IBUY0320                                                         
*                                                                               
         L     R5,AITENT2          HIGHS & LOWS KEPT IN INVTAB                  
         USING INVTAB2D,R5                                                      
*                                                                               
         CLI   GLARGS,255          TOTAL COL?                                   
         BNE   IBUY0260             NO                                          
**>><<**                                                                        
*                                                                               
*   TEST PRINT                                                                  
*                                                                               
*        L     RF,AIO                                                           
*        MVC   TESTWORK+0(12),RBUYKCON-RBUYREC(RF)                              
*        MVC   TESTWORK+13(04),ITHIGHT                                          
*        MVC   TESTWORK+18(04),RBUYCOS                                          
*       GOTO1 =V(PRNTBL),DMCB,=C'BUYCOS ',TESTWORK,C'DUMP',32,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
**>><<**                                                                        
         CLC   ITHIGHT,RBUYCOS     NEW PRICE GT STORED HIGH PRICE?              
         BNL   IBUY0220            NO  - LEAVE                                  
         MVC   ITHIGHT,RBUYCOS     YES - INSERT NEW HIGH PRICE                  
         XC    ITHISPT,ITHISPT     CLEAR HI PRICE # SPOTS TOT                   
IBUY0220 EQU   *                                                                
         CLC   ITLOWT,RBUYCOS      NEW PRICE LT STORED LOW  PRICE?              
         BNH   IBUY0240            NO  - LEAVE                                  
         MVC   ITLOWT,RBUYCOS      YES - INSERT NEW LOW  PRICE                  
         XC    ITLOSPT,ITLOSPT     CLEAR LO PRICE # SPOTS TOT                   
IBUY0240 EQU   *                                                                
         B     IBUY0320                                                         
*                                                                               
IBUY0260 ZIC   RF,GLARGS           GET PERIOD                                   
         BCTR  RF,0                                                             
         SLA   RF,2                *4                                           
         LR    R1,RF               SAVE DISPLACEMENT                            
         LA    RF,ITHIGH(RF)                                                    
         CLC   0(4,RF),RBUYCOS                                                  
         BNL   IBUY0280                                                         
         MVC   0(4,RF),RBUYCOS                                                  
         LR    RF,R1               RESET DISPLACEMENT                           
         LA    RF,ITHISPOT(RF)     DISPLACE TO SPOT COUNT                       
         XC    0(4,RF),0(RF)       CLEAR SPOT COUNT                             
IBUY0280 EQU   *                                                                
         LA    RF,ITLOW(R1)                                                     
         CLC   0(4,RF),RBUYCOS                                                  
         BNH   IBUY0300                                                         
         MVC   0(4,RF),RBUYCOS                                                  
         LR    RF,R1               RESET DISPLACEMENT                           
         LA    RF,ITLOSPOT(RF)     DISPLACE TO SPOT COUNT                       
         XC    0(4,RF),0(RF)       CLEAR SPOT COUNT                             
IBUY0300 EQU   *                                                                
         DROP  R5,R6                                                            
*                                                                               
* CALCULATE NUMBER OF SPOTS IN B'CAST MON                                       
IBUY0320 TM    OPTIND1,AURBYACT                                                 
         BNZ   IBUY0560            USES TOTAL SPOTS, NOT SPTS/WEEK              
*                                                                               
* MAY NEED TO RESET PER START FOR TOTAL COL                                     
         CLI   GLARGS,255          TOTAL COL?                                   
         BNE   IBUY0340             NO                                          
         GOTO1 DATCON,DMCB,(2,REQSTDT),(3,WORK2)                                
*                                                                               
IBUY0340 L     R6,AIO                                                           
         USING RBUYDTEL,R6                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         MVC   TESTWORK+52(06),=C'GETEL '                                       
         B     IBUY0380                                                         
IBUY0360 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         MVC   TESTWORK+52(06),=C'NEXTEL'                                       
IBUY0380 EQU   *                                                                
         BNE   IBUY0560                                                         
         MVC   WORK(3),RBUYDTST                                                 
         MVC   WEEKIND,=F'7'                                                    
         TM    RBUYDTIN,X'40'      EVERY OTHER WEEK?                            
         BZ    IBUY0400                                                         
         MVC   WEEKIND,=F'14'                                                   
IBUY0400 EQU   *                                                                
         CLC   WORK(3),WORK2       BUY EFF STDT < B'CAST START?                 
         BL    IBUY0540             YES - NEXT WEEK                             
         CLC   WORK(3),WORK2+3     BUY EFF STDT > B'CAST END?                   
         BH    IBUY0360             YES - WE'RE DONE                            
*                                                                               
*   TEST PRINT                                                                  
*                                                                               
*                                                                               
*        L     R5,AITENT           HIGHS & LOWS KEPT IN INVTAB                  
*        USING INVTABD,R5                                                       
*                                                                               
*        L     RF,AIO                                                           
*        MVC   TESTWORK+0(12),RBUYKCON-RBUYREC(RF)                              
*        MVC   TESTWORK+13(11),RBUYDTEL                                         
*        MVC   TESTWORK+25(27),ITPGM SET PROGRAM NAME                           
*        CLC   =C'TALK TALK',ITPGM CONCENTRATE ON 'TALK TALK'                   
*        BNE   TPRT0010                                                         
*                                                                               
*        CLI   GLARGS,255          TOTAL COL?                                   
*        BNE   TPRT0010             NO                                          
*       GOTO1 =V(PRNTBL),DMCB,=C'GETEL  ',TESTWORK,C'DUMP',64,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*                                                                               
*        XC    TESTWORK+52(06),TESTWORK+52                                      
*        DROP  R5                                                               
*                                                                               
TPRT0010 EQU   *                                                                
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
*                                                                               
         L     R1,0(R2)                                                         
         ZIC   R0,RBUYDTNW                                                      
         TM    FLAGS,FLNEG         MINUS BUY?                                   
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         AR    R1,R0                                                            
         ST    R1,0(R2)                                                         
*                                                                               
* ACCUMULATE # SPOTS FOR HIGH/LOW PRICE IN INVTAB.                              
*                                                                               
         TM    MAINFLAG,X'01'      ACCUMULATE THIS LEVEL?                       
         BO    IBUY0540            NO                                           
         L     R5,AITENT2          HIGHS & LOWS KEPT IN INVTAB                  
         USING INVTAB2D,R5                                                      
*                                                                               
         CLI   GLARGS,255          TOTAL COL?                                   
         BNE   IBUY0480             NO                                          
*                                  YES                                          
         L     RF,AIO              SET A(BUYREC)                                
         CLC   ITHIGHT,RBUYCOS-RBUYREC(RF)                                      
*                                  HIGH COST TOTAL = THIS COST?                 
         BNE   IBUY0440            NO  - DON'T ACCUMULATE SPOTS                 
         L     R1,ITHISPT          YES - ACCUMULATE HIGH SPOT COUNT             
         AR    R1,R0               ADD SPOTS                                    
         ST    R1,ITHISPT          SAVE IT BACK                                 
*                                                                               
*   TEST PRINT                                                                  
*                                                                               
*        MVC   TESTWORK+0(04),ITHIGHT                                           
*        MVC   TESTWORK+4(04),RBUYCOS-RBUYREC(RF)                               
*        STCM  R0,15,TESTWORK+08                                                
*        MVC   TESTWORK+12(04),ITHISPT                                          
*        MVC   TESTWORK+17(12),RBUYKCON-RBUYREC(RF)                             
*        MVC   TESTWORK+32(27),ITPGM SET PROGRAM NAME                           
*        MVC   TESTWORK+61(1),GLARGS                                            
*        CLC   =C'TALK TALK',ITPGM CONCENTRATE ON 'TALK TALK'                   
*        BNE   TPRT0020                                                         
*                                                                               
*       GOTO1 =V(PRNTBL),DMCB,=C'HIACCUM',TESTWORK,C'DUMP',64,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*                                                                               
TPRT0020 EQU   *                                                                
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
*                                  NOW CHECK FOR LOW ALSO                       
*                                     1ST ORD SATISFIES HIGH AND LOW            
IBUY0440 EQU   *                                                                
         L     RF,AIO              SET A(BUYREC)                                
         CLC   ITLOWT,RBUYCOS-RBUYREC(RF)                                       
*                                  LOW  COST TOTAL = THIS COST?                 
         BNE   IBUY0460            NO  - DON'T ACCUMULATE SPOTS                 
         L     R1,ITLOSPT          YES - ACCUMULATE LOW  SPOT COUNT             
         AR    R1,R0               ADD SPOTS                                    
         ST    R1,ITLOSPT          SAVE IT BACK                                 
*                                                                               
*   TEST PRINT                                                                  
*        MVC   TESTWORK+0(04),ITLOSPT                                           
*        MVC   TESTWORK+4(04),RBUYCOS-RBUYREC(RF)                               
*        STCM  R0,15,TESTWORK+08                                                
*        MVC   TESTWORK+12(04),ITLOSPT                                          
*        MVC   TESTWORK+17(12),RBUYKCON-RBUYREC(RF)                             
*        MVC   TESTWORK+32(27),ITPGM SET PROGRAM NAME                           
*        MVC   TESTWORK+61(1),GLARGS                                            
*        CLC   =C'TALK TALK',ITPGM CONCENTRATE ON 'TALK TALK'                   
*        BNE   TPRT0040                                                         
*                                                                               
*       GOTO1 =V(PRNTBL),DMCB,=C'LOACCUM',TESTWORK,C'DUMP',64,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*                                                                               
TPRT0040 EQU   *                                                                
*                                                                               
*   TEST PRINT END                                                              
*                                                                               
*                                  SET BUY NUMBER AS ACCUMULATED.               
         B     IBUY0520                                                         
IBUY0460 EQU   *                                                                
         B     IBUY0520                                                         
IBUY0480 EQU   *                                                                
         ZIC   RF,GLARGS           GET PERIOD                                   
         BCTR  RF,0                                                             
         SLA   RF,2                *4                                           
         LR    R1,RF               SAVE DISPLACEMENT                            
         LA    RF,ITHIGH(RF)       A(HIGH PRICE FOR MONTH)                      
         L     RE,AIO              SET A(BUYREC)                                
         CLC   0(4,RF),RBUYCOS-RBUYREC(RE)                                      
*                                  HIGH PRICE = THIS BUY PRICE?                 
         BNE   IBUY0500            NO  - CHECK LOW PRICE                        
         LA    RF,ITHISPOT(R1)     A(HIGH SPOT COUNT FOR MONTH)                 
         L     RE,0(RF)            UNLOAD PRESENT VALUE                         
         AR    RE,R0               ADD SPOTS TO LOW COUNT                       
         ST    RE,0(RF)            REPLACE VALUE IN ARRAY                       
*                                  STILL NEED TO CHECK IF PRICE IS              
*                                     LOW:  FIRST TIME, WILL                    
*                                     BE BOTH HIGH AND LOW                      
IBUY0500 EQU   *                                                                
         LA    RF,ITLOW(R1)        A(LOW PRICE FOR MONTH)                       
         L     RE,AIO              SET A(BUYREC)                                
         CLC   0(4,RF),RBUYCOS-RBUYREC(RE)                                      
*                                  LOW  PRICE = THIS BUY PRICE?                 
         BNE   IBUY0520            NO  - DON'T ACCUMULATE                       
         LA    RF,ITLOSPOT(R1)     A(LOW SPOT COUNT FOR MONTH)                  
         L     RE,0(RF)            UNLOAD PRESENT VALUE                         
         AR    RE,R0               ADD SPOTS TO LOW COUNT                       
         ST    RE,0(RF)            REPLACE VALUE IN ARRAY                       
         B     IBUY0520                                                         
IBUY0520 EQU   *                                                                
         DROP  R5                                                               
*                                                                               
* BUMP EFFECTIVE START DATE TO NEXT WEEK                                        
*                                                                               
IBUY0540 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+3)                                  
         GOTO1 ADDAY,DMCB,WORK+3,WORK+3,WEEKIND                                 
         GOTO1 DATCON,DMCB,(0,WORK+3),(3,WORK)                                  
         CLC   WORK(3),RBUYDTED    STDT > EDDT (DATES WHOLLY                    
*                                     WITHIN REQUEST PERIOD?)                   
         BH    IBUY0360            YES                                          
         CLC   WORK(3),WORK2+3     NO  - STDT > REQUEST PERIOD END?             
         BH    IBUY0360            YES                                          
*                                                                               
*   TEST PRNTB                                                                  
*        L     R5,AITENT           HIGHS & LOWS KEPT IN INVTAB                  
*        USING INVTABD,R5                                                       
*                                                                               
*        CLC   =C'TALK TALK',ITPGM CONCENTRATE ON 'TALK TALK'                   
*        BNE   TPRT0060                                                         
*        DROP  R5                                                               
*                                                                               
*        MVC   WEEKINDA(4),WEEKIND                                              
*        MVC   WEEKINDA+4(3),WORK                                               
*        MVC   WEEKINDA+7(3),RBUYDTED                                           
*                                                                               
*        PRINT GEN                                                              
*       GOTO1 =V(PRNTBL),DMCB,=C'WEEKIND',WEEKINDA,C'DUMP',16,=C'1D', X         
*              (C'P',MYPRINT)                                                   
*        L     RF,WEEKIND                                                       
*        PRINT NOGEN                                                            
*                                                                               
TPRT0060 EQU   *                                                                
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         B     IBUY0400                                                         
*                                                                               
IBUY0560 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*       GOTO1 =V(PRNTBL),DMCB,=C'EXIT   ',TESTWORK,C'DUMP',3,=C'1D',  X         
*              (C'P',MYPRINT)                                                   
*   TEST END                                                                    
*                                                                               
         L     R5,AITENT           HIGHS & LOWS KEPT IN INVTAB                  
         USING INVTABD,R5                                                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         OC    0(4,R2),0(R2)                                                    
         BZ    EXECIX                                                           
         OI    FLAGS,FLSORT        THERE IS DATA - PUT TO SRT                   
         B     EXECIX                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
IDEM     L     R5,AITENT                                                        
         USING INVTABD,R5                                                       
         ZIC   R1,GLARGS+1                                                      
         BCTR  R1,0                                                             
         SLA   R1,2                *4                                           
         LA    R1,ITDEMOS(R1)                                                   
         MVC   0(4,R2),0(R1)                                                    
         MVI   7(R2),1                                                          
         LA    R2,8(R2)                                                         
         B     IBUYTWO             SET 'DONT TOTAL SPOTS' FLAG                  
         DROP  R5                                                               
         SPACE 2                                                                
IBUYDEM  DS    0H                                                               
         MVC   0(3,R2),=X'FFFFFF'                                               
         OC    BUYDEMO,BUYDEMO                                                  
         BZ    EXECIX                                                           
         MVC   0(3,R2),BUYDEMO                                                  
         B     EXECIX                                                           
*                                                                               
IREP     DS    0H                                                               
         MVC   0(2,R2),AGENCY                                                   
         B     EXECIX                                                           
*                                                                               
ISAT     DS    0H                  FLAG SATELLITE STATIONS                      
*                                                                               
         MVI   0(R2),C' '          DEFAULT TO NON-SATELLITE                     
*                                                                               
         TM    FLAGS,FLSATRUN      IF SATELLITES BEING PROCESSED                
         BNO   *+8                                                              
         MVI   0(R2),C'S'             SET FLAG                                  
*                                                                               
         B     EXECIX                                                           
*                                                                               
ISTA     DS    0H                                                               
         L     RF,ATHISSTA                                                      
         USING STATABD,RF                                                       
         MVC   0(5,R2),STSTA                                                    
         DROP  RF                                                               
         B     EXECIX                                                           
*                                                                               
IMKT     DS    0H                                                               
         L     RF,ATHISSTA                                                      
         USING STATABD,RF                                                       
         MVC   0(20,R2),STMKT                                                   
         DROP  RF                                                               
         B     EXECIX                                                           
         EJECT                                                                  
***********************************************************************         
* DRIVER OUTPUT ROUTINES                                              *         
***********************************************************************         
*                                                                               
ONUM     L     RE,AINVTAB                                                       
         LA    RE,1(RE)            RE=A(1ST ENTRY)                              
         ZIC   R1,0(R2)            GET ENTRY NUMBER WANTED                      
         BCTR  R1,0                                                             
         MH    R1,=Y(ITLEN)                                                     
         AR    RE,R1                                                            
         ST    RE,AITENT                                                        
*                                                                               
*   TEST PRNTB                                                                  
*                                                                               
*      GOTO1 =V(PRNTBL),DMCB,=C'INVTAB ',AITENT,C'DUMP',300,=C'1D',   X         
*              (C'P',MYPRINT)                                                   
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         B     EXECOX                                                           
*                                                                               
ONUM2    EQU   *                                                                
**       L     RE,AINVTAB2                                                      
**       LA    RE,1(RE)            RE=A(1ST ENTRY)                              
         ZICM  RE,0(R2),4          GET A(ENTRY OF TABLE)                        
**       BCTR  R1,0                                                             
**       MH    R1,=Y(ITLEN2)                                                    
**       AR    RE,R1                                                            
         ST    RE,AITENT2                                                       
*                                                                               
*   TEST PRNTB                                                                  
*                                                                               
*        L     R3,ASPOOLD                                                       
*        USING SPOOLD,R3                                                        
*        MVC   P+1(05),=C'ONUM2'                                                
*        MVC   TESTWORK(4),AITENT2                                              
*        GOTO1 HEXOUT,DMCB,TESTWORK,P+10,4,=C'TOG'                              
*        ZICM  RF,TESTWORK,4                                                    
*        MVC   P+20(70),0(RF)                                                   
*        GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
*        DROP  R3                                                               
*                                                                               
*   TEST PRNTB  END                                                             
*                                                                               
         B     EXECOX                                                           
*                                                                               
ODPTCD   EQU   *                                                                
**       MVC   OUTAREA,BLANKS                                                   
**       MVC   LABLAREA(8),=C'DPT CODE'                                         
**       MVC   CODEAREA(1),0(R2)                                                
**       MVC   0(L'OUTAREA,R3),OUTAREA                                          
         MVC   0(1,R3),0(R2)       INSERT DAYPART CODE                          
         B     EXECOX                                                           
*                                                                               
ODAYTM   DS    0H                                                               
         MVC   OUTAREA,BLANKS                                                   
         GOTO1 UNDAY,DMCB,4(R2),0(R3)                                           
         LA    R6,7(R3)                                                         
         CLI   0(R6),C' '                                                       
         BH    *+10                                                             
         BCTR  R6,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R6),C'/'                                                       
         GOTO1 UNTIME,DMCB,5(R2),2(R6)                                          
         MVC   198(27,R3),9(R2)                                                 
         MVC   396(4,R3),0(R2)                                                  
         TM    OPTIND1,AURSHORT                                                 
         BZ    *+12                                                             
         MVI   594(R3),0           FORCE A BLANK LINE                           
         B     EXECOX                                                           
         MVI   990(R3),0           FORCE A BLANK LINE                           
         B     EXECOX                                                           
*                                                                               
ODAY     DS    0H                                                               
         GOTO1 UNDAY,DMCB,0(R2),0(R3)                                           
         LA    R6,7(R3)                                                         
         CLI   0(R6),C' '                                                       
         BH    *+10                                                             
         BCTR  R6,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R6),C'/'                                                       
         GOTO1 UNTIME,DMCB,1(R2),2(R6)                                          
         B     EXECOX                                                           
*                                                                               
OPROG    DS    0H                                                               
         MVC   0(27,R3),0(R2)                                                   
         B     EXECOX                                                           
*                                                                               
OINV     DS    0H                                                               
         MVC   0(4,R3),0(R2)                                                    
         B     EXECOX                                                           
*                                                                               
OINVDT   DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'12',0(R2)),(5,0(R3))                              
         B     EXECOX                                                           
*                                                                               
         EJECT                                                                  
ANYSPOTS OC    0(4,R2),0(R2)       ANY SPOTS?                                   
         BZR   RE                  NO  - ZERO IS DEFAULT NO-PRINT               
****     BNZR  RE                  YES - (OLD = PRINT)                          
         OI    FLAGS,FLPRTIT       YES - PRINT                                  
*        SR    R0,R0                                                            
         LTR   RB,RB               SET CC = NOT ZERO                            
         BR    RE                                                               
*                                                                               
OBUY     DS    0H                                                               
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
*                                                                               
         LR    R6,R3                                                            
*                                                                               
         TM    OPTIND1,AURSHORT    ONLY AVG CPS FOR STREAMLINED REPORT          
         BNZ   OBUY2                                                            
*                                  NUMBER OF SPOTS                              
         EDIT  (4,0(R2)),(11,0(R6))                                             
*                                  AVG COST PER SPOT                            
         LA    R6,198(R3)                                                       
OBUY2    L     RE,4(R2)            GROSS COST                                   
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,0(R2)            /TOTAL SPOTS                                 
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                =AVG COST/SPOT                               
         EDIT  (RF),(11,0(R6)),2                                                
         TM    OPTIND1,AURSHORT    ONLY AVG CPS FOR STREAMLINED REPORT          
         BNZ   OBUYX                                                            
*                                  GET HIGH/LOW RATES                           
         L     RE,AITENT2                                                       
         USING INVTAB2D,RE                                                      
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITHIGHT                                                    
         B     OBUY20                                                           
*                                                                               
         ZIC   RF,GLARGS           GET PERIOD NUMBER                            
         BCTR  RF,0                                                             
         SLA   RF,2                                                             
         LR    R1,RF                                                            
         LA    RF,ITHIGH(RF)                                                    
         L     RF,0(RF)                                                         
*                                  HIGHEST RATE                                 
OBUY20   LA    R6,198(R6)                                                       
         LTR   RF,RF                                                            
         BZ    OBUY25               LEAVE $0 FIELDS BLANK                       
         EDIT  (RF),(11,0(R6)),2                                                
*                                                                               
OBUY25   LA    R6,198(R6)                                                       
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITLOWT                                                     
         B     OBUY30                                                           
*                                                                               
         LA    RF,ITLOW(R1)                                                     
         L     RF,0(RF)                                                         
*                                  LOWEST RATE                                  
OBUY30   C     RF,=X'7FFFFFFF'                                                  
         BE    OBUY35                                                           
         EDIT  (RF),(11,0(R6)),2                                                
         DROP  RE                                                               
*                                  GROSS $                                      
OBUY35   LA    R6,198(R6)                                                       
         OC    4(4,R2),4(R2)       LEAVE $0 FIELDS BLANK                        
         BE    OBUYX                                                            
         EDIT  (4,4(R2)),(11,0(R6)),2                                           
*                                                                               
OBUYX    B     EXECOX                                                           
         EJECT                                                                  
*                                                                               
OBUYSPT  DS    0H                  # SPOTS                                      
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
         EDIT  (4,0(R2)),(6,0(R3))                                              
         B     EXECOX                                                           
*                                                                               
OBUYCPS  DS    0H                  AVG COST PER SPOT                            
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
         L     RE,4(R2)            GROSS COST                                   
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,0(R2)            /TOTAL SPOTS                                 
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                =AVG COST/SPOT                               
         EDIT  (RF),(11,0(R3)),2                                                
         B     EXECOX                                                           
*                                                                               
OBUYHI   DS    0H                  GET HIGH RATE                                
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
         L     RE,AITENT2                                                       
         USING INVTAB2D,RE                                                      
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITHIGHT                                                    
         B     OBUYHI2                                                          
*                                                                               
         ZIC   RF,GLARGS           GET PERIOD NUMBER                            
         BCTR  RF,0                                                             
         SLA   RF,2                                                             
         LR    R1,RF                                                            
         LA    RF,ITHIGH(RF)                                                    
         L     RF,0(RF)                                                         
         DROP  RE                                                               
*                                  HIGHEST RATE                                 
OBUYHI2  LTR   RF,RF                                                            
         BZ    EXECOX               LEAVE $0 FIELDS BLANK                       
         EDIT  (RF),(11,0(R3)),2                                                
         B     EXECOX                                                           
*                                                                               
OBUYLO   DS    0H                                                               
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
         L     RE,AITENT2                                                       
         USING INVTAB2D,RE                                                      
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITLOWT                                                     
         B     OBUYLO2                                                          
*                                                                               
         ZIC   RF,GLARGS           GET PERIOD NUMBER                            
         BCTR  RF,0                                                             
         SLA   RF,2                                                             
         LR    R1,RF                                                            
         LA    RF,ITLOW(R1)                                                     
         L     RF,0(RF)                                                         
         DROP  RE                                                               
*                                  LOWEST RATE                                  
OBUYLO2  C     RF,=X'7FFFFFFF'                                                  
         BE    EXECOX                                                           
         EDIT  (RF),(11,0(R3)),2                                                
         B     EXECOX                                                           
*                                                                               
OBUYGRS  DS    0H                  GROSS $                                      
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
         OC    4(4,R2),4(R2)       LEAVE $0 FIELDS BLANK                        
         BE    OBUYX                                                            
         EDIT  (4,4(R2)),(11,0(R3)),2                                           
         B     EXECOX                                                           
*                                                                               
*                                                                               
OBUYLSP  DS    0H                  LOW PRICE SPOTS                              
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
**>>**                                                                          
         L     RE,AITENT2                                                       
         USING INVTAB2D,RE                                                      
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITLOSPT       EXTRACT #SPTS/LO PRICE                       
         B     OBUYLS2                                                          
*                                                                               
         ZIC   RF,GLARGS           GET PERIOD NUMBER                            
         BCTR  RF,0                                                             
         SLA   RF,2                                                             
         LR    R1,RF                                                            
         LA    RF,ITLOSPOT(RF)                                                  
         L     RF,0(RF)                                                         
         DROP  RE                                                               
*                                  LOWEST  RATE # SPOTS                         
OBUYLS2  EQU   *                                                                
*        LTR   RF,RF                                                            
*        BZ    EXECOX               LEAVE $0 FIELDS BLANK                       
         EDIT  (RF),(05,0(R3))                                                  
         B     EXECOX                                                           
*                                                                               
**>>**                                                                          
*                                                                               
*                                                                               
OBUYHSP  DS    0H                  LOW PRICE SPOTS                              
         BAS   RE,ANYSPOTS                                                      
         BZ    OBUYX                                                            
**>>**                                                                          
         L     RE,AITENT2                                                       
         USING INVTAB2D,RE                                                      
         CLI   GLARGS,255                                                       
         BNE   *+12                                                             
         ICM   RF,15,ITHISPT       EXTRACT #SPTS/HI PRICE                       
         B     OBUYHS2                                                          
*                                                                               
         ZIC   RF,GLARGS           GET PERIOD NUMBER                            
         BCTR  RF,0                                                             
         SLA   RF,2                                                             
         LR    R1,RF                                                            
         LA    RF,ITHISPOT(RF)                                                  
         L     RF,0(RF)                                                         
         DROP  RE                                                               
*                                  HIGHEST RATE # SPOTS                         
OBUYHS2  EQU   *                                                                
*        LTR   RF,RF                                                            
*        BZ    EXECOX               LEAVE $0 FIELDS BLANK                       
         EDIT  (RF),(05,0(R3))                                                  
         B     EXECOX                                                           
*                                                                               
**>>**                                                                          
*                                                                               
         EJECT                                                                  
ANYDEM   DS    0H                                                               
         OC    8(4,R2),8(R2)       ANY DEMOS?                                   
         BZR   RE                   NO                                          
         ICM   RF,15,0(R2)                                                      
         BR    RE                                                               
*                                                                               
ODEM     DS    0H                                                               
         BAS   RE,ANYDEM                                                        
         BZ    ODEMX               NO SPOTS/DEMOS - NO PRINT                    
         LR    R6,R3                                                            
*                                                                               
*   OUTPUT DEMO                                                                 
         TM    OPTIND1,AURSHORT    ONLY AVG CPS FOR STREAMLINED REPORT          
         BNZ   ODEM2                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   R1,15,4(R2)         GET # LINES                                  
         BZ    ODEMX                                                            
         DR    RE,R1               RF = DEM VALUE                               
         TM    OPTIND1,DEMROUND    DEMO AS WHOLE NUMBER?                        
         BZ    ODEM10               NO                                          
*                                                                               
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(11,0(R6))                                                  
         M     RE,=F'10'                                                        
         LTR   RF,RF                                                            
         BZ    ODEMX                                                            
         B     ODEM20                                                           
*                                                                               
ODEM10   EDIT  (RF),(11,0(R6)),1                                                
ODEM20   LA    R6,198(R6)                                                       
*                                                                               
*   OUTPUT CPP OF AVG CPS                                                       
ODEM2    L     R0,12(R2)           GROSS COST                                   
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,8(R2)            /TOTAL SPOTS                                 
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                =AVG COST/SPOT                               
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R6)),2                                                
*                                                                               
         TM    OPTIND1,AURSHORT    ONLY AVG CPS FOR STREAMLINED REPORT          
         BNZ   ODEMX                                                            
*                                                                               
         LA    R6,198(R6)                                                       
*                                                                               
*   OUTPUT CPP OF HIGH                                                          
         L     R5,AITENT2                                                       
         USING INVTAB2D,R5                                                      
         L     R1,ITHIGHT                                                       
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R6)),2                                                
         LA    R6,198(R6)                                                       
*                                                                               
*   OUTPUT CPP OF LOW                                                           
         CLC   ITLOWT,=X'7FFFFFFF'     NO LOW WILL CAUSE OVERFLOW               
         BE    ODEM30                                                           
         L     R1,ITLOWT                                                        
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R6)),2                                                
ODEM30   LA    R6,198(R6)                                                       
         DROP  R5                                                               
*                                                                               
*   OUTPUT CPP OF GROSS                                                         
         L     R1,12(R2)           GROSS COST                                   
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R6)),2                                                
ODEMX    B     EXECOX                                                           
*                                                                               
         EJECT                                                                  
ROUND    DS    0H                                                               
         LR    R0,RE                                                            
         SR    RE,RE                                                            
         ICM   R1,15,4(R2)         GET # LINES                                  
         BZ    ROUNDX                                                           
         DR    RE,R1               RF = DEM VALUE                               
         TM    OPTIND1,DEMROUND    DEMO AS WHOLE NUMBER?                        
         BZ    ROUNDNZ              NO                                          
*                                                                               
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         M     RE,=F'10'                                                        
         LTR   RF,RF                                                            
         BZ    ROUNDX                                                           
ROUNDNZ  LTR   RE,R0                                                            
         BR    RE                                                               
ROUNDX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ODEMDEM  BAS   RE,ANYDEM                                                        
         BZ    ODEMX                                                            
         SR    RE,RE                                                            
         ICM   R1,15,4(R2)         GET # LINES                                  
         BZ    ODEMX                                                            
         DR    RE,R1               RF = DEM VALUE                               
         TM    OPTIND1,DEMROUND    DEMO AS WHOLE NUMBER?                        
         BZ    ODD10                NO                                          
*                                                                               
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(11,0(R3))                                                  
         M     RE,=F'10'                                                        
         B     EXECOX                                                           
*                                                                               
ODD10    EDIT  (RF),(11,0(R3)),1                                                
         B     EXECOX                                                           
*                                                                               
*   OUTPUT CPP OF AVG CPS                                                       
ODEMCPS  DS    0H                                                               
         BAS   RE,ANYDEM                                                        
         BZ    EXECOX                                                           
         BAS   RE,ROUND                                                         
         BZ    EXECOX                                                           
*                                                                               
         L     R0,12(R2)           GROSS COST                                   
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,8(R2)            /TOTAL SPOTS                                 
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                =AVG COST/SPOT                               
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R3)),2                                                
         B     EXECOX                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*   OUTPUT CPP OF HIGH                                                          
ODEMHI   DS    0H                                                               
         BAS   RE,ANYDEM                                                        
         BZ    EXECOX                                                           
         BAS   RE,ROUND                                                         
         BZ    EXECOX                                                           
         L     R5,AITENT2                                                       
         USING INVTAB2D,R5                                                      
         L     R1,ITHIGHT                                                       
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R3)),2                                                
         B     EXECOX                                                           
         DROP  R5                                                               
*                                                                               
*   OUTPUT CPP OF LOW                                                           
ODEMLO   DS    0H                                                               
         BAS   RE,ANYDEM                                                        
         BZ    EXECOX                                                           
         BAS   RE,ROUND                                                         
         BZ    EXECOX                                                           
         L     R5,AITENT2                                                       
         USING INVTAB2D,R5                                                      
         CLC   ITLOWT,=X'7FFFFFFF'     NO LOW WILL CAUSE OVERFLOW               
         BE    ODEM30                                                           
         L     R1,ITLOWT                                                        
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R3)),2                                                
         B     EXECOX                                                           
         DROP  R5                                                               
*                                                                               
*   OUTPUT CPP OF GROSS                                                         
ODEMGRS  DS    0H                                                               
         BAS   RE,ANYDEM                                                        
         BZ    EXECOX                                                           
         BAS   RE,ROUND                                                         
         BZ    EXECOX                                                           
         L     R1,12(R2)           GROSS COST                                   
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(11,0(R3)),2                                                
         B     EXECOX                                                           
*                                                                               
         EJECT                                                                  
OBUYDEM  DS    0H                                                               
         MVC   0(7,R3),=C'NO DEMO'                                              
         CLC   0(3,R2),=X'FFFFFF'                                               
         BE    OBDEMX                                                           
*                                                                               
         LA    R6,ELEM                                                          
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON (FROM RECNT45)             
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,DMCB,(1,(R2)),(2,0(R3)),(C'S',DBLOCK),0                  
OBDEMX   B     EXECOX                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER HEADER ROUTINES                                              *         
***********************************************************************         
*                                                                               
HBUY     DS    0H                                                               
         ZIC   R6,GLARGS           GET PERIOD NUMBER                            
         BCTR  R6,0                                                             
         SLA   R6,2                *4                                           
         LA    R6,MONTAB(R6)                                                    
         MVI   DATETYPE,5                                                       
         TM    OPTIND1,AURWEEK                                                  
         BNZ   *+12                                                             
         MVI   DATETYPE,6                                                       
         LA    R6,2(R6)            USE END DATE OF MON PERS                     
         GOTO1 DATCON,DMCB,(2,(R6)),(DATETYPE,0(R3))                            
         B     EXECX                                                            
*                                                                               
HDEM     DS    0H                                                               
         ZIC   R6,GLARGS           DEMO NUMBER                                  
         BCTR  R6,0                                                             
         MH    R6,=H'7'            *6                                           
         LA    R6,DEMNAMES(R6)                                                  
         MVC   0(7,R3),0(R6)                                                    
HDEMX    B     EXECX                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST TIME CONTROLS                                                 *         
***********************************************************************         
         SPACE 1                                                                
FIRSTS   CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   XIT                                                              
         MVC   TITLE(10),=C'AUR REPORT'                                         
         OC    TITLE,BLANKS                                                     
*                                                                               
FIRST2   GOTO1 CENTER,DMCB,TITLE,63                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HEADHOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HEADHK   DS    0H                                                               
         GOTO1 GENHEAD             LINE UP HEADLINES AND FORMAT TITLES          
HEADHKX  B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, VARIALBLES, STORAGE AREAS, ETC.                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
*                                                                               
MYRELO   DS    A                                                                
SVR1     DS    F                                                                
MYPRINT  DS    V                   V(PRINT)                                     
MYDUB    DS    D                                                                
AITENT   DS    A                                                                
AITENT2  DS    A                                                                
AITENTHI DS    A                                                                
ABUYELEM DS    A                                                                
AINVTAB  DS    A                                                                
AINVTAB2 DS    A                                                                
ACONTAB  DS    A                                                                
ASTATAB  DS    A                                                                
ATHISSTA DS    A                   A(CURRENT STATAB  ENTRY)                     
ATHISINV DS    A                   A(CURRENT INVTAB  ENTRY)                     
ATHISIN2 DS    A                   A(CURRENT INVTAB2 ENTRY)                     
BUYDEMO  DS    A                   CURRENT BUY PRIMARY DEMO                     
WEEKIND  DS    A                                                                
AUCOUNT  DC    F'0'                                                             
REQDPT   DS    C                   REQUESTED DAYPART CODE                       
REQSTDT  DS    XL2                 REQUEST START DATE (COMPRESSED)              
REQEDDT  DS    XL2                 REQUEST END DATE (COMPRESSED)                
REQSTDT2 DS    XL2                 REQ START DATE -1 YEAR (COMPRESSED)          
REQACST  DS    XL2                 ACTIVITY START DATE (COMPRESSED)             
REQACED  DS    XL2                 ACTIVITY END DATE (COMPRESSED)               
REQASTDT DS    XL3                 BIN RQST ACTIVITY START DATE (YMD)           
REQAEDDT DS    XL3                 BIN RQST ACTIVITY END DATE (YMD)             
REQISTDT DS    XL2                 INVENTORY PER START (COMPRESSED)             
REQIEDDT DS    XL2                 INVENTORY PER END (COMPRESSED)               
*                                                                               
MASK     DS    X                                                                
STAFILT  DS    CL2                 STATION LIMITER:                             
*                                  K OR W: ONLY THESE STATIONS                  
*                                  K- OR W-: EXCLUDE THESE STATIONS             
GSGFILT  DS    CL60                GROUP/SUBGROUP FILTER: ALLOW 30              
         DC    XL2'00'             DELIMITER                                    
NMONTHS  DS    X                   NUMBER OF MONTHS IN REQ PER                  
NDEMS    DS    X                   NUMBER OF DEMOS IN REQ                       
WORK2    DS    CL64                                                             
TESTWORK DS    CL64                                                             
REQEDDTB DS    XL3                 REQUEST END DATE (BINARY)                    
WEEKINDA DS    CL16                                                             
AURDEMS  DS    20XL3               ROOM FOR 20 DEMOS                            
DEMNAMES DS    20CL7               DEMO OUTPUT NAMES                            
CONDEMO  DS    XL3                 THIS CONTRACT DEMO ONLY                      
NWLSBYLO DS    CL12                LAST BUY PROCESSED                           
NWLSBYHI DS    CL12                LAST BUY PROCESSED                           
MAINFLAG DS    CL1                                                              
FIRSTYM  DC    CL1'Y'                                                           
TODAYDAT DS    CL6                                                              
*                                                                               
FLAGS    DC    XL1'00'             VARIOUS FLAGS                                
FLNEG    EQU   X'80'               BUY IS NEGATIVE                              
FLSAT    EQU   X'40'               SATELLITE OPTION                             
FLPRTIT  EQU   X'20'               PRINT THE LINE                               
FLSORT   EQU   X'10'               PUT REC TO SORT                              
FLSATRUN EQU   X'08'               PROCESSING SATELLITES                        
*        EQU   X'04'               *** UNUSED ***                               
*        EQU   X'02'               *** UNUSED ***                               
*        EQU   X'01'               *** UNUSED ***                               
*                                                                               
MATCHTYP DS    X                   TYPE OF MATCHTYP TO PERFORM                  
EXACT    EQU   X'80'                                                            
OVERLAP  EQU   X'40'                                                            
WITHIN   EQU   X'20'                                                            
*                                                                               
CURHIGH  DS    XL4                 SET ON OUTPUT FROM ITHIGH & ITLOW            
CURLOW   DS    XL4                                                              
*                                                                               
FF       EQU   X'FF'                                                            
XFF      DC    16X'FF'                                                          
BLANKS   DC    255C' '                                                          
*                                                                               
DATETYPE DS    X                                                                
MAXPERS  EQU   8                   MAXIMUM NUMBER OF PERIOD COLS                
         DS    0F                                                               
MONTAB   DS    CL(4*(MAXPERS+1)+1)                                              
BUCKETS  DS    CL200                                                            
*                                                                               
INVTABSZ EQU   1+255*ITLEN                                                      
INVTB2SZ EQU   1+20000*ITLEN2                                                   
*NVTB2SZ EQU   1+40000*ITLEN2                                                   
CONTABSZ EQU   8+(8*20000) 20000 8 BYTE ENTS (4 D/A,3 DEM,1 SPARE)              
STATABSZ EQU   1200*STATABLN        ROOM FOR 1000 STA'S                         
BUFFSIZE EQU   INVTABSZ+INVTB2SZ+CONTABSZ+STATABSZ                              
**CONTABSZ EQU   8+(8*100000) 100000 8 BYTE ENTS (4 D/A,3 DEM,1 SPARE)          
*                                                                               
MAXINV   EQU   25                  MAX OF 25 INVENTORY NUMBERS                  
ENTRIES  DS    X                   NUMBER OF INVENTORY NOS (OR PAIRS)           
*                                                                               
         DS    0D                                                               
FETCHBLK DS    XL(RFTBLKL)                                                      
         DS    0D                                                               
REPIOBLK DS    XL(RIPIOLEN)                                                     
*                                                                               
SCANBLK  DS    (MAXINV)CL32                                                     
SCANBLKX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    C'**AURIO*'                                                      
AURIO    DS    XL2000                                                           
*                                                                               
FETCHWRK DS    XL6144                                                           
*                                                                               
*                                                                               
STATABD  DSECT                                                                  
STSTA    DS    CL5                                                              
STMKT    DS    CL20                                                             
STATABLN EQU   *-STATABD                                                        
*                                                                               
INVTABD  DSECT                                                                  
ITNUM    DS    X                                                                
ITFLAG   DS    X                                                                
ITUSED   EQU   X'80'               INDICATES BUY ALREADY IN THIS INV            
*        EQU   X'40'               ** UNUSED **                                 
*        EQU   X'20'               ** UNUSED **                                 
*        EQU   X'10'               ** UNUSED **                                 
*        EQU   X'08'               ** UNUSED **                                 
*        EQU   X'04'               ** UNUSED **                                 
*        EQU   X'02'               ** UNUSED **                                 
*        EQU   X'01'               ** UNUSED **                                 
ITDPT    DS    CL6                                                              
ITDAY    DS    C                                                                
ITSTTM   DS    XL2                                                              
ITENTM   DS    XL2                                                              
ITPGM    DS    CL27                                                             
ITINV    DS    CL4                                                              
ITINVDT  DS    XL4                                                              
ITNUM2AD DS    F                   A(INVTAB2 ENTRY)                             
***      DS    X                   SPARE (FOR ALIGNMENT)                        
***ITHIGHT  DS    F                   HIGH PRICE:  TOTAL                        
***ITLOWT   DS    F                   LOW  PRICE:  TOTAL                        
***ITHISPT  DS    F                   HI PRICE # SPOTS TOTAL                    
***ITLOSPT  DS    F                   LO PRICE # SPOTS TOTAL                    
***ITHIGH   DS    (MAXPERS)F                                                    
***ITLOW    DS    (MAXPERS)F                                                    
***ITHISPOT DS    (MAXPERS)F          HI PRICE # SPOTS MONTHLY                  
***ITLOSPOT DS    (MAXPERS)F          LO PRICE # SPOTS MONTHLY                  
*                                                                               
ITDEMOS  DS    20XL4               ROOM FOR UP TO 20 DEMOS                      
*                                                                               
ITLEN    EQU   *-INVTABD                                                        
*                                                                               
INVTAB2D DSECT                                                                  
ITNUM2   DS    XL2                                                              
         DS    CL2                 SPARE                                        
ITHIGHT  DS    F                   HIGH PRICE:  TOTAL                           
ITLOWT   DS    F                   LOW  PRICE:  TOTAL                           
ITHISPT  DS    F                   HI PRICE # SPOTS TOTAL                       
ITLOSPT  DS    F                   LO PRICE # SPOTS TOTAL                       
ITHIGH   DS    (MAXPERS)F                                                       
ITLOW    DS    (MAXPERS)F                                                       
ITHISPOT DS    (MAXPERS)F          HI PRICE # SPOTS MONTHLY                     
ITLOSPOT DS    (MAXPERS)F          LO PRICE # SPOTS MONTHLY                     
*                                                                               
ITLEN2   EQU   *-INVTAB2D                                                       
*                                                                               
         EJECT                                                                  
         EJECT                                                                  
         SPACE 1                                                                
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE REWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE REWRIF0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE REWRIWORKD                                                     
CONRECD  DSECT                                                                  
       ++INCLUDE REGENCON                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
RSETRECD DSECT                                                                  
       ++INCLUDE REGENSET                                                       
STARECD  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE REFETCHD                                                       
       ++INCLUDE REPIOBLK                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053REWRI20   01/14/13'                                      
         END                                                                    
