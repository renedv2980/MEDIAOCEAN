*          DATA SET REFETCH    AT LEVEL 010 AS OF 12/07/12                      
*PHASE T00AA4A                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE GETKSRC                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE NSIWEEK                                                                
*INCLUDE DAYPAK                                                                 
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - FETCH'                   
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
*  DEC08/04 (BU ) --- PEOPLE METER CHANGES                            *         
*                                                                     *         
*  JAN07/05 (BU ) --- FOR T4 DATA, BYPASS SETTING DBSEL1WK.           *         
*                                                                     *         
*  JAN10/05 (BU ) --- STATION ALIAS PROCESSING                        *         
*                                                                     *         
*  JAN14/05 (BU ) --- ADD OVERNIGHT PROCESSING.  LEVEL 'B' IN PHASE   *         
*                                                                     *         
*  APR20/05 (BU ) --- EXTEND BYPASS SETTING DBSEL1WK TO TYPICAL       *         
*                     TIME PERIOD DATA.                               *         
*                                                                     *         
*  OCT20/05 (BU ) --- COMMENT OUT CHANGE OF APR20/05 PER L. HENDY     *         
*                                                                     *         
*  DEC05/06 (BU ) --- ALIAS:  CABLE OR WIRED CABLE                    *         
*                                                                     *         
*  DEC06/06 (BU ) --- ALIAS:  MAKE THIS CORRECT, FINALLY !!           *         
*                                                                     *         
*  DEC20/06 (BU ) --- RATE CARD:  TEMPORARY DATA SELF-CORRECT         *         
*                                                                     *         
*  FEB21/07 (OP ) --- FIX OVERFLOW PROBLEM FOR OVERNIGHTS             *         
*  FEB21/07 (OP ) --- CHANGE TO GET PEOPLE METER START-END BOOKS      *         
*                     FROM DEMTABS                                    *         
*                                                                     *         
*  AUG05/08 (KUI) --- ADD 'LAST UPDATED' AND 'CREATION' DATA          *         
*                                                                     *         
*  DEC01/08 (SKU) --- ADD RATING PRECISION AND OVERNIGHT BOOKTYPE     *         
*                                                                     *         
*  APR02/09 (SKU) --- SUPPORT FOR NEW INVENTORY KEY                   *         
*                                                                     *         
*  OCT27/10 (SKU) --- FIX TEXT RETRIEVAL USING WRONG BOOKVAL BITS     *         
*                                                                     *         
*  FEB09/11 (SMY) --- Storage protection fix at SETCOMFX (see *SMY*)  *         
*                                                                     *         
***********************************************************************         
REFETCH  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 RFTWRKL,REFETCH*,RR=RE,CLEAR=YES                                 
*                                                                               
         USING RFTWRKD,RC          ESTABLISH WORKING STORAGE                    
*                                                                               
         ST    RE,RELO                                                          
*                                                                               
         MVC   USERRD,4(RD)        SAVE USER RD                                 
*                                                                               
         ST    R1,SVAPARM          SAVE A(PARAMETER LIST)                       
*                                                                               
         L     RA,0(R1)            A(FETCHD)                                    
         LA    RA,0(RA)                                                         
         USING RFTBLKD,RA          ESTABLISH FETCH CONTROL BLOCK                
*                                                                               
         L     R9,=A(COMMON)       ESTABLISH COMMON ROUTINES                    
         A     R9,RELO                                                          
         USING COMMON,R9                                                        
*                                                                               
         L     RF,=V(GETBROAD)                                                  
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VGETBRD                                                       
*                                                                               
         L     RF,=V(NSIWEEK)                                                   
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VNSIWEEK                                                      
*                                                                               
         L     RF,=V(DAYPAK)                                                    
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VDAYPAK                                                       
*                                                                               
         GOTO1 =A(SETCOMFX),RR=RELO                                             
*                                                                               
*                                                                               
         LA    R0,RFTFHDR          INIT RETURNED DATA                           
         LA    R1,RFTFHDRL                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - PARREP'                  
********************************************************************            
*                                                                  *            
*     GET PARENT REP FROM REP RECORD FOR X'62' AND X'E2' LOOKUP.   *            
*         (USE GIVEN REP (AGENCY) FOR DEMO MENU LOOKUP)            *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PARREP   DS    0H                                                               
*                                                                               
*   IF REQUEST IS FOR OVERNIGHTS, NEED TO CALCULATE OVERNIGHT BOOK              
*                                                                               
         CLI   RFTOVRNT,C'Y'       OVERNIGHT REQUEST?                           
         BNE   PARR0010            NO                                           
         GOTO1 =A(OVRNTSET),RR=RELO                                             
PARR0010 EQU   *                                                                
*                                                                               
*   IF REQUEST IS TO OVERRIDE DEMO WITH HOMES, DO SO                            
*                                                                               
         CLI   RFTOVHMS,C'Y'       OVERNIGHT REQUEST?                           
         BNE   PARR0015            NO                                           
         GOTO1 =A(ORIDEDEM),RR=RELO                                             
PARR0015 EQU   *                                                                
*                                                                               
*                                                                               
*   DETERMINE IF THIS IS A LOCAL CABLE OUTLET STATION                           
*                                                                               
*                                                                               
         L     R3,RFTAWRK          SET A(WORKAREA)                              
         A     R3,=F'5800'         CHECK PREVIOUS USE                           
         LR    RF,R3                                                            
         LA    RF,5(RF)            SET A(ALIAS TABLE)                           
         ST    RF,AALIAS           SAVE A(ALIAS TABLE DATA)                     
         CLC   =C'ALIAS',0(R3)     INITIALIZED?                                 
         BE    PARR0020            YES                                          
         XCEF  (R3),200            NO  - CLEAR ALIAS AREA                       
         MVC   0(5,R3),=C'ALIAS'   SET INDICATOR                                
*                                                                               
*   NOTE:  ALIAS DATA BEGINS AT 5800 INTO THIS WORKAREA.                        
*        POSITIONS 1-5 OF THE AREA ARE AN INITIALIZER FLAG: "ALIAS"             
*        THERE ARE THEN A POSSIBLE 11 ENTRIES OF 17 CHARS EACH:                 
*        POSITIONS 1-5     =  STATION CALL LETTERS + MEDIA                      
*        POSITIONS 6-17    =  ALIAS OR EMPTY (BIN ZEROS)                        
*        AFTER 11 ENTRIES, A DELIMITER OF X'FFFFFFFF' IS INSERTED               
*        'AALIAS' IS SET TO A(FIRST 17-CHARACTER ENTRY).                        
*                                                                               
*           BILL UHR JAN10/05                                                   
*                                                                               
         LA    R3,192(R3)          SET ALIAS DELIMITER                          
         MVC   0(4,R3),=X'FFFFFFFF'                                             
PARR0020 EQU   *                                                                
*                                                                               
         GOTO1 =A(CABSTAT),RR=RELO                                              
*                                                                               
         CLI   RFTPAREP,0          SKIP IF PARENT REP KNOWN                     
         BNE   GETPROFX                                                         
*                                                                               
         XC    KEY,KEY             ESTABLISH AS REP RECORD KEY                  
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
*                                                                               
         MVI   RREPKTYP,X'01'      SET RECORD ID                                
         MVC   RREPKREP,RFTCREP    SET REP ID                                   
*                                                                               
         BAS   RE,READ             READ REP RECORD                              
*                                                                               
         MVC   AIO,RFTAIO1         USE A(IOA1)                                  
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         MVI   ELCODE,X'01'        SET TO FIND HEADER ELEMENT                   
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING RREPELEM,R6         ESTABLISH REP HEADER ELEMENT                 
*                                                                               
         MVC   RFTPAREP,RREPPAR    SAVE PARENT REP                              
*                                                                               
PARREPX  DS    0H                                                               
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GETPROF'                 
********************************************************************            
*                                                                  *            
*        GETPROF --- GET AND SET SFM/REP PROFILES FROM THE REP REC *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
GETPROF  DS    0H                                                               
*                                                                               
         XC    RFTSRMPP,RFTSRMPP   INIT RMP PROGRAM PROFILE                     
*                                                                               
         MVI   ELCODE,X'04'        SET TO FIND PROGRAM PROFILE ELM              
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   GETPROFX            NO PROFILE FOUND                             
*                                                                               
*- FIND RMP PROGRAM PROFILE WITHIN PROGRAM PROFILE ELEMENT                      
*                                                                               
         USING RREPPGMP,R6         ESTABLISH PROGRAM PROFILE ELEMENT            
*                                                                               
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
         CLI   0(RE),RREPQRMP      LOOKING FOR RMP PROGRAM PROFILE              
         BE    *+16                                                             
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,*-12                                                          
         B     GETPROFX            NO MATCH                                     
*                                                                               
         MVC   RFTSRMPP,2(RE)      SAVE PROFILE                                 
*                                                                               
GETPROFX DS    0H                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - MODE'                    
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MODE     DS    0H                                                               
*                                                                               
         CLC   RFTCSTAT,SPACES     ANY STATION IN BLOCK?                        
         BNH   MODE0020            NO                                           
*                                                                               
         GOTO1 =A(VSTAT),RR=RELO                                                
MODE0020 EQU   *                                                                
*                                                                               
         CLI   RFTAMODE,RFTADIRQ   FETCH BY DEMOS DIRECTLY                      
         BE    DIR                                                              
*                                                                               
*        COLLECT DAYPARTS IN LIST                                               
*                                                                               
         LA    R1,RFTCDTMS         START OF DAYPART/DAYS/TIMES LIST             
         CLI   0(R1),X'FF'         PARAMETER IS ADDRESS?                        
         BNE   *+8                 NO                                           
         ICM   R1,15,1(R1)                                                      
         ST    R1,SVDTMA           SAVE LIST POINTER                            
*                                                                               
         SR    RF,RF                      COUNT DAYPARTS IN LIST                
         LA    R0,RFTCDTMS+(8*RFTCDTLQ)   END OF LIST                           
DPTINI02 DS    0H                                                               
         CLI   RFTCDTMS,X'FF'      PARAMETER IS ADDRESS?                        
         BE    *+10                                                             
         CR    R1,R0               NO - ARE WE AT THE END OF LIST?              
         BNL   DPTINI10            YES                                          
*                                                                               
         CLI   RFTCDTDP-RFTCDTM(R1),C' '                                        
         BNH   DPTINI10                                                         
*                                                                               
         LA    R1,RFTCDTLQ(R1)            NEXT IN LIST                          
         LA    RF,1(RF)                                                         
         B     DPTINI02                                                         
*                                                                               
DPTINI10 DS    0H                                                               
         STC   RF,SVNUMDPT         SAVE NUMBER OF REQUESTED DAYPARTS            
*                                                                               
         CLI   RFTAMODE,RFTAINVQ   FETCH BY DAYPART POINTERS                    
         BE    INVS                                                             
*                                                                               
         CLI   RFTAMODE,RFTATXTQ   FETCH TEXT DAT ONLY                          
         BE    MST                                                              
*                                                                               
         CLI   RFTAMODE,RFTAMSTQ   FETCH BY MASTER KEYS                         
         BE    MST                                                              
*                                                                               
         B     RFTX                UNKNOWN MODE                                 
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - INVS'                    
***********************************************************************         
*                                                                     *         
*        FETCH INVENTORY RECORDS BASED ON FILTER CRITERIA             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVS     DS    0H                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DPT'                     
********************************************************************            
*                                                                  *            
*     READ INVENTORY RECORDS USING DAYPART POINTER AND PASS        *            
*         TO CALLER                                                *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
DPT      DS    0H                                                               
         L     R2,SVDTMA           POINT TO DAYPART LIST                        
         SR    R3,R3                                                            
         ICM   R3,1,SVNUMDPT       MAX NUMBER OF DAYPARTS                       
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         CLI   RFTCDTDP-RFTCDTM(R2),C' '  DONE IF NO MORE DPTS                  
         BNH   DPTDONE                                                          
*                                                                               
         ST    R2,SVDTMA           SAVE LIST POINTER                            
*                                                                               
         BAS   RE,DPTINV           PROCESS INVENTORY RECS FOR DPT               
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R2,RFTCDTLQ(R2)     BUMP LIST POINTER                            
         BCT   R3,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                  NO MORE DAYPARTS IN LIST                     
*                                                                               
         B     RFTX                DONE                                         
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - MST'                     
***********************************************************************         
*                                                                     *         
*        FETCH INVENTORY RECORDS VIA MASTER KEY                       *         
*              I.E. IN INVENTORY NUMBER ORDER                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MST      DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RINVKEY,R4          ESTABLISH INVENTORY RECORD POINTER           
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET POINTER ID                               
         MVC   RINVKREP,RFTPAREP   USE PARENT REP                               
         MVC   RINVKSTA,RFTCSTAT   SET STATION                                  
*                                                                               
         CLI   RINVKSTA+4,C' '     IF NO MEDIA PROVIDED                         
         BH    *+8                                                              
         MVI   RINVKSTA+4,C'T'        DEFAULT TO TV                             
*                                                                               
         OC    RFTCINV,RFTCINV     SKIP IF NO INVENTORY NUMBER                  
         BZ    *+10                                                             
         MVC   RINVKINV,RFTCINV    SET INVENTORY NUMBER IN KEY                  
*                                                                               
         BAS   RE,HIGH             READ FIRST INVENTORY RECORD                  
*                                                                               
MSTINVLP DS    0H                                                               
*                                                                               
         MVC   SVINVKEY,KEY        SAVE FOUND KEY                               
*                                                                               
         LA    R4,KEY              RESTORE POINTER                              
         USING RINVKEY,R4          ESTABLISH INVENTORY MASTER KEY               
*                                                                               
         CLC   KEYSAVE(RINVKINV-RINVKEY),KEY  MUST BE SAME STATION              
         BNE   MSTINVDN                                                         
*                                                                               
         CLI   RINVKRTP,C'M'       IF MARKET OR STATION LEVEL RECORD            
         BE    *+8                                                              
         CLI   RINVKRTP,C'S'                                                    
         BE    MSTINVTX               GO CHECK OUT IF WANTED                    
*                                                                               
         OC    RINVKRTP,RINVKRTP   IGNORE IF NOT A HEADER RECORD                
         BNZ   MSTINVCN                                                         
*                                                                               
*        FILTER ON INVENTORY NUMBER RANGE                                       
*                                                                               
         CLI   RINVKINV+3,0        IGNORE IF OLD TYPE INV RECORD                
         BE    MSTINVCN                                                         
*                                                                               
         OC    RFTCINV,RFTCINV     SKIP IF NO INVENTORY NUMBER                  
         BZ    MSTINV12                                                         
*                                                                               
         CLC   RINVKINV,RFTCINV    MUST LIE IN INV# RANGE                       
         BE    MSTINV11                                                         
*                                                                               
         OC    RFTCINVL,RFTCINVL   DONE IF NO END TO RANGE                      
         BZ    MSTINVDN                                                         
*                                                                               
         CLC   RINVKINV,RFTCINVL   ELSE MUST BE IN RANGE                        
         BH    MSTINVDN                                                         
*                                                                               
MSTINV11 DS    0H                                                               
*                                                                               
         OC    RFTCEFST,RFTCEFST   SKIP IF THERE IS NO EFF DATE GIVEN           
         BZ    MSTINV12                                                         
*                                                                               
         OC    RFTCINVL,RFTCINVL   IF NO INV RANGE                              
         BNZ   MSTINV12                                                         
         OC    RFTCEFEN,RFTCEFEN   AND NO EFF DATE RANGE                        
         BNZ   MSTINV12                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RFTCEFST),(3,FULL)  TRANSLATE EFF DATE           
*                                                                               
         CLC   RINVKSTD,FULL       THEN NEED EXACT MATCH                        
         BL    MSTINVCN                                                         
         BH    MSTINVDN                                                         
         B     MSTINV13                                                         
*                                                                               
MSTINV12 DS    0H                                                               
*                                                                               
         OC    RFTCEFEN,RFTCEFEN   IF THERE IS AN END EFF DATE                  
         BZ    MSTINV13                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RFTCEFEN),(3,FULL)  TRANSLATE EFF DATE           
*                                                                               
         CLC   RINVKSTD,FULL       SKIP IF IT STARTS TOO LATE                   
         BH    MSTINVCN                                                         
*                                                                               
MSTINV13 DS    0H                                                               
*                                                                               
         MVC   AIO,RFTAIO1         USE A(IOA1)                                  
*                                                                               
         GOTO1 GETRECX             READ IN RECORD                               
         BNE   MSTINVCN                                                         
*                                                                               
         L     R4,RFTAIO1          ESTABLISH INVENTORY RECORD                   
         USING RINVKEY,R4                                                       
*                                                                               
*        APPLY FILTERS FOR EACH ENTRY IN RFTCDTMS LIST                          
*                                                                               
*                                                                               
         L     R2,SVDTMA           POINT TO DAYPART LIST                        
MSTFLTLP DS    0H                                                               
*                                                                               
         USING RFTCDTM,R2          ESTABLISH DPT/DAY/TIMES LIST ENTRY           
*                                                                               
*        FILTER ON TIMES                                                        
*                                                                               
         OC    RFTCDTDY,RFTCDTDY   SKIP IF THERE IS NO DAY FILTER               
         BNZ   *+10                                                             
         OC    RFTCDTST(4),RFTCDTST   AND NO TIME FILTER                        
         BZ    MSTDTMFD                                                         
*                                                                               
*        FILTER ON DAYS                                                         
*                                                                               
         MVI   ELCODE,X'02'           FIND DAY/TIME ELEMENTS                    
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   MSTFLTCN               NO ELEMENT FOUND - DROP                   
*                                                                               
MSTDTMLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6                                                      
*                                                                               
         CLI   RFTCDTDY,0          SKIP IF NO DAYS IN FILTER                    
         BE    *+14                                                             
         CLC   RIDTDAY,RFTCDTDY       MATCH ON DAY                              
         BNE   MSTDTMCN                                                         
*                                                                               
         OC    RFTCDTST(4),RFTCDTST   IF TIMES GIVEN                            
         BZ    MSTDTM22                                                         
*                                                                               
         CLC   RIDTTIME(2),RFTCDTEN      TIMES MUST OVERLAP                     
         BNL   MSTDTMCN                                                         
         CLC   RIDTTIME+2(2),RFTCDTST                                           
         BNH   MSTDTMCN                                                         
*                                                                               
MSTDTM22 DS    0H                                                               
*                                                                               
         B     MSTDTMFD                                                         
*                                                                               
MSTDTMCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    MSTDTMLP                                                         
*                                                                               
MSTDTMDN DS    0H                                                               
*                                                                               
         B     MSTFLTCN               NO MATCH - DROP                           
*                                                                               
MSTDTMFD DS    0H                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   MSTFLTCN            NO ELEMENT FOUND                             
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
*        FILTER ON DAYPART                                                      
*                                                                               
         CLI   RFTCDTDP-RFTCDTM(R2),C' '  SKIP IF NO DPT SPECIFIED              
         BNH   MSTDPTFD                                                         
*                                                                               
         LA    RE,RINVDP           POINT TO INV DAYPARTS                        
         LA    RF,L'RINVDP         MAX NUMBER OF DAYPARTS                       
*                                                                               
MSTDPTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   MSTDPTDN                                                         
*                                                                               
         CLC   RFTCDTDP-RFTCDTM(L'RFTCDTDP,R2),0(RE) MATCH ANY DPT              
         BE    MSTDPTFD                                                         
*                                                                               
MSTDPTCN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            NEXT DAYPART IN RECORD                       
         BCT   RF,MSTDPTLP                                                      
*                                                                               
MSTDPTDN DS    0H                                                               
*                                                                               
         B     MSTFLTCN            NO MATCH                                     
*                                                                               
MSTDPTFD DS    0H                                                               
*                                                                               
*        FILTER ON PASSED FILTERS.                                              
*                                                                               
         OC    RFTCFTRS,RFTCFTRS   SKIP IF NO FILTERS PASSED                    
         BZ    MSTFTRX                                                          
*                                                                               
         LA    R5,RFTCFTRS         POINT TO PASSED FILTERS                      
         LA    RE,L'RFTCFTRS       MAX NUMBER OF FILTERS                        
*                                                                               
MSTFLT1L DS    0H                                                               
*                                                                               
         CLI   0(R5),C'A'          SKIP IF NO FILTER SPECIFIED                  
         BL    MSTFLT1C                                                         
*                                                                               
         LA    R1,RINVPFLT         POINT TO INV REC FILTERS                     
         LA    R0,L'RINVPFLT       MAX NUMBER OF FILTERS                        
*                                                                               
MSTFLT2L DS    0H                                                               
*                                                                               
         CLC   0(1,R5),0(R1)       MATCHES ANY FILTER ON RECORD                 
         BE    MSTFLT1F            THEN WE WANT IT                              
*                                                                               
MSTFLT2C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP INV FILTER POINTER                      
         BCT   R0,MSTFLT2L                                                      
*                                                                               
MSTFLT2D DS    0H                  NO MATCH SO FAR                              
*                                                                               
MSTFLT1C DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP PASSED FILTERS POINTER                  
         BCT   RE,MSTFLT1L                                                      
*                                                                               
         B     MSTFLTCN            NOTHING MATCHES-DON'T WANT IT                
*                                                                               
MSTFLT1F DS    0H                  PASSES FILTERS TEST                          
*                                                                               
MSTFTRX  DS    0H                                                               
*                                                                               
*        FILTER ON OVERRIDE EFFECTIVE DATES                                     
*                                                                               
         OC    RFTCDTES(4),RFTCDTES SKIP IF NO EFF DATE OVERRIDES               
         BZ    MSTOEDTN                                                         
*                                                                               
         OC    RFTCDTEE,RFTCDTEE    SKIP IF                                     
         BZ    *+14                    NO END DATE FILTER GIVEN                 
         CLC   RFTCDTEE,RINVPEFF    IGNORE IF                                   
         BL    MSTFLTCN                STARTING AFTER SELECTED END DATE         
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2 KEEP IF INV HAS OPEN END DATE           
         BZ    MSTOEDTX                                                         
*                                                                               
         CLC   RFTCDTES,RINVPEFF+2 IGNORE ANY                                   
         BH    MSTFLTCN               ENDING BEFORE SELECTED START DATE         
*                                                                               
MSTOEDTX DS    0H                                                               
*                                                                               
         B     MSTEDTX             ELSE KEEP                                    
*                                                                               
MSTOEDTN DS    0H                                                               
*                                                                               
*        FILTER ON DEFAULT EFFECTIVE DATES                                      
*                                                                               
         OC    RFTCEFST(4),RFTCEFST SKIP IF NO EFF DATE FILTERING               
         BZ    MSTEDTX                                                          
*                                                                               
         OC    RFTCEFEN,RFTCEFEN    SKIP IF                                     
         BZ    *+14                    NO END DATE FILTER GIVEN                 
         CLC   RFTCEFEN,RINVPEFF    IGNORE IF                                   
         BL    MSTFLTCN                STARTING AFTER SELECTED END DATE         
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2 KEEP IF INV HAS OPEN END DATE           
         BZ    MSTEDTX                                                          
*                                                                               
         CLC   RFTCEFST,RINVPEFF+2 IGNORE ANY                                   
         BH    MSTFLTCN               ENDING BEFORE SELECTED START DATE         
*                                                                               
         B     MSTEDTX             ELSE KEEP                                    
*                                                                               
MSTEDTX  DS    0H                                                               
*                                                                               
         B     MSTFLTFD            PASSES ALL FILTERS                           
*                                                                               
MSTFLTCN DS    0H                                                               
*                                                                               
         LA    R2,RFTCDTLQ(R2)     BUMP TO NEXT ENTRY IN LIST                   
         ZIC   RF,SVNUMDPT         GET NUMBER OF DAYPARTS                       
         MHI   RF,RFTCDTLQ         GET DISPLACMENT TO END OF LIST               
         A     RF,SVDTMA           ADD START OF LIST                            
         CR    R2,RF               CONTINUE IF END OF LIST NOT REACHED          
         BL    MSTFLTLP                                                         
*                                                                               
MSTFLTDN DS    0H                                                               
*                                                                               
         B     MSTINVCN            NO MATCHES - DROP                            
*                                                                               
MSTFLTFD DS    0H                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - MSTINVGT'                
***********************************************************************         
*                                                                     *         
*        ALL FILTERS PASSED - GO GET DATA FOR INVENTORY NUMBER        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MSTINVGT DS    0H                                                               
*                                                                               
*                                                                               
         MVC   SVINVKEY,RINVKEY    SAVE INVENTORY KEY                           
*                                                                               
         BAS   RE,HDRFMT           FORMAT HEADER DATA FOR CALLER                
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    MSTINVDN                                                         
*                                                                               
         BAS   RE,GTBOOK           GO AND LOOK FOR BOOKS                        
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    MSTINVDN                                                         
*                                                                               
MSTINVTX DS    0H                                                               
*                                                                               
         GOTO1 =A(GTTXT),RR=RELO GO AND LOOK FOR TEXT DATA                      
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    MSTINVDN                                                         
*                                                                               
         MVC   KEY,SVINVKEY        RESTORE MASTER POINTER                       
*                                                                               
         BAS   RE,HIGH             RESTORE FILE POINTER                         
*                                                                               
         CLI   RINVKRTP,C'M'       DONE IF MARKET OR STATION RECORD             
         BE    *+8                                                              
         CLI   RINVKRTP,C'S'                                                    
         BE    MSTINVDN               GO CHECK OUT IF WANTED                    
*                                                                               
MSTINVCN DS    0H                                                               
*                                                                               
         BAS   RE,SEQ              READ NEXT INVENTORY RECORD                   
*                                                                               
         B     MSTINVLP                                                         
*                                                                               
MSTINVDN DS    0H                                                               
*                                                                               
MSTINVX  DS    0H                                                               
         B     RFTX                                                             
         DROP  R6,R4               JRD                                          
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DIR'                     
***********************************************************************         
*                                                                     *         
*        FETCH DEMO DATA DIRECTLY FROM DEMO FILES BASED ON            *         
*              CALLERS DAYS/TIMES                                     *         
*                                                                     *         
*              APPROACH IS TO BUILD A DUMMY INVENTORY RECORD AND      *         
*              THEN FETCH THE DEMOS                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DIR      DS    0H                                                               
*                                                                               
*        BUILD INVENTORY KEY                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RINVKEY,R4          ESTABLISH INVENTORY RECORD POINTER           
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET POINTER ID                               
         MVC   RINVKREP,RFTPAREP   USE PARENT REP                               
         MVC   RINVKSTA,RFTCSTAT   SET STATION                                  
*                                                                               
         CLI   RINVKSTA+4,C' '     IF NO MEDIA PROVIDED                         
         BH    *+8                                                              
         MVI   RINVKSTA+4,C'T'        DEFAULT TO TV                             
*                                                                               
         OC    RFTCINV,RFTCINV     SKIP IF NO INVENTORY NUMBER                  
         BZ    *+10                                                             
         MVC   RINVKINV,RFTCINV    SET INVENTORY NUMBER IN KEY                  
*                                                                               
         L     R4,RFTAIO1          POINT TO INVENTORY I/O AREA                  
         XC    0(256,R4),0(R4)     CLEAR STAT OF I/OAREA                        
*                                                                               
         MVC   RINVKEY,KEY         COPY KEY                                     
*                                                                               
         LA    RF,RINVPEL-RINVKEY  SET MINIMUM LENGTH OF RECORD                 
         STCM  RF,3,RINVLEN                                                     
*                                                                               
         LA    R6,RINVPEL          ESTABLISH HEADER ELEMENT                     
         USING RINVPEL,R6                                                       
*                                                                               
         MVI   RINVPCOD,X'01'      SET ELEMENT CODE                             
*                                                                               
         LA    RF,RINVPROG-RINVPEL SET ELEMENT MINIMUM LENGTH                   
         STC   RF,RINVPLEN                                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RINVLEN        UPDATE RECORD LENGTH                         
         AR    RE,RF                                                            
         STCM  RE,3,RINVLEN                                                     
*                                                                               
         AR    R6,RF               BUMP TO NEXT ELEMENT AREA                    
         LA    R3,RFTCDTMS         POINT TO CALLERS DAY/TIMES                   
         USING RFTCDTM,R3          ESTABLISH LIST ENTRY                         
*                                                                               
DIRDTMLP DS    0H                                                               
*                                                                               
         OC    RFTCDTM(RFTCDTLQ),RFTCDTM   TEST FOR END OF LIST                 
         BZ    DIRDTMDN                                                         
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         XC    RIDTELEM(7),RIDTELEM   INIT THE ELEMENT                          
*                                                                               
         MVI   RIDTCODE,X'02'      SET ELEMENT CODE                             
         MVI   RIDTLEN,7           SET ELEMENT LENGTH                           
         MVC   RIDTDAY,RFTCDTDY    SET DAY                                      
         MVC   RIDTTIME,RFTCDTST   SET TIME                                     
*                                                                               
         LA    R6,7(R6)            BUMP TO NEXT ELEMENT AREA                    
*                                                                               
DIRDTMCN DS    0H                                                               
*                                                                               
         LA    R3,RFTCDTLQ(R3)     BUMP TO NEXT DAY/TIME AREA                   
         B     DIRDTMLP                                                         
*                                                                               
DIRDTMDN DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVI   0(R6),0             LAST BYTE MUST BE NULLS                      
*                                                                               
         SR    R6,R4               RECORD LENGTH                                
*                                                                               
*JRD     SR    RE,RE                                                            
*JRD     ICM   RE,3,RINVLEN        UPDATE RECORD LEMGTH                         
*JRD     AR    RE,R6                                                            
*JRD     STCM  RE,3,RINVLEN                                                     
         STCM  R6,3,RINVLEN        JRD - ACTUAL RECORD LENGTH                   
*                                                                               
*        DUMMY INVENTORY RECORD HAS BEEN BUILT                                  
*                                                                               
         MVC   SVINVKEY,KEY        SAVE KEY                                     
*                                                                               
         LA    R4,KEY              RESTORE POINTER                              
         USING RINVKEY,R4          ESTABLISH INVENTORY MASTER KEY               
*                                                                               
         BAS   RE,GTBOOK           GO AND LOOK FOR BOOKS                        
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    DIRX                                                             
*                                                                               
*                                                                               
DIRX     DS    0H                                                               
         B     RFTX                                                             
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - RFTX'                    
***********************************************************************         
*                                                                     *         
*        PROGRAM EXITS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
RFTX     DS    0H                                                               
         CLI   RFTERR,0                                                         
         XIT1                                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DPTINV'                  
********************************************************************            
*                                                                  *            
*     FIND FIRST/NEXT INVENTORY RECORD FOR DAYPART                 *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
DPTINV   NTR1                                                                   
*                                                                               
         L     R2,SVDTMA           POINT TO CURRENT LIST ENTRY                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RIDPKEY,R4          ESTABLISH INVENTORY DAYPART POINTER          
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ   SET DAYPART POINTER ID                       
         MVC   RIDPKREP,RFTPAREP   USE PARENT REP                               
         MVC   RIDPKSTA,RFTCSTAT   SET STATION                                  
*                                                                               
         CLI   RIDPKSTA+4,C' '     IF NO MEDIA PROVIDED                         
         BH    *+8                                                              
         MVI   RIDPKSTA+4,C'T'        DEFAULT TO TV                             
*                                                                               
         MVC   RIDPKDPT,RFTCDTDP-RFTCDTM(R2) SET DAYPART                        
*                                                                               
         OC    RFTCINV,RFTCINV     IF THERE IS AN INVENTORY # FILTER            
         BZ    *+10                                                             
         MVC   RIDPKINV,RFTCINV       SET IN KEY                                
*                                                                               
         BAS   RE,HIGH             READ FIRST INVENTORY RECORD                  
*                                                                               
DPTINVLP DS    0H                                                               
*                                                                               
         LA    R4,KEY              RESTORE POINTER                              
         USING RIDPKEY,R4          ESTABLISH INVENTORY DAYPART POINTER          
*                                                                               
         CLC   KEYSAVE(RIDPKDAY-RIDPKEY),KEY  MUST BE SAME DAYPART              
         BNE   DPTINVDN                                                         
*                                                                               
         CLI   RIDPKINV,0          IGNORE IF OLD TYPE INV RECORD                
         BE    DPTINVCN                                                         
*                                                                               
*        FILTER ON INVENTORY NUMBER                                             
*                                                                               
         OC    RFTCINV,RFTCINV     SKIP IF NO INVENTORY NUMBER                  
         BZ    DPTINV11                                                         
*                                                                               
         CLC   RIDPKINV,RFTCINV    MUST LIE IN INV# RANGE                       
         BE    DPTINV11                                                         
*                                                                               
         OC    RFTCINVL,RFTCINVL   DROP IF NO END TO RANGE                      
         BZ    DPTINVCN                                                         
*                                                                               
         CLC   RIDPKINV,RFTCINVL   ELSE MUST BE IN RANGE                        
         BH    DPTINVCN                                                         
*                                                                               
DPTINV11 DS    0H                                                               
*                                                                               
         MVC   AIO,RFTAIO1         USE A(IOA1)                                  
*                                                                               
         GOTO1 GETRECX             READ IN RECORD                               
         BNE   DPTINVCN            SKIP ON BAD READ                             
*                                                                               
         L     R4,RFTAIO1          ESTABLISH INVENTORY RECORD                   
         USING RINVKEY,R4                                                       
*                                                                               
*        APPLY FILTERS FOR EACH ENTRY IN RFTCDTMS LIST                          
*                                                                               
*        IF RECORD PASSES, AND REPORTING FIRST OCCURRENCE IN LIST               
*           THE ENTRY MUST MATCH THE CURRENT ENTRY                              
*           OR ELSE RECORD IS BYPASSED. I.E. IT MATCHED PRIOR ENTRY             
*           OR WILL MATCH A LATER ENTRY                                         
*                                                                               
         LA    R2,RFTCDTMS         POINT TO START OF DAYPART LIST               
         CLI   0(R2),X'FF'                                                      
         BNE   *+8                                                              
         ICM   R2,15,1(R2)                                                      
*                                                                               
         CLI   RFTCDCTL,0         IF NO DAYPART CONTROL OPTIONS                 
         BNE   *+8                                                              
         L     R2,SVDTMA             START AT CURRENT LIST ENTRY                
*                                                                               
DPTFLTLP DS    0H                                                               
*                                                                               
         USING RFTCDTMS,R2         ESTABLISH DPT/DAY/TIMES LIST ENTRY           
*                                                                               
*        FILTER ON TIMES                                                        
*                                                                               
         OC    RFTCDTDY,RFTCDTDY   SKIP IF THERE IS NO DAY FILTER               
         BNZ   *+10                                                             
         OC    RFTCDTST(4),RFTCDTST   AND NO TIME FILTER                        
         BZ    DTMDTMFD                                                         
*                                                                               
*        FILTER ON DAYS                                                         
*                                                                               
         MVI   ELCODE,X'02'           FIND DAY/TIME ELEMENTS                    
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   DPTFLTCN               NO ELEMENT FOUND - DROP                   
*                                                                               
DTMDTMLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6                                                      
*                                                                               
         CLI   RFTCDTDY,0          SKIP IF NO DAYS IN FILTER                    
         BE    *+14                                                             
         CLC   RIDTDAY,RFTCDTDY       MATCH ON DAY                              
         BNE   DTMDTMCN                                                         
*                                                                               
         OC    RFTCDTST(4),RFTCDTST   IF TIMES GIVEN                            
         BZ    DTMDTM22                                                         
*                                                                               
         CLC   RIDTTIME(2),RFTCDTEN      TIMES MUST OVERLAP                     
         BNL   DTMDTMCN                                                         
         CLC   RIDTTIME+2(2),RFTCDTST                                           
         BNH   DTMDTMCN                                                         
*                                                                               
DTMDTM22 DS    0H                                                               
*                                                                               
         B     DTMDTMFD                                                         
*                                                                               
DTMDTMCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DTMDTMLP                                                         
*                                                                               
DTMDTMDN DS    0H                                                               
*                                                                               
         B     DPTFLTCN               NO MATCH - DROP                           
*                                                                               
DTMDTMFD DS    0H                                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   DPTFLTCN            NO ELEMENT FOUND                             
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
*        FILTER ON DAYPART                                                      
*                                                                               
         CLI   RFTCDCTL,RFTCDCPQ   IF REPORTING IN PRIMARY DAYPART              
         BNE   *+18                                                             
         CLC   RFTCDTDP-RFTCDTM(L'RFTCDTDP,R2),RINVDP  MATCH 1ST DPT            
         BNE   DPTFLTCN                                                         
         B     DPTDPTFD                                                         
*                                                                               
         LA    RE,RINVDP           POINT TO INV DAYPARTS                        
         LA    RF,L'RINVDP         MAX NUMBER OF DAYPARTS                       
*                                                                               
DPTDPTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   DPTDPTDN                                                         
*                                                                               
         CLC   RFTCDTDP-RFTCDTM(L'RFTCDTDP,R2),0(RE) MATCH ANY DPT              
         BE    DPTDPTFD                                                         
*                                                                               
DPTDPTCN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            NEXT DAYPART IN RECORD                       
         BCT   RF,DPTDPTLP                                                      
*                                                                               
DPTDPTDN DS    0H                                                               
*                                                                               
         B     DPTFLTCN            NO MATCH                                     
*                                                                               
DPTDPTFD DS    0H                                                               
*                                                                               
*        FILTER ON PASSED FILTERS.                                              
*                                                                               
         OC    RFTCFTRS,RFTCFTRS   SKIP IF NO FILTERS PASSED                    
         BZ    DTMFTRX                                                          
*                                                                               
         LA    R5,RFTCFTRS         POINT TO PASSED FILTERS                      
         LA    RE,L'RFTCFTRS       MAX NUMBER OF FILTERS                        
*                                                                               
DTMFLT1L DS    0H                                                               
*                                                                               
         CLI   0(R5),C'A'          SKIP IF NO FILTER SPECIFIED                  
         BL    DTMFLT1C                                                         
*                                                                               
         LA    R1,RINVPFLT         POINT TO INV REC FILTERS                     
         LA    R0,L'RINVPFLT       MAX NUMBER OF FILTERS                        
*                                                                               
DTMFLT2L DS    0H                                                               
*                                                                               
         CLC   0(1,R5),0(R1)       MATCHES ANY FILTER ON RECORD                 
         BE    DTMFLTFD            THEN WE WANT IT                              
*                                                                               
DTMFLT2C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP INV FILTER POINTER                      
         BCT   R0,DTMFLT2L                                                      
*                                                                               
DTMFLT2D DS    0H                  NO MATCH SO FAR                              
*                                                                               
DTMFLT1C DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP PASSED FILTERS POINTER                  
         BCT   RE,DTMFLT1L                                                      
*                                                                               
         B     DPTFLTCN            NOTHING MATCHES-DON'T WANT IT                
*                                                                               
DTMFLTFD DS    0H                  PASSES FILTERS TEST                          
*                                                                               
DTMFTRX  DS    0H                                                               
*                                                                               
*        FILTER ON OVERRIDE EFFECTIVE DATES                                     
*                                                                               
         OC    RFTCDTES(4),RFTCDTES SKIP IF NO EFF DATE OVERRIDES               
         BZ    DTMOEDTN                                                         
*                                                                               
         OC    RFTCDTEE,RFTCDTEE    SKIP IF                                     
         BZ    *+14                    NO END DATE FILTER GIVEN                 
         CLC   RFTCDTEE,RINVPEFF    IGNORE IF                                   
         BL    DPTFLTCN                STARTING AFTER SELECTED END DATE         
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2 KEEP IF INV HAS OPEN END DATE           
         BZ    DTMOEDTX                                                         
*                                                                               
         CLC   RFTCDTES,RINVPEFF+2 IGNORE ANY                                   
         BH    DPTFLTCN               ENDING BEFORE SELECTED START DATE         
*                                                                               
DTMOEDTX DS    0H                                                               
*                                                                               
         B     DTMEDTX             ELSE KEEP                                    
*                                                                               
DTMOEDTN DS    0H                                                               
*                                                                               
*        FILTER ON DEFAULT EFFECTIVE DATES                                      
*                                                                               
         OC    RFTCEFST(4),RFTCEFST SKIP IF NO EFF DATE FILTERING               
         BZ    DTMEDTX                                                          
*                                                                               
         OC    RFTCEFEN,RFTCEFEN    SKIP IF                                     
         BZ    *+14                    NO END DATE FILTER GIVEN                 
         CLC   RFTCEFEN,RINVPEFF    IGNORE IF                                   
         BL    DPTFLTCN                STARTING AFTER SELECTED END DATE         
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2 KEEP IF INV HAS OPEN END DATE           
         BZ    DTMEDTX                                                          
*                                                                               
         CLC   RFTCEFST,RINVPEFF+2 IGNORE ANY                                   
         BH    DPTFLTCN               ENDING BEFORE SELECTED START DATE         
*                                                                               
         B     DTMEDTX             ELSE KEEP                                    
*                                                                               
DTMEDTX  DS    0H                                                               
         B     DPTFLTFD            PASSES ALL FILTERS                           
*                                                                               
DPTFLTCN DS    0H                                                               
*                                                                               
         LA    R2,RFTCDTLQ(R2)     BUMP TO NEXT ENTRY IN LIST                   
         C     R2,SVDTMA           CONTINUE IF NOT PAST CURRENT ENTRY           
         BNH   DPTFLTLP                                                         
         B     DPTINVCN            NO MATCHES - DROP                            
*                                                                               
DPTFLTFD DS    0H                                                               
         CLI   RFTCDCTL,0         SKIP IF NO DAYPART CONTROL OPTIONS            
         BE    DTMDCTLX                                                         
*                                                                               
         C     R2,SVDTMA          ELSE FOUND ENTRY MUST BE CURRENT ONE          
         BNE   DPTINVCN                                                         
*                                                                               
DTMDCTLX DS    0H                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DPTINVGT'                
***********************************************************************         
*                                                                     *         
*        ALL FILTERS PASSED - GO GET DATA FOR INVENTORY NUMBER        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTINVGT DS    0H                                                               
         MVC   SVPINVKY,KEY        SAVE DPT PASSIVE                             
         MVC   SVINVKEY,RINVKEY    SAVE INVENTORY RECORD KEY                    
*                                                                               
         BAS   RE,HDRFMT           FORMAT HEADER DATA FOR CALLER                
*                                                                               
         CLI   RFTRETRN,RFTRBADQ   SKIP IF USER REQUESTS                        
         BE    DPTINVRJ                                                         
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    DPTINVDN                                                         
*                                                                               
         BAS   RE,GTBOOK           GO AND LOOK FOR BOOKS                        
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    DPTINVDN                                                         
*                                                                               
         GOTO1 =A(GTTXT),RR=RELO                                                
*                                                                               
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS                        
         BE    DPTINVDN                                                         
*                                                                               
DPTINVRJ DS    0H                                                               
         MVC   KEY,SVPINVKY        RESTORE PASSIVE POINTER                      
*                                                                               
         BAS   RE,HIGH             RESTORE FILE POINTER                         
*                                                                               
         B     DPTINVCN                                                         
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DPTINVCN'                
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FINDING INVENTORY RECORDS FOR DAYPART        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTINVCN DS    0H                                                               
*                                                                               
         BAS   RE,SEQ              READ NEXT DAYPART POINTER                    
*                                                                               
         B     DPTINVLP                                                         
*                                                                               
DPTINVDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - HDRFMT'                  
***********************************************************************         
*                                                                     *         
*        FORMAT HEADER FIELDS FOR RETURN                              *         
*              INCLUDES RATE DATA IF ASKED FOR                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDRFMT   NTR1  LABEL=*                                                          
*                                                                               
         LA    R0,RFTFHDR          INIT RETURNED DATA                           
         LA    R1,RFTFHDRL                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   RFTFDTM,0(R2)       RETURN DPT/DY/TM MATCHED TO                  
*                                                                               
         TM    RFTCNTL,RFTCHDRQ+RFTCRTEQ    SKIP UNLESS HDR DATA WANTED         
         BZ    HDRFMTX                                                          
*                                                                               
         L     R4,RFTAIO1          ESTABLISH INVENTORY RECORD                   
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   RFTFINV,RINVKINV    INVENTORY ID                                 
*                                                                               
         TM    RFTCNTL,RFTCHDRQ    SKIP UNLESS HEADER DATA WANTED               
         BNO   HFMHDRX                                                          
*                                                                               
*        RETURN DAYS                                                            
*                                                                               
         MVI   ELCODE,X'02'        FIND DAY/TIME ELEMENTS                       
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   HFMDTMX             NO ELEMENT FOUND                             
*                                                                               
         LA    R0,8                MAX NUMBER OF DAY/TIMES                      
         LA    R2,RFTFDTMS         POINT TO FIRST DAY/TIME                      
*                                                                               
HFMDTMLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6                                                      
*                                                                               
         MVC   0(5,R2),RIDTDAY     RETURN DAY-TIMES                             
*                                                                               
HFMDTMCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   HFMDTMDN            END OF ELEMENTS                              
*                                                                               
         LA    R2,5(R2)            NEXT DAY/TIME                                
         BCT   R0,HFMDTMLP                                                      
*                                                                               
HFMDTMDN DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
HFMDTMX  DS    0H                                                               
*                                                                               
*        RETURN PROGRAMMING                                                     
*                                                                               
         MVI   ELCODE,X'03'        FIND PROGRAM ELEMENTS                        
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   HFMPGMX             NO ELEMENT FOUND                             
*                                                                               
         LA    R0,8                MAX NUMBER OF PROGRAM NAMES                  
         LA    R2,RFTFPGMS         POINT TO FIRST PROGRAM NAME                  
*                                                                               
HFMPGMLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN         ELEMENT LENGTH                                
         SH    RF,=Y(RIPGNAME-RIPGELEM) PROGRAM NAME LENGTH                     
         BZ    HFMPGMCN                                                         
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RIPGNAME    RETURN PROGRAM NAME                          
*                                                                               
HFMPGMCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   HFMPGMDN            END OF ELEMENTS                              
*                                                                               
         LA    R2,27(R2)           NEXT PROGRAM NAME                            
         BCT   R0,HFMPGMLP                                                      
*                                                                               
HFMPGMDN DS    0H                                                               
*                                                                               
HFMPGMX  DS    0H                                                               
*                                                                               
*        RETURN AVAILS                                                          
*                                                                               
HFMAVL   DS    0H                                                               
*                                                                               
         L     R6,RFTAIO1                                                       
         MVI   ELCODE,X'04'        AVAIL DAY/TIMES                              
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   HFMAVLN             NONE FOUND                                   
*                                                                               
         LA    R2,RFTFAVLS         POINT TO AVAILS AREA                         
*                                                                               
HFMAVLLP DS    0H                                                               
*                                                                               
         USING RIAPELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   0(L'RFTFAVLS,R2),RIADAY  RETURN AVAILS DAY AND TIME              
*                                                                               
HFMAVLCN DS    0H                                                               
*                                                                               
         LA    R2,L'RFTFAVLS(R2)   BUMP TO NEXT AVAILS AREA                     
*                                                                               
         BAS   RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    HFMAVLLP            ONE FOUND                                    
*                                                                               
HFMAVLDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
*                                                                               
HFMAVLN  DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   HFMAVLN5            NO ELEMENT FOUND                             
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   RFTFDPTS,RINVDP     DAYPART CODES                                
         MVC   RFTFFTRS,RINVPFLT   FILTERS                                      
         MVC   RFTFEFST(4),RINVPEFF  EFFECTECTIVE START AND END DATES           
         DROP  R6                                                               
*                                                                               
* ACTIVITY ELEMENT (LAST UPDATE AND CREATION)                                   
*                                                                               
HFMAVLN5 DS    0H                                                               
         XC    RFTFCDTE,RFTFCDTE                                                
         XC    RFTFLUPD,RFTFLUPD                                                
         MVI   ELCODE,X'EF'        FIND ACTIVITY ELEMENT                        
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   HFMHDRX             NO ELEMENT FOUND                             
         USING RINVAEL,R6          ACTIVITY ELEMENT                             
         GOTO1 VDATCON,DMCB,(3,RINVAFST),(2,RFTFCDTE)                           
         GOTO1 VDATCON,DMCB,(3,RINVALST),(2,RFTFLUPD)                           
         DROP  R6                                                               
*                                                                               
HFMHDRX  DS    0H                                                               
*                                                                               
*        RETURN RATES                                                           
*                                                                               
HDRRTE   DS    0H                                                               
*                                                                               
         TM    RFTCNTL,RFTCRTEQ    SKIP UNLESS RATES WANTED                     
         BNO   HDRRTEX                                                          
*                                                                               
         TM    RFTCNTL,RFTCRNWQ    NEW STYLE RATE - SKIP FOR NOW                
         BO    HDRRTEX                 NEED HEADER HOOK 1ST                     
*                                                                               
         GOTO1 =A(OLDRATE),RR=Y                                                 
*                                                                               
HDRRTEX  DS    0H                                                               
*                                                                               
         MVI   RFTMODE,RFTNHDRQ    INDICATE NEW HEADER DATA                     
*                                                                               
         BAS   RE,HOOK             HOOK TO CALLER                               
*                                                                               
         CLI   RFTRETRN,RFTRBADQ   SKIP IF USER REQUESTS                        
         BE    HDRNRTX                                                          
*                                                                               
         TM    RFTCNTL,RFTCRTEQ    SKIP UNLESS RATES WANTED                     
         BNO   HDRNRTX                                                          
*                                                                               
         TM    RFTCNTL,RFTCRNWQ    OLD STYLE RATE - ALREADY DONE                
         BNO   HDRNRTX                                                          
*                                                                               
         GOTO1 =A(NEWRATE),RR=Y                                                 
*                                                                               
HDRNRTX  DS    0H                                                               
*                                                                               
HFMHOOKX DS    0H                                                               
*                                                                               
HDRFMTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GTBOOK'                  
***********************************************************************         
*                                                                     *         
*        CONTROL READING OF BOOKS - NON RANGE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTBOOK   NTR1                                                                   
*                                                                               
         TM    RFTCNTL,RFTCDEMQ    SKIP UNLESS DEMO DATA WANTED                 
         BNO   GBOK0900                                                         
*                                                                               
         LA    R2,RFTCBKS          LIST OF DESIRED BOOKS                        
         CLI   0(R2),X'FF'         ADDRESS PARM?                                
         BNE   *+8                 NO                                           
         ICM   R2,15,1(R2)                                                      
*                                                                               
         LA    R4,KEY              SWITCH DSECT TO KEY AREA                     
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   AIO,RFTAIO2         USE SECOND IOAREA FOR DEMO TRACK             
*                                                                               
GBOK0020 DS    0H                                                               
         OC    0(L'RFTCBKS,R2),0(R2)   DONE IF END OF BOOKS REACHED             
         BZ    GBOK0160                                                         
*                                                                               
         XC    ARAVLNEL,ARAVLNEL   NO UPGRADE TO BE DONE                        
         GOTO1 =A(PURE),RR=RELO    GET DEMO DATA FROM DEMO FILES                
*                                                                               
         BAS   RE,DEMDATA          GET DEMO DATA                                
*                                                                               
         TM    RFTCNTL,RFTCSLVQ    SKIP IF SHARES AND LEVELS NOT WANTED         
         BNO   GBOK0060                                                         
*                                                                               
         CLI   MULTSW,C'Y'         SKIP IF COMBINATION BOOK                     
         BE    GBOK0060                                                         
*                                                                               
         TM    RFTSRMPP+RESMASTB,RESMHPTA      PRINT E/P SHARES?                
         BZ    GBOK0040                        YES                              
*                                                                               
         CLC   SVPGMCD,=C'ES'      SKIP IF PROGRAM CODE IS ES                   
         BE    GBOK0060                                                         
*                                                                               
         CLC   SVPGMCD,=C'PE'      OR PE                                        
         BE    GBOK0060                                                         
*                                                                               
****     CLC   SVPGMCD,=C'PJ'      OR PJ                                        
****     BE    GBOK0060                                                         
*                                                                               
****     CLC   SVPGMCD,=C'PA'      OR PA                                        
****     BE    GBOK0060                                                         
*                                                                               
GBOK0040 DS    0H                                                               
         GOTO1 =A(GTSHRLVL),RR=RELO   GET SHARES AND LEVELS                     
*                                                                               
GBOK0060 DS    0H                                                               
*                                                                               
*        PRINT UPGRADE EXPRESSION                                               
*                                                                               
         TM    PRINTOPT,PROPTUGQ   IF THERE IS ANY UPGRADE                      
         BZ    *+8                                                              
         BAS   RE,UPPRNT              PRINT UPGRADE EXPRESSION                  
*                                                                               
*        PRINT FOOTNOTES                                                        
*                                                                               
         MVI   OPTION,C'A'         INDICATE AUTO FOOTNOTES WANTED               
*                                                                               
         CLI   GSOQLF,C'P'         IF P OR E BOOK                               
         BE    *+8                                                              
         CLI   GSOQLF,C'E'                                                      
         BNE   GBOK0080                                                         
*                                                                               
         TM    RFTSRMPP+RMPFTNTB,RMPFTNTA      IF SUPPRESSING FOOTNOTES         
         BNO   GBOK0080                                                         
*                                                                               
         MVI   OPTION,C'X'                 INDICATE NO FOOTNOTES WANTED         
*                                                                               
GBOK0080 DS    0H                                                               
*                                                                               
*        PRINT HISTORICAL DATA                                                  
*                                                                               
         GOTO1 =A(HISTDATA),RR=RELO FIND HISTORICAL DATA                        
*                                                                               
         GOTO1 =A(FMTTXT),RR=RELO  RETURN ANY AUTO FOOTNOTES                    
*                                                                               
GBOK0100 DS    0H                                                               
*                                                                               
*        HOOK TO CALLER                                                         
*                                                                               
         MVI   RFTMODE,RFTNBKQ     INDICATE NEW BOOK                            
*                                                                               
         BAS   RE,HOOK             HOOK TO CALLER                               
*                                                                               
         XC    RFTFDDAT(256),RFTFDDAT   CLEAR DEMO DATA AREAS                   
         XC    RFTFDDAT+256(RFTFDDAL-256),RFTFDDAT+256                          
*                                                                               
         BAS   RE,CLEARDEM         CLEAR EXTENDED DEMO AREAS                    
*                                                                               
         XC    RFTFTDAT(RFTFTDAL),RFTFTDAT   CLEAR TEXT DATA AREAS              
*                                                                               
*        NEXT BOOK                                                              
*                                                                               
GBOK0120 DS    0H                                                               
*                                                                               
         CLI   RFTRETRN,RFTRNXTQ   DONE IF USER REQUESTS NEW CATEGORY           
         BE    *+8                                                              
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS END                    
         BE    GBOK0160                                                         
*                                                                               
*   NOTE:  FOR OVERNIGHT AVERAGING, THE DATE IN RFTCBKS MAY BE                  
*        THE START OF A BLOCK OF DATES WHICH WILL BE AVERAGED                   
*        TOGETHER.  THE 'PURE' ROUTINE PROCESSES ALL THE DATES                  
*        IN THE BLOCK (THEY ARE SPECIALLY FLAGGED) AND PASSES                   
*        BACK A COUNT OF 'EXTRA' (BEYOND THE FIRST ENTRY) DATES                 
*        AS WELL AS THE ADDRESS OF THE LAST DATE IN THE BLOCK.                  
*        THE CODE BELOW USES THE ADDRESS OF THE LAST DATE IN THE                
*        BLOCK TO POSITION TO THE APPROPRIATE 'NEXT' ENTRY IN                   
*        THE LIST.  BILL UHR  (FEB3/2006)                                       
*                                                                               
         OC    OVRNTDUB+4(4),OVRNTDUB+4                                         
*                                  ANY ADDRESS BACK FROM OVERNIGHTS?            
         BZ    GBOK0140            NO                                           
         L     R2,OVRNTDUB+4       YES - LAST A(OVRNT AVERAGE BLOCK)            
*                                     REPLACES R2, WHICH POINTS TO              
*                                     FIRST ITEM IN BLOCK                       
GBOK0140 EQU   *                                                                
         LA    R2,L'RFTCBKS(R2)    POINT TO NEXT BOOK                           
         CLI   RFTCBKS,X'FF'       PARM IS ADDRESS?                             
         BE    GBOK0020            YES                                          
*                                                                               
         LA    R0,RFTCBKS+(7*RFTCBKLQ)                                          
         CR    R2,R0                                                            
         BL    GBOK0020                                                         
*                                                                               
GBOK0160 DS    0H                                                               
         SR    R0,R0               SAFETY                                       
*                                                                               
*        HANDLE UPGRADE EXPRESSIONS                                             
*                                                                               
         ICM   R2,15,RFTCUPGA      SKIP IF NO UPGRADES AVAILABLE                
         BZ    GBOK0340                                                         
*                                                                               
GBOK0180 DS    0H                                                               
*                                                                               
*        R2==> BASE BOOK DESCRIPTION                                            
*                                                                               
         MVC   KEY,SVINVKEY        COPY INV REC KEY                             
*                                                                               
         ST    R2,ARAVLNEL         SAVE A(RAVLNEL)                              
*                                                                               
         LA    R3,L'RFTCBKS+6(R2)  POINT TO UPGRADE ELEMENT                     
         USING RAVLNEL,R3          ESTABLISH UPGRADE ELEMENT                    
*                                                                               
         CLI   RAVLNCOD,X'05'      DONE IF NOT AN UPGRADE ELEMENT               
         BNE   GBOK0320                                                         
*                                                                               
         ST    R2,RFTFUPGA         RETURN A(UPGRADE EXPRESSION)                 
*                                                                               
*        VALIDATE STATION/BOOK                                                  
*                                                                               
         LA    R8,DBLOCKA1                                                      
         USING DBLOCKD,R8                                                       
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         MVC   DBSELAGY,RFTCREP                                                 
         MVC   DBFILE,=C'TPT'                                                   
*                                                                               
         MVC   DBAREC,RFTAIO2          A(DEMO WORK AREA)                        
*                                                                               
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION BOOK                        
         MVC   DBCOMFCS,RFTACOM                                                 
         MVC   DBSELSRC,RFTCSRC    SET SOURCE FIELD                             
         MVC   DBSELBK,RFTCBKYR-RFTCBKS(R2)   SET BOOK                          
*                                                                               
         CLI   RFTCBKSV-RFTCBKS(R2),C'O'                                        
         BE    *+10                                                             
         MVC   DBBTYPE,RFTCBKSV-RFTCBKS(R2)   BOOK TYPE (SURVEY)                
*                                                                               
         MVC   DBSELSTA,RINVKSTA                                                
*                                                                               
*   HAS AN ALIAS FOR THIS STATION BEEN FOUND?                                   
*                                                                               
         OC    SVALISTA,SVALISTA                                                
         BZ    GBOK0200            NO                                           
         MVC   DBSELSTA,SVALISTA   YES - REPLACE STATION WITH ALIAS             
         MVC   DBSELALF,SVALIMKT   REPLACE / INSERT ALPHA MKT                   
*        CLI   DBBTYPE,C'W'        WIRED CABLE?                                 
*        BE    GBOK0200            YES - LEAVE IT ALONE                         
*        MVI   DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                        
GBOK0200 EQU   *                                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         TM    RFTCNTL,RFTCIMPQ    IF DEMOS TO BE BASED ON IMPS                 
         BO    *+8                                                              
         TM    RFTSRMPP+RMPIMPSB,RMPIMPSA                                       
         BNO   *+8                                                              
         MVI   DBTAPEP,C'Y'           TURN ON FLAG                              
*                                                                               
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKFI   IF INVENTORY BOOK                
         BNE   *+14                                                             
         MVC   DBSELAGY,RFTPAREP                                                
         B     GBOK0220                        DON'T VALIDATE                   
*                                                                               
         GOTO1 =A(CHKOVRNT),DMCB,(R8),RR=RELO                                   
         GOTO1 VDEMAND,DMCB,DBLOCK,0,0                                          
*                                                                               
*   IF DOING OVERNIGHT UPGRADE, EXPECT AN ERROR HERE !!                         
*                                                                               
         CLC   =X'5A0C',1(R2)      SHARE BOOK = DEC/90?                         
         BE    GBOK0220            SKIP ERROR TEST                              
*                                                                               
         CLI   DBERROR,0           EXIT ON ERROR                                
         BNE   GBOK0280                                                         
*                                                                               
*        VALIDATE POSSIBLE SECOND BOOK                                          
*                                                                               
         CLC   RAVLNOP1,=H'500'    SKIP IF SECOND FACTOR IS NOT A BOOK          
         BNH   GBOK0220                                                         
*                                                                               
         MVC   DBSELBK,RAVLNOP1    SET FOR SECOND BOOK                          
                                                                                
* WHEN VALIDATING SECOND UPGRADE BOOK - (PUT BOOK)                              
* DONT WORRY ABOUT LIVE +  DELAY VIEWING STREAMS.  WE SHOULD VALIDATE           
* AGAINST STANDARD+7 STREAM. WE HAVE BOOKTYPE TRANSPARENCY CODE IN THE          
* DEMO SYSTEM TO SWITCH THE BOOKTYPES IN THE DEMOS SYSTEM.                      
                                                                                
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
                                                                                
         CLI   DBBTYPE,BOOKTYPE_HS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
                                                                                
         CLI   DBBTYPE,BOOKTYPE_WS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_W3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'U'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'W'                                                     
                                                                                
         CLI   DBBTYPE,BOOKTYPE_C3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,C'Z'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'C'                                                     
                                                                                
*                                                                               
         GOTO1 =A(CHKOVRNT),DMCB,(R8),RR=RELO                                   
         GOTO1 VDEMAND,DMCB,DBLOCK,0,0                                          
                                                                                
* RESET BOOKTYPE                                                                
         CLI   RFTCBKSV-RFTCBKS(R2),C'O'                                        
         BE    *+10                                                             
         MVC   DBBTYPE,RFTCBKSV-RFTCBKS(R2)   BOOK TYPE (SURVEY)                
*                                                                               
         CLI   DBERROR,0           EXIT ON ERROR                                
         BNE   GBOK0280                                                         
*                                                                               
GBOK0220 DS    0H                                                               
*                                                                               
         GOTO1 =A(GETUPG),DMCB,(R2),RR=RELO   GET UPGRADE                       
*                                                                               
         CLI   HITCNT,0            SKIP IF NO PROGRAMS FOUND                    
         BE    GBOK0280                                                         
*                                                                               
         CLI   DBERROR,0           SKIP IF DEMO LOOK UP ERROR                   
         BNE   GBOK0280                                                         
*                                                                               
         BAS   RE,DEMDATA          GET DEMO DATA                                
*                                                                               
*        RETRIEVE SHARE AND HUT/PUT VALUES IF POSSIBLE                          
*                                                                               
         TM    RFTCNTL,RFTCSLVQ    SKIP IF SHARES AND LEVELS NOT WANTED         
         BNO   GBOK0060                                                         
*                                                                               
         CLI   MULTSW,C'Y'         SKIP IF COMBINATION BOOK                     
         BE    GBOK0260                                                         
*                                                                               
         CLC   SVPGMCD,=C'PA'      IF PROGRAM CODE IS PA, PRINT SHR/LVL         
         BE    GBOK0240                                                         
*                                                                               
         TM    RFTSRMPP+RESMASTB,RESMHPTA      PRINT E/P SHARES?                
         BZ    GBOK0240                        YES                              
*                                                                               
         CLC   SVPGMCD,=C'ES'      SKIP IF PROGRAM CODE IS ES                   
         BE    GBOK0260                                                         
*                                                                               
         CLC   SVPGMCD,=C'PE'      OR PE                                        
         BE    GBOK0260                                                         
*                                                                               
*****    CLC   SVPGMCD,=C'PJ'      OR PJ                                        
*****    BE    GBOK0260                                                         
*                                                                               
GBOK0240 DS    0H                                                               
*                                                                               
         GOTO1 =A(GTSHRLVL),RR=RELO   GO FIND THEM                              
*                                                                               
GBOK0260 DS    0H                                                               
*                                                                               
*        PRINT UPGRADE EXPRESSION                                               
*                                                                               
         TM    PRINTOPT,PROPTUGQ   IF THERE IS ANY UPGRADE                      
         BZ    *+8                                                              
         BAS   RE,UPPRNT              PRINT UPGRADE EXPRESSION                  
*                                                                               
*        PRINT FOOTNOTES                                                        
*                                                                               
         MVI   OPTION,C'A'         INDICATE AUTO FOOTNOTES WANTED               
*                                                                               
         TM    RFTSRMPP+RMPFTNTB,RMPFTNTA SKIP IF SUPPRESSING FOOTNOTES         
         BNO   *+8                                                              
         MVI   OPTION,C'X'                 INDICATE NO FOOTNOTES WANTED         
*                                                                               
*        PRINT HISTORICAL DATA                                                  
*                                                                               
         GOTO1 =A(HISTDATA),RR=RELO FIND HISTORICAL DATA                        
*                                                                               
         GOTO1 =A(FMTTXT),RR=RELO  RETURN ANY AUTO FOOTNOTES                    
*                                                                               
         MVI   RFTMODE,RFTNBKQ     INDICATE NEW BOOK                            
*                                                                               
*                                                                               
         XC    RFTFBK,RFTFBK       CLEAR BOOK                                   
         MVC   RFTFUPGA,ARAVLNEL   PASS A(UPGRADE EXPRESION)                    
*                                                                               
         BAS   RE,HOOK             HOOK TO CALLER                               
*                                                                               
GBOK0280 DS    0H                                                               
         XC    RFTFDDAT(256),RFTFDDAT   CLEAR DEMO DATA AREAS                   
         XC    RFTFDDAT+256(RFTFDDAL-256),RFTFDDAT+256                          
*                                                                               
         BAS   RE,CLEARDEM         CLEAR EXTENDED DEMOS                         
*                                                                               
         XC    RFTFTDAT(RFTFTDAL),RFTFTDAT   CLEAR TEXT DATA AREAS              
*                                                                               
         CLI   RFTRETRN,RFTRNXTQ   DONE IF USER REQUESTS NEW CATEGORY           
         BE    *+8                                                              
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS END                    
         BE    GBOK0320                                                         
*                                                                               
GBOK0300 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RAVLNLEN         GET ELEMENT LENGTH                           
*                                                                               
         TM    RFTFUPEX,X'80'      EXPANDED UPGRADE ELT IN USE?                 
         BNO   GBOK0310            NO  -                                        
         LA    RF,XUPGRADE(RF)     YES -  ADD EXTRA SPACE USE                   
GBOK0310 DS    0H                                                               
         LA    R2,L'RFTCBKS+6(RF,R2) BUMP TO NEXT ELEMENT                       
         B     GBOK0180                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
GBOK0320 DS    0H                                                               
*                                                                               
GBOK0340 DS    0H                                                               
*                                                                               
GBOK0900 DS    0H                                                               
         XIT1                                                                   
         DS    0F                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - CLEARDEM'                
***********************************************************************         
*                                                                     *         
*        CLEAR EXTENDED DEMO DATA RETURN AREAS                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLEARDEM NTR1                                                                   
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   CLRDEMX                                                          
*                                                                               
         ZICM  R0,RFTCDMRT,4       GET DEMO RETURN BLOCK                        
         LR    RE,R0               COPY START OF LIST                           
*                                                                               
         ZICM  R1,RFTCDMNM,2       GET THE COUNT                                
         MHI   R1,4                4 BYTES FOR EACH DEMO                        
         SR    RF,RF               ZERO LENGTH FROM                             
*                                                                               
         MVCL  R0,RE               CLEAR DEMOS                                  
*                                                                               
         ZICM  R0,RFTCDMSH,4       GET SHARE RETURN BLOCK                       
         MVCL  R0,RE               CLEAR                                        
*                                                                               
         ZICM  R0,RFTCDMLV,4       GET LEVEL RETURN BLOCK                       
         MVCL  R0,RE               CLEAR                                        
*                                                                               
CLRDEMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - DEMDATA'                 
***********************************************************************         
*                                                                     *         
*        RETRIEVE DEMO DATA FROM INVENTORY RECORD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DEMDATA  NTR1                                                                   
*                                                                               
         MVI   MULTSW,C'N'         INIT MULTI BOOK SWITCH                       
*                                                                               
         XC    SVPGMCD,SVPGMCD     INIT PROGRAM CODE                            
*                                                                               
         L     R6,RFTAIO2          CHECK FOR PROGRAM/BREAK CODE ELEMENT         
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMDATAX                                                         
*                                                                               
         USING RINVCEL,R6                                                       
*                                                                               
         MVC   SVPGMCD,RINVCODE    SAVE PROGRAM/BREAK CODE                      
*                                                                               
         TM    RINVCTYP,X'80'                                                   
         BNO   *+8                                                              
         MVI   MULTSW,C'Y'                                                      
*                                                                               
         NI    PRINTOPT,X'FF'-PROPTUGQ TURN OFF UPGRADE INDICATOR               
*                                                                               
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,PROPTUGQ   INDICATE AN UPGRADE WAS FOUND                
*                                                                               
         LA    R5,RFTFDEMS         POINT AT OUTPUT FOR DEMO VALUES              
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   DEMDATA1                                                         
*                                                                               
         ZICM  R5,RFTCDMRT,4                                                    
*                                                                               
DEMDATA1 DS    0H                                                               
         GOTO1 =A(GETDEMO),RR=RELO                                              
*                                                                               
DEMDATAX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - COMMON'                  
***********************************************************************         
*                                                                     *         
*        COMMON ROUTINES                                              *         
*              CONSTANTS                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COMMON   DS    0D                                                               
*                                                                               
VRECUP   DC    V(RECUP)                                                         
VUPOUT   DC    V(UPOUT)                                                         
VGETKSRC DC    V(GETKSRC)                                                       
*                                                                               
TYPTAB   DC    C'TTSA.'                                                         
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
OFORMATI DC    C'IUNUIUN',X'5A0B00'                                             
INDEX    DC    C'&&',X'FF'            INDEX UPGRADE MARKER                      
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PASS CONTROL TO USER HOOK ROUTINE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1                                                                   
*                                                                               
         MVI   RFTRETRN,0          CLEAR CALLER'S RETURN CODE                   
*                                                                               
         L     RF,RFTHOOKA                                                      
         L     RE,USERRD           USER RD                                      
         LM    R1,RC,24(RE)        USERS R1-RC                                  
         BASR  RE,RF                                                            
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)        RESTORE REFETCH'S REGS                       
*                                                                               
         MVC   ABUFF,ABUFFSTR      CLEAR TEXT AREAS                             
*                                                                               
         BR    RE                                                               
*                                                                               
SPACES   DC    CL160' '                                                         
PATCH    DC    XL64'00'                                                         
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - I/O'                     
***********************************************************************         
*                                                                     *         
*        I/O ROUTINES                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
READ     NTR1                      READ DIRECTORY POINTER                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY                      
         CLI   DMCB+8,0            MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DMEXIT   XIT1                                                                   
*                                                                               
READ2    NTR1                      READ DIRECTORY POINTER                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY                      
         CLI   DMCB+8,0            MUST FIND RECORD                             
         BE    READ2020            FOUND                                        
         LTR   RB,RB               SET CC NOT ZERO                              
         B     DMEXIT2                                                          
READ2020 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
*                                                                               
DMEXIT2  XIT1                                                                   
*                                                                               
                                                                                
HIGH     NTR1                      READ KEY EQUAL OR HIGH                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                      
         TM    DMCB+8,X'10'        RECORD NOT FOUND OKAY                        
         BO    *+14                                                             
         CLI   DMCB+8,0            MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DMEXIT                                                           
*                                                                               
SEQ      NTR1                      READ SEQUENTIAL                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEY,KEY                      
         TM    DMCB+8,X'10'        RECORD NOT FOUND OKAY                        
         BO    *+14                                                             
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DMEXIT                                                           
*                                                                               
GETREC   NTR1                      READ RECORD INTO CORE                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,AIO,        X        
               DMWORK                                                           
         CLI   DMCB+8,0            MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DMEXIT                                                           
*                                                                               
GETRECX  NTR1                      READ RECORD INTO CORE                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,AIO,        X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    DMEQX               RECORD FOUND RETURN CC EQUAL                 
         CLI   DMCB+8,X'10'                                                     
         BE    DMNEQX              RECORD NOT FOUND RETURN CC NE                
         DC    H'0'                                                             
*                                                                               
DMEQX    DS    0H                                                               
         CR    RB,RB                                                            
         B     DMEXIT                                                           
DMNEQX   DS    0H                                                               
         LTR   RB,RB                                                            
         B     DMEXIT                                                           
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GETEL'                   
***********************************************************************         
*                                                                     *         
*        FIND ELEMENT OF TYPE IN RECORD                               *         
*                                                                     *         
*NTRY    R6 ==> RECORD TO SEARCH OR LAST ELEMENT FOUND                *         
*        ELCODE = XL1(ELEMENT ID WANTED)                              *         
*                                                                     *         
*EXIT    R6 ==> ELEMENT FOUND OR NULLS                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - EFFDEMS'                 
***********************************************************************         
*                                                                     *         
*        WORK OUT EFFECTIVE DEMO TYPES FOR DIFFERENT LINES            *         
*        -------------------------------------------------            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EFFDEMS  NTR1                                                                   
*                                                                               
         LA    R3,RFTCDEMS                POINT TO LIST OF DEMOS                
         LA    R0,RFTCDEMS+(RFTCNDQ*3)    END OF LIST                           
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   EFFDEM1L                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R0,RFTCDMNM,2       GET THE COUNT                                
         MHI   R0,3                LENGTH OF DEMOS                              
         AR    R0,R3               END OF DEMO LIST                             
*                                                                               
EFFDEM1L DS    0H                                                               
*                                                                               
         OC    0(3,R3),0(R3)      CHECK FOR END OF DEMOS                        
         BZ    EFFDEM1D                                                         
*                                                                               
         MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
*                                                                               
         LA    R1,EFFTABLE                                                      
*                                                                               
EFFDEM2L DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR EOT                                
         BE    EFFDEM2D                                                         
*                                                                               
                                                                                
         CLC   WORK(2),0(R1)       MATCH TO TABLE ENTRY                         
         BE    EFFDEM2D                                                         
*                                                                               
EFFDEM2C DS    0H                                                               
*                                                                               
         LA    R1,3(R1)            POINT TO NEXT ENTRY IN TABLE                 
         B     EFFDEM2L                                                         
*                                                                               
EFFDEM2D DS    0H                                                               
*                                                                               
         MVC   1(1,R3),2(R1)       SUBSTITUTE TYPE                              
*                                                                               
EFFDEM1C DS    0H                                                               
*                                                                               
         LA    R3,3(R3)            POINT TO NEXT DEMO IN LIST                   
         CR    R3,R0                                                            
         BL    EFFDEM1L                                                         
*                                                                               
EFFDEM1D DS    0H                                                               
*                                                                               
EFFDEMSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
EFFTABLE DS    0H                                                               
         DC    C'TRR'                                                           
         DC    C'TSX'                                                           
         DC    C'TLQ'                                                           
         DC    C'RSS'                                                           
         DC    C'RLP'                                                           
         DC    C'T2S'                                                           
         DC    C'T4P'                                                           
         DC    X'FFFF',C'-'                                                     
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - UPPRNT'                  
***********************************************************************         
*                                                                     *         
*              PRINT UPGRADE COMMENTS                                 *         
*              ----------------------                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
UPPRNT   NTR1                                                                   
*                                                                               
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,X'05'        ANY UPGRADE ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   UPPRNTX                                                          
*                                                                               
* BOOK CODE CHECK AND HISTORY ELEMENT CHECKS ADDED TO MAINTAIN                  
* CONSISTENT LOGIC WITH INV MASTER REPORT (FJD 1/29/01)                         
*                                                                               
         CLC   SVPGMCD,=C'PJ'      ONLY SHOW UPGRADE IF                         
         BE    UPPRNT30            CODE IS PJ                                   
                                                                                
         TM    RFTSRMPP+RMPESUPB,RMPESUPA  (CHECK PROFILE)                      
         BNO   UPPRNTX                                                          
                                                                                
         CLC   SVPGMCD,=C'ES'      OR IF CODE IS ES AND PROFILE                 
         BNE   UPPRNTX             BIT IS ON                                    
*                                                                               
UPPRNT30 DS    0H                 SUPPRESS UPGD FOOTNT IF >1 SHARE BK           
         ST    R6,AUPELEM          SAVE A(UPGRADE ELEMENT)                      
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,RIDHCDQ      LOOK FOR DEMO HISTORY ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   UPPRNT50                                                         
                                                                                
         USING RIDHEL,R6                                                        
         MVC   SHRSRC,RIDHSRC      REMEMBER RATING SERVICE,                     
         MVC   SHRBK,RIDHBK         BOOK                                        
         MVC   SHRBTYP,RIDHBTYP     AND BOOKTYPE OF 1ST SHARE BOOK              
         DROP  R6                                                               
                                                                                
UPPRNT40 DS    0H                  MATCH AGAINST SUBSEQUENT SHARE BOOKS         
         BAS   RE,NEXTEL                                                        
         BNE   UPPRNT50                                                         
         USING RIDHEL,R6                                                        
         CLC   SHRSRC,RIDHSRC                                                   
         BNE   UPPRNT45                                                         
         CLC   SHRBK,RIDHBK                                                     
         BNE   UPPRNT45                                                         
         CLC   SHRBTYP,RIDHBTYP                                                 
         BNE   UPPRNT45                                                         
         B     UPPRNT40                                                         
         DROP  R6                                                               
                                                                                
UPPRNT45 DS    0H                  SKIP UPGRADE EXPRESSION EDIT                 
         B     UPPRNTX                                                          
                                                                                
UPPRNT50 EQU   *                                                                
         L     R6,AUPELEM          RESTORE R6 TO UPGRADE ELEMENT                
*                                                                               
         GOTO1 VUPOUT,DMCB,(R6),RFTFUPGR,RR=RELO EDIT UPGRADE                   
*                                                                               
         LA    R3,RFTFUPGR         SET A(RFTFUPGR)                              
         GOTO1 =A(PEEPMTRS),RR=RELO                                             
*                                  CORRECT UPGRADE FOOTNOTE, IF NEEDED          
*                                                                               
UPPRNTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - LTORG'                   
***********************************************************************         
*                                                                     *         
*        LITERAL POOL                                                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
MONTHS   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - FMTTXT'                  
***********************************************************************         
*                                                                     *         
*        FORMAT TEXT DATA FOR RETURN                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FMTTXT   NTR1  BASE=*                                                           
*                                                                               
         MVI   SVWRPOPT,0          INIT WRAP OPTION                             
*                                                                               
         L     R3,ABUFF            START OF NEXT AVAILABLE BUFFER LINE          
         ST    R3,RFTFTXTA         RETURN ADDRESS                               
         ST    R3,RFTFTX1A         RETURN ADDRESS OF 1ST LINE OF TEXT           
         MVI   RFTFTXTN,0          INIT NUMBER OF LINES OF TEXT                 
         MVI   RFTFTX1N,0          INIT NUMBER OF LINES OF TEXT                 
*                                                                               
         MVC   0(132,R3),SPACES    INIT FIRST LINE OF TEXT                      
         SR    R2,R2               LINE COUNTER                                 
*                                                                               
         CLI   OPTION,C'X'         GET OUT IF NO TEXT WANTED                    
         BE    FMTTXTX                                                          
*                                                                               
         L     R6,RFTAIO2          POINT TO RECORD WITH TEXT                    
*                                                                               
         MVI   ELCODE,X'02'        IS A FILTER APPLICABLE                       
         BAS   RE,GETEL                                                         
         BNE   FTXFLTX                                                          
*                                                                               
         USING RINVFEL,R6          ESTABLISH FILTER ELEMENT                     
*                                                                               
         MVC   SVWRPOPT,RINVFWRP   SAVE WRAP OPTION                             
         MVC   RFTFTXFL,RINVFWRP   RETURN WRAP OPTION TO CALLER                 
*                                                                               
*        FILTER ON LOCAL STATUS                                                 
*                                                                               
         CLC   RFTCREP,RFTCREP     IF NOT LOCAL REP                             
         BNE   FTXLCLX                                                          
*                                                                               
         CLI   RINVFLOC,C'Y'       AND TEXT IS LOCAL ONLY                       
         BE    FMTTXTX                 THEN DON'T WANT IT FOR MAIN REP          
*                                                                               
FTXLCLX  DS    0H                                                               
*                                                                               
*        FILTER ON DEMO SOURCE                                                  
*                                                                               
         CLI   RINVFSRC,0          IF TEXT CONDITIONAL ON SOURCE                
         BE    FTXSRCX                                                          
*                                                                               
         CLC   RFTCSRC,RINVFSRC       THEN DEMOS' SOURCES MUST MATCH            
         BNE   FMTTXTX                                                          
*                                                                               
         OI    PRINTOPT,PROPTXFQ      SET TO PRINT TEXT FILTER                  
*                                                                               
FTXSRCX  DS    0H                                                               
*                                                                               
*        FILTER ON DEMO BOOKS                                                   
*                                                                               
         OC    RINVFBK,RINVFBK     SKIP IF TEXT NOT DEPENDENT ON BOOKS          
         BZ    FTXBKX                                                           
*                                                                               
*        FIND KSRC                                                              
*                                                                               
         XC    GFRCIN(GFRCINL),GFRCIN INIT GETKSRC PARAMETERS                   
         XC    GFRCOUT(GFRCOUTL),GFRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GFIRSVC,RFTCSRC                SET RATING SERVICE                
         MVC   GFIBITS,RINVFBKT               SET BOOKVAL BITS                  
         MVC   GFIBKTYP,RINVFBTP              SET BOOK TYPE                     
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GFRCIN),GFRCOUT,RFTACOM,RR=RELO              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,RFTCBKS          MAX 7 BOOKS                                  
         CLI   0(R1),X'FF'         ADDRESS PARM?                                
         BNE   *+8                 NO                                           
         ICM   R1,15,1(R1)                                                      
*                                                                               
FTXBKLP  DS    0H                                                               
*                                                                               
         CLC   RINVFBK,RFTCBKYR-RFTCBKS(R1) MATCH BOOK YEAR/MONTH               
         BNE   FTXBKCN                                                          
*                                                                               
*        CLC   GFOBITS,RFTCBKVL-RFTCBKS(R1)  MATCH BOOKVAL BITS                 
         CLC   RINVFBKT,RFTCBKVL-RFTCBKS(R1)  MATCH BOOKVAL BITS                
         BNE   FTXBKCN                                                          
*                                                                               
         CLI   GFIBKTYP,0          SKIP IF NO BOOK TYPE FILTER                  
                                                                                
         CLI   RFTCBKSV-RFTCBKS(R1),0    AND NO BOOK TYPE ASKED FOR             
         BNH   FTXBKFD                                                          
*                                                                               
         CLC   GFIBKTYP,RFTCBKSV-RFTCBKS(R1) MATCH BOOKTYPE                     
         BE    FTXBKFD                                                          
*                                                                               
FTXBKCN  DS    0H                                                               
         LA    R1,L'RFTCBKS(R1)        POINT TO NEXT BOOK                       
         CLI   RFTCBKS,X'FF'           PARM IS ADDRESS?                         
         BNE   *+18                    NO                                       
         OC    0(L'RFTCBKS,R1),0(R1)   DONE IF END OF BOOKS REACHED             
         BNZ   FTXBKLP                                                          
         B     FTXBKDN                                                          
*                                                                               
         LA    R0,RFTCBKS+(7*RFTCBKLQ)                                          
         CR    R1,R0                                                            
         BL    FTXBKLP                                                          
*                                                                               
FTXBKDN  DS    0H                  FAILS BOOK FILTER                            
         B     FMTTXTX                                                          
*                                                                               
FTXBKFD  DS    0H                  PASSES BOOK FILTER                           
*                                                                               
         OI    PRINTOPT,PROPTXFQ   PRINT TEXT FILTER                            
*                                                                               
FTXBKX   DS    0H                                                               
*                                                                               
*        FILTER ON SPECIFIC DEMOS                                               
*                                                                               
FTXDEM   DS    0H                  DEMO FILTERS                                 
*                                                                               
         ZIC   R0,RINVFLEN         DEMO FILTERS                                 
         SH    R0,=H'10'                                                        
         BNP   FTXDEMX             NO DEMO FILTERS                              
*                                                                               
*                                  R0 HAS NUMBER OF DEMO CODES                  
         LA    R1,RINVFDEM         START OF DEMO CODES IN FILTER                
*                                                                               
FTXDEM1L DS    0H                                                               
*                                                                               
         LA    R4,RFTCDEMS         REQUESTED DEMOS                              
         LA    RE,RFTCDEMS+(RFTCNDQ*3)   END OF DEMO LIST                       
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   FTXDEM2L                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R0,RFTCDMNM,2       GET THE COUNT                                
         MHI   R0,3                LENGTH OF DEMOS                              
         AR    R0,R3               END OF DEMO LIST                             
*                                                                               
FTXDEM2L DS    0H                                                               
*                                                                               
         CLC   0(1,R1),2(R4)       MATCH FILTER DEMO TO REQUESTED DEMO          
         BE    FTXDEMFD                                                         
*                                                                               
FTXDEM2C DS    0H                                                               
*                                                                               
         LA    R4,3(R4)            BUMP TO NEXT DEMO                            
         CR    R4,RE                                                            
         BL    FTXDEM2L                                                         
*                                                                               
FTXDEM2D DS    0H                                                               
*                                                                               
FTXDEM1C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)                                                         
         BCT   R0,FTXDEM1L                                                      
*                                                                               
FTXDEM1D DS    0H                                                               
*                                                                               
         B     FMTTXTX             FAILS DEMO FILTERING                         
*                                                                               
FTXDEMFD DS    0H                                                               
*                                                                               
         OI    PRINTOPT,PROPTXFQ   SET TO PRINT FILTERS                         
*                                                                               
FTXDEMX  DS    0H                                                               
*                                                                               
*        PRINT ANY TEXT FILTERS                                                 
*                                                                               
         TM    PRINTOPT,PROPTXFQ   ANY FILTERS TO PRINT                         
         BNO   FTXFLTX                                                          
*                                                                               
         MVC   4(12,R3),=C'TEXT FILTERS'                                        
         LA    R4,20(R3)                                                        
*                                                                               
         CLI   RINVFSRC,0          SKIP IF NO SOURCE FILTER                     
         BE    FTXPSRCX                                                         
*                                                                               
         MVC   0(7,R4),=C'SOURCE='                                              
         MVC   7(1,R4),RINVFSRC                                                 
         LA    R4,11(R4)                                                        
*                                                                               
FTXPSRCX DS    0H                                                               
*                                                                               
         OC    RINVFBK,RINVFBK     SKIP IF NO BOOK FILTERS                      
         BZ    FTXPBKX                                                          
*                                                                               
         MVC   0(5,R4),=C'BOOK='                                                
         LA    R5,5(R4)                                                         
*                                                                               
         CLI   GFOQLF,C' '          PRINT QUALIFIER IF NON BLANK                
         BNH   *+14                                                             
         MVC   0(1,R5),GFOQLF                                                   
         LA    R5,1(R5)                                                         
*                                                                               
FTXPBTPX DS    0H                                                               
*                                                                               
         ZIC   R1,RINVFMN          PRINT BOOK MMM/YY                            
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
         MVI   3(R5),C'/'                                                       
         EDIT  (1,RINVFYR),(2,4(R5))                                            
*                                                                               
         LA    R5,6(R5)                                                         
*                                                                               
         CLI   GFOBKTYP,C' '       PRINT BOOK TYPE IF NON BLANK                 
         BNH   *+22                                                             
         MVI   0(R5),C'('                                                       
         MVC   1(1,R5),GFOBKTYP                                                 
*                                                                               
         CLI   GFOBKTYP+1,C' '     IF 2 CHARACTERS                              
         BNH   *+14                                                             
         MVC   2(1,R5),GFOBKTYP+1     MOVE SECOND BYTE                          
         LA    R5,1(R5)               BUMP POINTER                              
*                                                                               
         MVI   2(R5),C')'                                                       
         LA    R5,3(R5)                                                         
*                                                                               
         LA    R4,16(R4)                                                        
*                                                                               
FTXPBKX  DS    0H                                                               
*                                                                               
*        PRINT OUT TEXT DEMO FILTERS                                            
*                                                                               
         ZIC   R0,RINVFLEN         CALCULATE NUMBER OF DEMO FILTERS             
         SH    R0,=H'10'                                                        
         BNP   FTXPTXFX            NONE                                         
*                                                                               
         MVC   0(6,R4),=C'DEMOS='                                               
         LA    R4,6(R4)                                                         
*                                                                               
         LA    R8,DBLOCKA1                                                      
         USING DBLOCKD,R8                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,RFTACOM                                                 
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         LA    R1,RINVFDEM         START OF DEMO FILTERS                        
*                                                                               
FTXPTXFL DS    0H                                                               
*                                                                               
         ST    R1,FULL    SAVE WHERE WE ARE IN RINVFDEM LIST                    
*                                                                               
         MVI   WORK,0                                                           
         MVI   WORK+1,C'I'                                                      
         MVC   WORK+2(1),0(R1)                                                  
*                                                                               
         GOTO1 VDEMOCON,DMCB,(0,WORK),(6,0(R4)),(0,DBLOCKD)                     
*                                                                               
         DROP  R8                                                               
*                                                                               
         LA    R4,6(R4)            POINT TO NEXT PRINT AREA                     
*                                                                               
FTXPTXFC DS    0H                                                               
*                                                                               
         CLI   0(R4),C' '          FIND NEXT PRINT SPOT                         
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R4),C','          SET COMMA                                    
         LA    R4,2(R4)            NEXT DEMO PRINT AREA                         
*                                                                               
         L     R1,FULL             POINT TO NEXT DEMO                           
         LA    R1,1(R1)                                                         
         SH    R0,=H'1'            DECREMENT DEMO COUNTER                       
         BP    FTXPTXFL                                                         
*                                                                               
FTXPTXFD DS    0H                                                               
*                                                                               
         BCTR  R4,0                ELIMINATE LAST COMMA                         
*                                                                               
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
*                                                                               
FTXPTXFX DS    0H                                                               
*                                                                               
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES    SPACE BETWEEN FILTERS & TEXT                 
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES                                                 
         LA    R2,2(R2)                                                         
*                                                                               
FTXFLTX  DS    0H                                                               
*                                                                               
*        PRINT LINES OF TEXT                                                    
*                                                                               
         ST    R3,RFTFTX1A         RETURN ADDRESS 1ST TEXT LINE                 
         STC   R2,RFTFTX1N         SAVE CURRENT LINE NUMBER                     
*                                                                               
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,X'01'        TEXT ELEMENT ID                              
*                                                                               
         LR    R5,R3               COPY START OF PRINT AREA                     
*                                                                               
         CLI   OPTION,C'A'         SKIP IDENTIFIER IF FOOTNOTE                  
*******  BE    FTXPTIDX                                                         
         B     FTXPTIDX            ALWAYS SKIP TEXT ID                          
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   0(4,R3),=C'TEXT'                                                 
*                                                                               
         CLI   OPTION,C'M'                                                      
         BNE   *+10                                                             
         MVC   0(4,R3),=C'MKT.'                                                 
*                                                                               
         CLI   OPTION,C'S'                                                      
         BNE   *+10                                                             
         MVC   0(4,R3),=C'STN.'                                                 
*                                                                               
         EDIT  (2,RINVKTXT),(5,5(R3)),ALIGN=LEFT                                
*                                                                               
*        FIND END OF TEXT NUMBER                                                
*                                                                               
         LA    R5,6(R3)                                                         
*                                                                               
         CLC   0(3,R5),SPACES                                                   
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     *-14                                                             
*                                                                               
         LA    R5,1(R5)            LEAVE SPACE                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
FTXPTIDX DS    0H                                                               
*                                                                               
*        PRINT LINES OF TEXT                                                    
*                                                                               
         CLI   SVWRPOPT,C'Y'       IF USING WORD WRAP                           
         BNE   *+12                                                             
         BAS   RE,FTXWRP              USE SPECIAL CODE                          
         B     FTXTXTX                GO DO PRINTING                            
*                                                                               
         USING RINVTEL,R6                                                       
*                                                                               
         BAS   RE,GETEL            FIND FIRST LINE OF TEXT                      
*                                                                               
FTXTXTLP DS    0H                                                               
*                                                                               
         BNE   FTXTXTDN            END OF TEXT                                  
*                                                                               
         LA    RE,RINVTEXT         START OF TEXT IN ELEMENT                     
         ZIC   R1,RINVTLEN         TEXT EXECUTE LENGTH                          
         SH    R1,=H'7'                                                         
         BM    FTXTXTCN            SKIP IF NO TEXT                              
*                                                                               
         CLI   OPTION,C'A'         IF FOOTNOTES                                 
         BNE   FTXTXT10                                                         
*                                                                               
         LA    R5,0(R3)                                                         
         LA    RE,5(RE)               REMOVE MMMYY FROM FOOTNOTE                
         SH    R1,=H'5'                                                         
         BM    FTXTXTCN               SKIP IF NO TEXT                           
*                                                                               
         CLI   0(RE),C'('             IF THERE IS A BOOKTYPE                    
         BNE   *+16                                                             
         LA    RE,3(RE)                  REMOVE IT ALSO                         
         SH    R1,=H'3'                                                         
         BM    FTXTXTCN                  SKIP IF NO TEXT                        
*                                                                               
         LA    RE,1(RE)               BYPASS SPACING                            
         SH    R1,=H'1'                                                         
         BM    FTXTXTCN                                                         
*                                                                               
FTXTXT10 DS    0H                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)                                                    
*                                                                               
FTXTXTCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES       INIT NEXT PRINT LINE                      
         LA    R5,132(R5)          UPDATE PRINT POSITION IN LINE                
         LA    R2,1(R2)            BUMP LINE COUNTER                            
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         B     FTXTXTLP                                                         
*                                                                               
FTXTXTDN DS    0H                                                               
*                                                                               
         ST    R3,ABUFF            SET A(END OF BUFFER)                         
*                                                                               
FTXTXTX  DS    0H                                                               
*                                                                               
         STC   R2,RFTFTXTN         SET NUMBER OF TEXT  & FILTER LINES           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RFTFTX1N         NUMBER OF FILTER LINES                       
         SR    R2,RF               NUMBER OF TEXT LINES                         
         STC   R2,RFTFTX1N                                                      
*                                                                               
FMTTXTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - FTXWRP'                  
***********************************************************************         
*                                                                     *         
*        ROUTINE FOR WORD WRAPPING. TEXT IS STORED                    *         
*        CONSECUTIVELY AND THEN CHOPPED                               *         
*        NEW LINE OCCURS WHEN NEXT ELEMENT STARTS WITH 2 SPACES       *         
*                                                                     *         
*NTRY  R3==> FIRST PRINT LINE IN BUFFER TO BE USED                    *         
*      R5==> FIRST POSITION ON LINE FOR PRINTING                      *         
*      R2==> NUMBER OF LINES IN BUFFER                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FTXWRP   NTR1                                                                   
*                                                                               
         L     R4,ATXTWRK          POINT TO TEXT WORKAREA                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R3               START OF PRINT AREA                          
         S     RF,ABUFFSTR         AREA USED SO FAR                             
         D     RE,=F'132'          NUMBER OF LINES USED                         
*                                                                               
         LA    R0,20               MAX 20 LINES                                 
         SR    R0,RF               NUMBER OF LINES AVAILABLE                    
         LR    RF,R0               SAVE COUNTER                                 
*                                                                               
         MVC   0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LR    R0,RF               RESTORE COUNTER                              
*                                                                               
         L     R4,ATXTWRK          POINT TO TEXT WORKAREA                       
*                                                                               
         BAS   RE,GETEL            FIND FIRST LINE OF TEXT                      
*                                                                               
TWRLOOP  BNE   TWRDONE             END OF TEXT                                  
*                                                                               
         USING RINVTEL,R6          ESTABLISH TEXT ELEMENT                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         TEXT ELEMENT LENGTH                          
         SH    RF,=H'7'            TEXT'S EXECUTE LENGTH                        
         BM    TWRCONT             MUST HAVE TEXT                               
*                                                                               
         CH    RF,=H'1'            MAX 2 POSITIONS CHECKED                      
         BNH   *+8                                                              
         LA    RF,1                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RINVTEXT(0),SPACES IF ELEMENT STARTS WITH SPACES                 
         BH    TWRLP20                                                          
*                                     NEED TO CHOP WHAT WE HAVE                 
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RF,ATXTWRK          START OF PRINT AREA                          
         SR    R4,RF               LENGTH OF TEXT TO PRINT                      
         BNP   TWRLP10             NOTHING TO PRINT                             
         ST    R4,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         CLI   RFTCTXTW,0          DEFAULT TEXT WIDTH IS 60                     
         BNE   *+8                                                              
         MVI   RFTCTXTW,60                                                      
*                                                                               
         GOTO1 VCHOPPER,DMCB,(0,ATXTWRK),(RFTCTXTW,(R5)),(C'P',(R0))            
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+8                                                              
         LA    R1,1                ALWAYS PRINT ONE LINE                        
*                                                                               
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
         SR    R0,R1               NUMBER OF LINES AVAILABLE                    
         BNP   TWRDN10             NO ROOM LEFT                                 
*                                                                               
*                                                                               
         LA    RF,132              PRINT LINE WIDTH                             
         MR    RE,R1               DISPLACEMENT USED                            
*                                                                               
         AR    R5,RF               UPDATE PRINT START                           
*                                                                               
TWRLP10  DS    0H                                                               
*                                                                               
         L     R4,ATXTWRK          RESET PRINT POINTERS                         
*                                                                               
TWRLP20  DS    0H                                                               
*                                                                               
         LA    R1,RINVTEXT         POINT TO START OF TEXT                       
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         TEXT ELEMENT LENGTH                          
         SH    RF,=H'7'            TEXT'S EXECUTE LENGTH                        
         BM    TWRCONT             MUST HAVE TEXT                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)       MOVE TEXT TO PRINTAREA                       
*                                                                               
TWRCONT  DS    0H                                                               
*                                                                               
         LA    R4,1(RF,R4)         NEXT PRINT AREA                              
         MVI   0(R4),C' '          PUT SPACE BETWEEN TEXT LINES                 
         LA    R4,1(R4)            BUMP POINTER                                 
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         B     TWRLOOP                                                          
*                                                                               
TWRDONE  DS    0H                                                               
*                                                                               
*        CHOP ANY REMAINING TEXT                                                
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RF,ATXTWRK          START OF PRINT AREA                          
         SR    R4,RF               LENGTH OF TEXT TO PRINT                      
         BZ    TWRDN10             NOTHING TO PRINT                             
         ST    R4,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         CLI   RFTCTXTW,0          DEFAULT TEXT WIDTH IS 60                     
         BNE   *+8                                                              
         MVI   RFTCTXTW,60                                                      
*                                                                               
         GOTO1 VCHOPPER,DMCB,(0,ATXTWRK),(RFTCTXTW,(R5)),(C'P',(R0))            
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+6                 MUST HAVE PRINTED SOMETHING                  
         DC    H'0'                                                             
*                                                                               
         SR    R0,R1               NUMBER OF LINES LEFT                         
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
TWRDN10  DS    0H                                                               
*                                                                               
FTXWRPX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - HISTDATA'                
***********************************************************************         
*                                                                     *         
*        FORMAT ANY HISTORICAL TRANSFER DATA IN X'03' ELEMENT         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HISTDATA NTR1  BASE=*                                                           
*                                                                               
         L     R3,ABUFF            POINT TO CURRENT LINE IN BUFFER              
         ST    R3,RFTFFTNA         RETURN START ADDRESS                         
         MVI   RFTFFTNN,0          INIT NUMBER OF PRINT LINES                   
*                                                                               
         LA    R0,3                INIT THREE LINES OF BUFFER                   
         LR    RF,R3                                                            
*                                                                               
         MVC   0(132,RF),SPACES                                                 
         LA    RF,132(RF)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         CLI   OPTION,C'X'         EXIT IF FOOTNOTES NOT WANTED                 
         BE    HISTX                                                            
*                                                                               
         L     R6,RFTAIO2          FIND TRANSFER FROM ELEMENT                   
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   HISTX               NOT FOUND - SHOULD NEVER HAPPEN              
*                                                                               
         USING RINVFREL,R6         ESTABLISH TRANSFER FROM ELEMENT              
*                                                                               
         LR    R5,R3               A(START OF TEXT AREA)                        
         MVC   0(5,R5),RINVFRST    MOVE STATION                                 
         LA    R5,6(R5)            BUMP TEXT POINTER                            
*                                                                               
*        TRANSLATE REP SOURCE TO PRINTABLE FORMAT                               
*                                                                               
         XC    GFRCIN(GFRCINL),GFRCIN INIT GETKSRC PARAMETERS                   
         XC    GFRCOUT(GFRCOUTL),GFRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GFIRSVC,RFTCSRC                SET RATING SERVICE                
         MVC   GFIBITS,RINVFRBK               SET BOOKVAL BITS                  
         MVC   GFIBKTYP,RINVFRBT              SET BOOK TYPE                     
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GFRCIN),GFRCOUT,RFTACOM,RR=RELO              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   HISTX                                                            
*                                                                               
         CLI   GFOQLF,C' '         SKIP IF QUALIFIER IS BLANK                   
         BE    *+14                                                             
         MVC   0(1,R5),1(R2)            PRINT TRANSLATED SOURCE                 
         LA    R5,1(R5)                                                         
*                                                                               
         ZIC   R1,RINVFRBK+2               MONTH                                
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
*                                                                               
         MVI   3(R5),C'/'                                                       
         ZIC   R1,RINVFRBK+1                                                    
         EDIT  (R1),(2,4(R5))     YEAR                                          
         LA    R5,7(R5)                                                         
*                                                                               
         MVC   0(1,R5),RINVFRTY      TYPE (P/I)                                 
         MVC   1(1,R5),RINVFRPR      FUNCTION                                   
         MVC   2(2,R5),GFOBKTYP      BOOK TYPE                                  
         LA    R5,5(R5)                                                         
*                                                                               
         CLI   GFOBKTYP+1,C' '     BACK UP IF ONLY I CH                         
         BH    *+8                                                              
         SHI   R5,1                                                             
*                                                                               
         ZIC   R2,RINVFRLN        MOVE FROM DATA IF ANY                         
         SH    R2,=H'16'                                                        
         BZ    HISTX              NO OTHER DATA TO BE ADDED                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RFTCTXTW       GET TEXT PRINT WIDTH                         
         BNZ   *+8                                                              
         LA    RF,60               DEFAULT TO 60 WIDE                           
*                                                                               
         CR    R2,RF               WILL IT FIT ON ONE LINE?                     
         BNH   HIST45              YES                                          
*                                                                               
*                                  NO - CHOP                                    
         GOTO1 VCHOPPER,DMCB,((R2),RINVFRDT),((RF),(R5)),(C'P',3)               
*                                                                               
         L     RF,DMCB+8           NUMBER OF LINES USED                         
         LA    RF,0(RF)            CLEAR HOB                                    
*                                                                               
         STC   RF,RFTFFTNN         RETURN NUMBER OF PRINT LINES                 
*                                                                               
         LA    RE,132              BUFFER LINE LENGTH                           
         MR    RE,RE               INDEX TO NEXT AVAILABLE LINE                 
         LA    RE,0(RE,R3)         POINT TO NEXT LINE IN BUFFER                 
         ST    R3,ABUFF            SAVE ADDRESS                                 
*                                                                               
         B     HISTX                                                            
*                                                                               
HIST45   BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),RINVFRDT    MOVE OUT PRINT DATA                          
*                                                                               
         LA    R3,132(R3)          BUMP BUFFER POINTER                          
         ST    R3,ABUFF                                                         
         MVI   RFTFFTNN,1          RETURN NUMBER OF PRINT LINES                 
*                                                                               
HISTX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
***SETOVBKS                                                                     
*        BOOK LIST PASSED IN TO FETCH CONTAINS EITHER A SINGLE                  
*        REQUEST FOR TP / PAV OVERNIGHT DATA, OR A STRING OF                    
*        BOOKS FOR WHICH AN AVERAGE IS REQUESTED.  AN EXTENDED                  
*        BLOCK IS SET UP IN ALL CASES FOR THE DEMAND CALL.                      
*                                                                               
*        THE EXTENDED BLOCK IS COMPRESSED FOR ONE ENTRY PER                     
*        OVERNIGHT BOOK (FORMAT = 2 BYTES:  YY/WK#), WITH THE                   
*        DAYS OF WEEK FIELD REPRESENTING EACH DAY IN THE WEEK                   
*        FOR WHICH THERE IS AN ENTRY.                                           
*                                                                               
*        FURTHER, THE RESULTING DAYS OF THE WEEK BYTE IS 'AND'ED'               
*        WITH THE "DAYS" BYTE OF THE INVENTORY HEADER'S FIRST DAY               
*        / TIME ENTRY.  THIS RESTRICTS THE DATA RETRIEVED TO                    
*        ONLY THOSE DAYS REPRESENTED IN THE HEADER.                             
*        ****  THERE IS NO REQUIREMENT THAT THE OVERNIGHT DATES                 
*        ENTERED BY THE CLIENT WILL CORRESPOND WITH THE DAYS                    
*        SPECIFIED IN THE INVENTORY HEADER.                                     
*                                                                               
*        R2 -> BOOK(S) IN STRING.                                               
*                                                                               
SETOVBKS NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R2,0(R1)            RESET A(BOOK(S))                             
*                                                                               
         L     R3,RFTAIO1          SET A(INVENTORY HEADER)                      
         LA    R3,34(R3)           SET A(DESCRIPTION ELT OF RECORD)             
SOBK0010 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
         CLI   0(R3),2             DAY/TIME ELEMENT?                            
         BE    SOBK0015            YES                                          
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SOBK0010            GO BACK FOR NEXT ELEMENT                     
SOBK0015 EQU   *                                                                
         USING RIDTELEM,R3                                                      
         MVC   WORK+75(1),RIDTDAY  SAVE DAY BYTE FROM INV                       
         MVC   WORK+76(4),RIDTTIME SAVE TIME STRING FROM INV                    
         DROP  R3                                                               
*                                                                               
         LA    R8,DBLOCKA1                                                      
         USING DBLOCKD,R8                                                       
*                                                                               
         L     R1,DBEXTEND         GET NEXT EXTENSION ADDRESS                   
SOBK0020 DS    0H                                                               
         L     R1,DBXTNEXT-DBXTTID(R1)                                          
         USING DBBDTMD,R1          OVERNIGHT ROLLING AVERAGE                    
         CLC   DBBDTID,=C'BKDT'    BOOKDATE EXTENSION?                          
         BE    SOBK0040            YES - REUSE IT                               
         OC    DBBDTID(DBBDTNXT-DBBDTID),DBBDTID                                
*                                  NO  - SLOT ALREADY IN USE?                   
         BNZ   SOBK0020            YES - CHECK NEXT SLOT                        
*                                  NO  - SLOT IS EMPTY                          
SOBK0040 EQU   *                                                                
         XC    0(128,R1),0(R1)     CLEAR 128 BYTES                              
         MVC   DBBDTID,=C'BKDT'    INSERT DATA TYPE INDICATOR                   
         LR    RF,R1               CALCULATE NEXT EXTENSION AREA                
         LA    RF,128(RF)                                                       
         STCM  RF,15,DBBDTNXT      SAVE A(NEXT EXTEND AREA)                     
         LA    R3,DBBDTMS          SET A(FIRST DAY/TIME DATA)                   
*                                                                               
         DROP  R1                                                               
*                                                                               
*   FROM THIS POINT ON, R3 -> FIRST DAY/TIME DATA IN BKDT.                      
*        EACH CONVERTED DATE WILL BE ACCUMULATED IN ITS                         
*        APPROPRIATE SLOT BY WEEK, AND THE DAY FLAGS SET                        
*        IN THE SLOT'S DAY BYTE.                                                
*                                                                               
SOBK0050 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,(R2)),(0,WORK)                                   
*                                  SET DATE AS EBCDIC                           
         MVC   DMCB+4(4),VGETDAY   SET A(GETDAY)                                
         MVI   DMCB+4,1            SET START DAY OF WEEK                        
         GOTO1 VNSIWEEK,DMCB,WORK,,VADDAY,VDATCON                               
*                                                                               
         BAS   RE,OVNTSLOT         FIND PROPER OVERNIGHT SLOT                   
*                                                                               
*   OVNTSLOT RETURNS A(SLOT IN USE) IN DUB+4                                    
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                                                               
         GOTO1 VDAYPAK,DMCB,(3,WORK+6),WORK+12,WORK+13                          
*                                                                               
         L     RF,DUB+4            SET A(SLOT IN USE)                           
*                                                                               
         OC    2(1,RF),WORK+12     SET DAY OF WEEK                              
         MVC   3(4,RF),WORK+76     INSERT TIME STRING                           
*                                                                               
         CLI   3(R2),C'O'          OTP AND NOT BLOCK?                           
         BE    SOBK0220            YES - NO LOOP                                
         CLI   3(R2),C'V'          OPV AND NOT BLOCK?                           
         BE    SOBK0220            YES - NO LOOP                                
         CLI   3(R2),C'o'          OTP AND BLOCK?                               
         BE    SOBK0100            YES - LOOP                                   
         CLI   3(R2),C'v'          OPV AND BLOCK?                               
         BE    SOBK0100            YES - LOOP                                   
         CLI   3(R2),C'x'          END OF LOOP?                                 
         BE    SOBK0200            YES - CLEAN UP                               
         DC    H'0'                UNIDENTIFIED DATA                            
SOBK0100 EQU   *                                                                
         LA    R2,5(R2)            BUMP TO NEXT INPUT BOOK                      
         L     RF,OVRNTDUB                                                      
         LA    RF,1(RF)            INCREMENT EXTRA BOOK COUNT                   
         ST    RF,OVRNTDUB                                                      
         B     SOBK0050            PROCESS NEXT BOOK                            
SOBK0200 EQU   *                                                                
         ST    R2,OVRNTDUB+4       SAVE A(LAST BOOK PROCESSED)                  
SOBK0220 EQU   *                                                                
*                                                                               
*   NEXT STEP IS TO TURN OFF ALL DAYS IN EACH SLOT THAT ARE                     
*        NOT REPRESENTED IN THE INVENTORY HEADER.  IF THE                       
*        HEADER IS FOR A PROGRAM THAT RUNS MONDAY ONLY,                         
*        ALL OTHER DAY BITS WILL BE CLEARED.                                    
*                                                                               
         LR    RF,R3               SET A(FIRST DAY/TIME DATA)                   
SOBK0240 EQU   *                                                                
         OC    0(2,RF),0(RF)       SLOT EMPTY?                                  
         BZ    SOBK0300            YES - FINISHED                               
         NC    2(1,RF),WORK+75     EXCLUSIVE AND INVENTORY DAYS                 
*                                                                               
SOBK0260 EQU   *                                                                
         LA    RF,7(RF)            BUMP TO  NEXT SLOT                           
         B     SOBK0240            GO BACK FOR NEXT                             
SOBK0300 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  OVNTSLOT:   EACH DATE COMING IN FROM ARRAY WILL BE CONVERTED                 
*        TO A YR/WK# WITHIN YEAR, AND A DAY OF WEEK.  EACH UNIQUE               
*        YR/WK# WILL RECEIVE A NEW SLOT IN THE TABLE, AND THE                   
*        DAYS OF WEEK FOR ALL ENTRIES IN THAT WEEK WILL BE OR'D                 
*        INTO THE DAY BYTE.  IN THIS MANNER, UP TO SEVEN                        
*        INDIVIDUAL DAYS WILL BE COMPRESSED INTO A SINGLE TABLE                 
*        SLOT.                                                                  
*        R3  ->  FIRST SLOT OF BKDT EXTENDED AREA                               
*                                                                               
OVNTSLOT NTR1                                                                   
         MVC   DUB(1),DMCB+4       LOAD YEAR NUMBER RETURNED                    
         MVC   DUB+1(1),DMCB       LOAD WEEK NUMBER RETURNED                    
OVNT0020 EQU   *                                                                
         OC    0(2,R3),0(R3)       SLOT EMPTY?                                  
         BZ    OVNT0080            YES - USE IT                                 
         CLC   DUB(2),0(R3)        RETURNED Y/WK# = Y/WK# IN SLOT?              
         BE    OVNT0100            YES - ADD TO SLOT                            
         LA    R3,7(R3)            NO  - BUMP TO NEXT SLOT                      
         B     OVNT0020            GO BACK AND CHECK IT                         
OVNT0080 EQU   *                                                                
         MVC   0(2,R3),DUB         INSERT Y/WK# INTO SLOT                       
OVNT0100 EQU   *                                                                
         ST    R3,DUB+4            SAVE A(SLOT FOUND)                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
***SETCOMFX                                                                     
*    SECTION OF CODE LIFTED TO REGAIN SOME ADDRESSABILITY                       
*                                                                               
SETCOMFX NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R4,RFTACOM                                                       
         USING COMFACSD,R4         ESTABLISH COMFACS                            
*                                                                               
*        SAVE ROUTINE ADDRESSES                                                 
*                                                                               
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VDEMOVAL,CDEMOVAL                                                
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMAINT,CDEMAINT                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VDEMOMTH,CDEMOMTH                                                
*                                                                               
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETDAY,CGETDAY                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* 02/09/11 - The below x'90' move is a storage protection violation.  *         
*  I have no idea why it is there.  The first byte of any module is   *         
*  always a STM (X'90') so the below seems to serve no purpose.       *         
*  Commented out by SMYE on above date.                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*SMY*    L     RF,VDEMOUT                                                       
*SMY*    MVI   0(RF),X'90'                                                      
*                                                                               
*        RELOCATE V-CONS                                                        
*                                                                               
         XC    DMCB(12),DMCB       LOAD EXTRA VCONS                             
         MVC   DMCB+4(4),=X'D9000A00'  SET FOR CORE RESIDENT PHASES             
*                                                                               
         MVI   DMCB+7,QCHOPPER                                                  
         GOTO1 VCALLOV,DMCB        GET V(CHOPPER)                               
         MVC   VCHOPPER,DMCB                                                    
*                                                                               
         MVI   DMCB+7,QDEMOCON                                                  
         GOTO1 VCALLOV,DMCB        GET V(DEMOCON)                               
         MVC   VDEMOCON,DMCB                                                    
*                                                                               
         MVI   DMCB+7,QREDEMUP                                                  
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VDEMUP,DMCB                                                      
*                                                                               
         MVI   DMCB+7,QREGTIUN                                                  
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VGETIUN,DMCB                                                     
*                                                                               
         MVI   DMCB+7,QDEFINE                                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VDEFINE,DMCB                                                     
*                                                                               
         L     RF,RFTAWRK          GET A(WORKAREA)                              
         ST    RF,ABUFFSTR                                                      
         ST    RF,ABUFF            SET A(FIRST AVAILABLE TEXT LINE)             
*                                                                               
         LA    RF,20*132(RF)       FIND START OF TXTWRK                         
         ST    RF,ATXTWRK                                                       
*                                                                               
         DROP  R4                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***SETCOMFX                                                                     
***CHKOVRNT                                                                     
********************************************************************            
* CHKOVRNT:  IF OVERNIGHTS ARE REQUESTED, REPLACE DBLOCK           *            
*            ENTRIES FROM CALCULATED INFORMATION                   *            
*            OVRNTDTE REPLACES DBSELBK                             *            
*            OVRNTDAY REPLACES DBSELDAY                            *            
*            DBSELMED IS SET TO 'O' FOR OVERNIGHTS                 *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
CHKOVRNT NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET USING ADDR                               
         USING DBLOCKD,R8                                                       
*                                                                               
         OC    OVRNTDTE,OVRNTDTE   VALUES PRESENT IN OVERRIDES?                 
         BZ    CHKO0900            NO  - EXIT                                   
         MVC   DBSELBK,OVRNTDTE    YES - REPLACE DBLOCK ENTRIES                 
         MVC   DBSELDAY,OVRNTDAY                                                
         MVC   DBBTYPE,RFTOVNBK                                                 
*                                                                               
*   TEST                                                                        
***>>>   MVI   DBSELDAY,1          FORCE DAY TO MONDAY                          
*   TEST                                                                        
*                                                                               
         MVI   DBSELMED,C'O'                                                    
CHKO0900 EQU   *                                                                
*&&DO                                                                           
*   TEST DUMP:                                                                  
         LA    RF,DBSELBK                                                       
         LA    RE,DBSELDAY                                                      
         LA    R1,DBSELMED                                                      
         DC    H'0'                                                             
*   TEST DUMP END                                                               
*&&                                                                             
         XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***CHKOVRNT                                                                     
***ORIDEDEM                                                                     
********************************************************************            
* ORIDEDEM:  OVERRIDE DEMO LIST WITH 'HOMES'                       *            
*              --------------------------------                    *            
*                                                                  *            
*                                                                  *            
********************************************************************            
ORIDEDEM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,RFTCDEMS                POINT TO LIST OF DEMOS                
         LA    R0,RFTCDEMS+(RFTCNDQ*3)    END OF LIST                           
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   ORID0020                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R0,RFTCDMNM,2       GET THE COUNT                                
         MHI   R0,3                LENGTH OF DEMOS                              
         AR    R0,R3               END OF DEMO LIST                             
*                                                                               
ORID0020 DS    0H                                                               
*                                                                               
         OC    0(3,R3),0(R3)      CHECK FOR END OF DEMOS                        
         BZ    ORID0800            END OF DEMOS                                 
*                                                                               
         MVC   0(3,R3),=X'00D901'  OVERRIDE DEMO WITH RATINGS/HOMES             
         LA    R3,3(R3)            BUMP TO NEXT SLOT                            
         B     ORID0020            GO BACK FOR NEXT                             
ORID0800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***ORIDEDEM                                                                     
***OVRNTSET                                                                     
********************************************************************            
* OVRNTSET:  CALCULATE BOOK FROM DATE ENTERED WITH FETCH CALL      *            
*              --------------------------------                    *            
*                                                                  *            
*                                                                  *            
********************************************************************            
OVRNTSET NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    OVRNTDTE(3),OVRNTDTE                                             
*                                  CLEAR OVERNIGHT DATE / DAY                   
         CLI   RFTAMODE,C'D'       FETCH VIA DAY/TIME?                          
         BNE   OVRN0900            NO  - EXIT                                   
         OC    RFTCDTES,RFTCDTES   YES - ANY START DATE?                        
         BZ    OVRN0900            NO  - NO DATE                                
*                                                                               
*   TEST OVERRIDE THE DATE COMING IN WITH 12/7/04  !!                           
*                                                                               
***>>>   MVC   RFTCDTES(2),=X'D187'                                             
*                                                                               
*   TEST OVERRIDE END                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RFTCDTES),(0,WORK)                               
*                                  SET DATE AS EBCDIC                           
         MVC   DMCB+4(4),VGETDAY   SET A(GETDAY)                                
         MVI   DMCB+4,1            SET START DAY OF WEEK                        
         GOTO1 VNSIWEEK,DMCB,WORK,,VADDAY,VDATCON                               
*                                                                               
         MVC   OVRNTDTE(1),DMCB+4   LOAD YEAR NUMBER RETURNED                   
         MVC   OVRNTDTE+1(1),DMCB   LOAD WEEK NUMBER RETURNED                   
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
         MVC   OVRNTDAY,DMCB       SET DAY OF WEEK                              
*                                                                               
OVRN0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***OVRNTSET                                                                     
***CABSTAT                                                                      
********************************************************************            
*                                                                  *            
*              LOCAL CABLE OUTLET DETERMINATION                    *            
*              --------------------------------                    *            
*                                                                  *            
********************************************************************            
CABSTAT  NTR1  BASE=*,LABEL=*                                                   
         XC    SVALISTA,SVALISTA   CLEAR SAVE AREAS                             
         XC    SVALIMKT,SVALIMKT                                                
         CLI   RFTCSTAT,C'0'       NUMERIC MEDIA (SAT STATION?)                 
         BNL   CSTA0800            YES                                          
*                                                                               
         L     R3,AALIAS           SET A(1ST ENTRY IN ALIAS TABLE)              
CSTA0020 EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BNE   CSTA0040            NO  - TEST FOR STATION                       
         L     R3,AALIAS           SET A(1ST ENTRY IN ALIAS TABLE)              
         XC    DLOCABST(LOCABLEN,R3),DLOCABST(R3)                               
*                                  CLEAR 1ST TABLE ENTRY FOR REUSE              
         B     CSTA0080            YES - STATION NOT IN TABLE                   
*                                     REPLACE FIRST ENTRY                       
CSTA0040 EQU   *                                                                
         OC    DLOCABST(5,R3),DLOCABST(R3)                                      
*                                  ANY ENTRY IN TABLE FIELD?                    
         BZ    CSTA0080            NO  - ADD THIS ENTRY TO TABLE                
         CLC   RFTCSTAT,DLOCABST(R3)                                            
*                                  YES - STATION MATCH?                         
         BE    CSTA0100            YES - CHECK FOR LOCAL CABLE                  
         LA    R3,LOCABLEN(R3)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     CSTA0020            GO BACK FOR NEXT ENTRY                       
CSTA0080 EQU   *                                                                
         MVC   DLOCABST(5,R3),RFTCSTAT                                          
*                                  INSERT STATION INTO TABLE                    
         XC    DLOCABAL(12,R3),DLOCABAL(R3)                                     
*                                  CLEAR ALIAS TO BIN ZERO                      
         BAS   RE,STATGET          GET THE STATION RECORD                       
*                                     ALIAS IS INSERTED, IF FOUND               
         B     CSTA0100            RETURN                                       
CSTA0100 EQU   *                                                                
*                                                                               
*   NOW CHECK TABLE ENTRY FOR ALIAS.                                            
*        IF PRESENT, RESET STATION, ALFA, AND BOOKTYPE IN DBLOCK                
*                                                                               
         OC    DLOCABAL(12,R3),DLOCABAL(R3)                                     
*                                  ANY ALIAS?                                   
         BZ    CSTA0800            NO  - FINISHED                               
         BAS   RE,STARESET         YES - RESET STATION CALL LETTERS             
         BAS   RE,ALFRESET         RESET ALPHA MARKET                           
*                                                                               
*                                                                               
         B     CSTA0800                                                         
CSTA0800 EQU   *                                                                
         XIT1                                                                   
LOCABLEN EQU   17                  L(ALIAS TABLE ENTRY LENGTH)                  
DLOCABST EQU   0                   DISPLACEMENT TO STATION IN ENTRY             
DLOCABAL EQU   5                   DISPLACEMENT TO ALIAS   IN ENTRY             
***CABSTAT                                                                      
         EJECT                                                                  
***STARESET                                                                     
*   STRIP STATION FROM ALIAS, INSERT INTO DBSELSTA                              
STARESET NTR1                                                                   
         LA    R4,DLOCABAL(R3)     A(STATION OF ALIAS IN TABLE)                 
         LA    R0,5                SET LOOP CONTROL MAX                         
*                                                                               
         MVC   SVALISTA,SPACES     CLEAR STATION TO SPACES                      
         LA    R2,SVALISTA                                                      
*                                                                               
SSET0020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    SSET0040            YES - DON'T MOVE ANY MORE CHARACTERS         
         MVC   0(1,R2),0(R4)       NO  - MOVE CHAR TO DBLOCK                    
         LA    R2,1(R2)            BUMP TO NEXT OUTPUT POS                      
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,SSET0020         MOVE 5 CHARS MAX                             
SSET0040 EQU   *                                                                
         CLI   SVALISTA+4,C' '     ANY LAST CHARACTER? (MEDIA)                  
         BH    SSET0060            YES                                          
         MVI   SVALISTA+4,C'T'     NO  - SET TO T                               
SSET0060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***STARESET                                                                     
         EJECT                                                                  
***ALFRESET                                                                     
*   STRIP MARKET(?) FROM ALIAS, INSERT INTO DBSELALF                            
ALFRESET NTR1                                                                   
         LA    R4,DLOCABST(R3)     A(STATION OF ALIAS IN TABLE)                 
         LA    R0,17               SET LOOP CONTROL MAX                         
*                                                                               
         MVC   SVALIMKT,SPACES     CLEAR ALPHA TO SPACES                        
         LA    R2,SVALIMKT                                                      
*                                                                               
ASET0020 EQU   *                                                                
         CLI   0(R4),C'/'          SEPARATOR FOUND?                             
         BE    ASET0040            YES - NOW MOVE TRAILING CHARACTERS           
         LA    R4,1(R4)            BUMP TO NEXT INPUT  POS                      
         BCT   R0,ASET0020         MOVE 5 CHARS MAX                             
         B     ASET0060            NO SEPARATOR FOUND                           
ASET0040 EQU   *                                                                
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         MVC   0(3,R2),0(R4)       INSERT MARKET(?)                             
         OC    0(3,R2),SPACES                                                   
*                                  THERE MAY BE NOTHING THERE, BUT              
*                                     THERE WILL ALWAYS BE ROOM FOR             
*                                     AT LEAST THREE CHARACTERS.                
ASET0060 EQU   *                                                                
         XIT1                                                                   
                                                                                
***ALFRESET                                                                     
***STATGET                                                                      
*                                                                               
*   LOOK FOR ALIAS ENTRY IN STATION RECORD.                                     
*   R3  -->  TABLE ENTRY FOR ALIAS                                              
*                                                                               
STATGET  NTR1                                                                   
         XC    KEY,KEY             ESTABLISH AS REP RECORD KEY                  
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      SET RECORD ID                                
         MVC   RSTAKREP,RFTCREP    SET REP ID                                   
         MVC   RSTAKSTA,RFTCSTAT   SET STATION CALL LETTERS                     
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   SGET0010                                                         
         MVI   RSTAKSTA+4,C' '                                                  
SGET0010 EQU   *                                                                
*                                                                               
         BAS   RE,READ2            READ STA RECORD                              
         BNZ   SGET0800            NOT FOUND: EXIT                              
*                                                                               
         MVC   AIO,RFTAIO1         USE A(IOA1)                                  
*                                                                               
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         MVI   ELCODE,X'15'        SET TO FIND ALIAS ELEMENT                    
         L     R6,RFTAIO1                                                       
         BAS   RE,GETEL                                                         
         BNE   SGET0800            NO ENTRY: EXIT ROUTINE                       
*                                                                               
         USING RSTALIEL,R6         ESTABLISH STATION ALIAS ELEMENT              
*                                                                               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            SUBTRACT FOR MOVE BY LENGTH                  
         EX    RF,SGET0020                                                      
         B     SGET0040                                                         
SGET0020 EQU   *                                                                
         MVC   DLOCABAL(0,R3),RSTALIAS                                          
*                                  INSERT ALIAS BY LENTH INTO TABLE             
SGET0040 EQU   *                                                                
         OC    DLOCABAL(12,R3),DLOCABAL(R3)                                     
*                                  OR IN SPACES                                 
         B     SGET0800            EXIT ROUTINE                                 
*                                                                               
SGET0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R4,R6                                                            
*                                                                               
***STATGET                                                                      
***VSTAT                                                                        
********************************************************************            
*                                                                  *            
*              STATION MARKET NUMBER RETRIEVAL VIA DEMAND          *            
*              ------------------------------------------          *            
*                                                                  *            
********************************************************************            
VSTAT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
*   TEST TO DETERMINE STATION'S MARKET NUMBER                                   
*   TEST                                                                        
*        L     R0,RFTAIO1                                                       
*        LA    RE,1                                                             
*        LA    RF,2                                                             
*        DC    H'0'                                                             
*                                  RETRIEVE STATION MARKET NUMBER               
         LA    R5,DBLOCKA1                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK       USE DEMAND TO RETRIEVE MARKET #              
         MVC   DBCOMFCS,RFTACOM                                                 
*                                                                               
*   TEST                                                                        
****>>>  DC H'0'                                                                
*   TEST                                                                        
*                                                                               
         L     RE,RFTAIO2          CLEAR OUT IO AREA                            
         ST    RE,DBAREC                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   DBFUNCT,DBGETDEM    PHONY GETDEM CALL, BECAUSE                   
*                                     IT RETURNS MARKET NUMBER                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'       SET NSI AS SERVICE                           
         MVC   DBSELSTA,RFTCSTAT                                                
         MVI   DBSELSTA+4,C'T'                                                  
         MVI   DBSELSRC,C'N'       SET SORT TO 'NSI'                            
         MVC   DBSELBK,=X'6805'    INSERT A BOOK DATE                           
         MVI   DBSELDAY,X'40'      SET DAY TO MONDAY                            
         MVC   DBSELAGY,RFTCREP    INSERT REP CODE                              
         MVC   DBSELTIM,=X'06B306B3'                                            
*                                  INSERT TIME                                  
*                                                                               
VSTT0100 GOTO1 VDEMAND,DMCB,DBLOCK,0                                            
*                                                                               
*                                                                               
         LA    RE,DBACTRMK                                                      
         MVC   RFTCSMKT,DBACTRMK   SAVE THE MARKET NUMBER                       
*                                                                               
VSTT0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
***VSTAT                                                                        
***PEEPMTRS                                                                     
*                                                                               
*   PEEPMTRS:                                                                   
*       DROP INAPPROPRIATE PEOPLE METER INDICATORS IF DATES                     
*        DID NOT ACTUALLY USE PEOPLE METER DATA.                                
*   R3 --> RETURNED UPGRADE PRINTOUT                                            
*   R6 --> UPGRADE ELEMENT X'05'                                                
*                                                                               
*                                                                               
PEEPMTRS NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING RAVLNCOD,R6                                                      
*                                                                               
         MVC   HALF,RFTCSMKT       RETRIEVE STATION MARKET #                    
         MVC   RFTCUSTR,=C'MMMYY(X)'                                            
*                                                                               
*   CHECK UPGRADE BOOK TYPE:  IF NOT REGULAR OR HISP PM, EXIT                   
*                                                                               
         MVC   RFTCUSTR+6(1),RAVLNBT     CONSTRUCT COMPARISON STRING            
*                                  INSERT BOOK TYPE                             
*                                                                               
         CLI   RAVLNBT,C'I'        HISPANIC PEOPLE METER?                       
         BE    PEEP0100            YES - PROCESS HISP MARKETS                   
         CLI   RAVLNBT,C'P'        REGULAR  PEOPLE METER?                       
         BNE   PEEP0800            NO  - NO FURTHER CHECK NEEDED                
*                                                                               
*        REGULAR PEOPLE METER MARKETS                                           
*                                                                               
* GET START - END BOOKS FROM TABLE IN DEMTABS                                   
PEEP0100 ICM   RF,15,RFTACOM                                                    
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,LPMSTEND  GET A(LPM START-END BOOKS TABLE)             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING LPMSETD,RE                                                       
         L     R0,4(R1)            L'TABLE ENTRY                                
PEEP0130 CLC   =X'FFFF',0(RE)                                                   
         BE    PEEP0800            NOT USING PEOPLE METER, EXIT                 
         CLC   RAVLNBT,LPMSEBKT    COMPARE ON BOOK TYPE                         
         BNE   PEEP0150                                                         
         CLC   LPMSETMK,HALF       COMPARE ON MARKET NUMBER                     
         BNE   PEEP0150                                                         
         MVC   RFTCDSTR,LPMSTART   MOVE START AND END BOOK                      
         B     PEEP0200                                                         
**PEEP0150 LA    RE,LPMSETQ(RE)                                                 
PEEP0150 AR    RE,R0                                                            
         B     PEEP0130                                                         
         DROP  RE                                                               
*&&DO                                                                           
         MVC   RFTCDSTR,=X'68046808'                                            
         CLC   HALF,=H'101'        NY?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   RFTCDSTR,=X'68056807'                                            
         CLC   HALF,=H'403'        LA?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   RFTCDSTR,=X'68076809'                                            
         CLC   HALF,=H'202'        CHI?                                         
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   RFTCDSTR,=X'680A680C'                                            
         CLC   HALF,=H'407'        SF?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         B     PEEP0800            NOT USING PEOPLE METER - EXIT                
*                                                                               
*                                                                               
*        HISPANIC PEOPLE METER MARKETS                                          
*                                                                               
PEEP0100 EQU   *                                                                
         MVC   RFTCDSTR,=X'68046808'                                            
         CLC   HALF,=H'101'        NY?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   RFTCDSTR,=X'68076809'                                            
         CLC   HALF,=H'202'        CHI?                                         
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   RFTCDSTR,=X'680A680C'                                            
         CLC   HALF,=H'407'        SF?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         B     PEEP0800            NOT USING PEOPLE METER - EXIT                
*&&                                                                             
PEEP0200 EQU   *                                                                
         LA    R4,RAVLNOP1                                                      
PEEP0220 EQU   *                                                                
         OC    0(2,R4),0(R4)       ANYTHING IN OPERAND?                         
         BZ    PEEP0800            NO  - FINISHED SCAN                          
         CLC   0(2,R4),RFTCDSTR    OPERAND PRIOR TO PM START?                   
         BL    PEEP0240            YES - EDIT RETURNED FOOTNOTE                 
*                                  NO                                           
PEEP0230 EQU   *                                                                
         LA    R4,2(R4)            BUMP TO NEXT OPERAND                         
         B     PEEP0220            GO BACK FOR NEXT                             
PEEP0240 EQU   *                                                                
         MVC   WORK(2),0(R4)       LIFT YYMM FROM OPERAND                       
         MVI   WORK+2,1            SET DAY TO 01                                
         GOTO1 VDATCON,DMCB,(3,WORK),(9,WORK+3)                                 
         MVC   RFTCUSTR(3),WORK+3  ELIMINATE '/'                                
         MVC   RFTCUSTR+3(2),WORK+7                                             
         LR    RF,R3               SET START OF UPGRADE FOOTNOTE                
         LA    RE,132              CHECK EACH POSITION FOR STRING               
PEEP0260 EQU   *                                                                
         CLC   0(8,RF),RFTCUSTR    MMMYY(X) STRING FOUND IN FTNTE?              
         BE    PEEP0300            YES - RESET IT                               
PEEP0280 EQU   *                                                                
         LA    RF,1(RF)            NO  - BUMP TO NEXT POSITION                  
         BCT   RE,PEEP0260         CHECK NEXT EIGHT POSITIONS                   
         B     PEEP0230            GO BACK FOR NEXT OPERAND                     
PEEP0300 EQU   *                                                                
         MVC   5(3,RF),SPACES      CLEAR (X) FROM OPERAND                       
         B     PEEP0280            TAKE CARE OF REPEATS ON LINE                 
PEEP0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GTTXT'                   
***********************************************************************         
*                                                                     *         
*        PASS BACK TEXT DATA                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GTTXT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    RFTCNTL,RFTCTXTQ    SKIP UNLESS TEXT DATA WANTED                 
         BNO   GTTXTX                                                           
*                                                                               
         LA    R4,KEY              ESTABLISH INVENTORY RECORD KEY               
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   KEY,SVINVKEY        COPY CURRENT INVENTORY KEY                   
*                                                                               
         CLI   RFTCTXTT,RFTCTXIQ   IF NOT INVENTORY TEXT                        
         BE    GTTNINV                                                          
*                                     CREATE TEXT KEY                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,RINVKTYQ      INVENTORY RECORD ID                       
         MVC   RINVKREP,RFTPAREP      USE PARENT REP                            
         MVC   RINVKSTA,RFTCSTAT      STATION                                   
*                                                                               
         CLI   RINVKSTA+4,C' '     IF NO MEDIA PROVIDED                         
         BH    *+8                                                              
         MVI   RINVKSTA+4,C'T'        DEFAULT TO TV                             
*                                                                               
GTTNINV  DS    0H                                                               
*                                                                               
         MVC   RINVKRTP(1),RFTCTXTT      SET TEXT TYPE                          
*                                                                               
         MVC   RINVKTXT,RFTCTXT#   INIT TEXT NUMBER                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
GTTLOOP  DS    0H                                                               
*                                                                               
         CLC   RINVKEY(RINVKTXT-RINVKEY),KEYSAVE  DONE IF KEY BREAK             
         BNE   GTTDONE                                                          
*                                                                               
         OC    RFTCTXT#,RFTCTXT#   IF TEXT NUMBER GIVEN                         
         BZ    *+14                                                             
         CLC   RINVKTXT,RFTCTXT#      IT MUST MATCH FOUND KEY                   
         BNE   GTTDONE                                                          
*                                                                               
         MVC   AIO,RFTAIO2         SET I/O AREA                                 
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   OPTION,RINVKRTP     PASS TEXT TYPE                               
         GOTO1 =A(FMTTXT),RR=RELO  FORMAT TEXT                                  
*                                                                               
         MVC   RFTFTXT#,RINVKTXT   RETURN TEXT NUMBER                           
         MVC   RFTFTXTT,RINVKRTP   RETURN TEXT TYPE                             
*                                                                               
         MVI   RFTMODE,RFTNTXTQ    INDICATE NEW TXT DATA                        
*                                                                               
         OC    RFTFTXTN,RFTFTXTN   SKIP IF NOTHING TO PRINT                     
         BZ    *+8                                                              
         BAS   RE,HOOK                HOOK TO CALLER                            
*                                                                               
GTTCONT  DS    0H                                                               
*                                                                               
         XC    RFTFTDAT(RFTFTDAL),RFTFTDAT   CLEAR TEXT DATA AREAS              
*                                                                               
         CLI   RFTRETRN,RFTRNXTQ   DONE IF USER REQUESTS NEW CATEGORY           
         BE    *+8                                                              
         CLI   RFTRETRN,RFTRXITQ   DONE IF USER REQUESTS END                    
         BE    GTTDONE                                                          
*                                                                               
         OC    RFTCTXT#,RFTCTXT#   DONE IF SPECIFIC TEXT NUMBER WANTED          
         BNZ   GTTDONE                                                          
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     GTTLOOP                                                          
*                                                                               
GTTDONE  DS    0H                                                               
*                                                                               
GTTXTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***PEEPMTRS                                                                     
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GETDEMO'                 
***********************************************************************         
*                                                                     *         
*              ROUTINES FOR HANDLING THE DEMO FORMATTING              *         
*              -----------------------------------------              *         
*      ON ENTRY, R5 POINTS TO OUTPUT FOR DEMO VALUES                  *         
*                R2 POINTS TO DEMO BOOK DESCRIPTION                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETDEMO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,RFTCDEMS         DEMO LIST                                    
*                                                                               
         LA    R0,24               MAX 24 DEMOS ALLOWED                         
         LR    RF,R3               COPY START OF LIST                           
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETDEM20                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       GET LIST OF DEMOS                            
         LR    RF,R3               COPY START OF LIST                           
*                                                                               
         ZICM  R0,RFTCDMNM,2       GET THE COUNT                                
*                                                                               
GETDEM20 DS    0H                                                               
         CLI   0(RF),X'FF'         OKAY IF USER MARKED END OF LIST              
         BE    *+22                                                             
         OC    0(3,RF),0(RF)       MARK EOL                                     
         BZ    *+12                                                             
         LA    RF,3(RF)            NEXT IN LIST                                 
         BCT   R0,*-22                                                          
*                                                                               
         MVI   0(RF),X'FF'         SET END OF LIST MARKER                       
*                                                                               
         LA    R8,DBLOCKA1         ESTABLISH DEMO BLOCK                         
         USING DBLOCK,R8                                                        
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBFILE,=C'INV'      INDICATE DEMOS FROM INVENTORY REC            
         MVC   DBCOMFCS,RFTACOM    PASS COMFACS ADDRESS                         
*                                                                               
         L     R6,RFTAIO2          PASS INVENTORY RECORD ADDRESS                
         ST    R6,DBAREC                                                        
*                                                                               
         LA    R6,34(R6)           PASS A(1ST ELEMENT IN RECORD)                
         ST    R6,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
*                                                                               
         USING DBXTTID,R1                                                       
*                                                                               
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
*                                                                               
         MVC   DBXTSCTL,RFTPRCSN                                                
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',(R3)),DBLOCK,(R5)                             
*                                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RFTFBK,0(R2)        RETURN BOOK FOUND                            
*                                                                               
GETDEMOX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GTSHRLVL'                
***********************************************************************         
*                                                                     *         
*              ROUTINES FOR RETRIEVING SHARES AND LEVELS              *         
*              -----------------------------------------              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GTSHRLVL NTR1  BASE=*                                                           
*                                                                               
         MVC   SVDEMLST,RFTCDEMS   SAVE LIST OF DEMOS                           
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETSHR20                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R1,RFTCDMNM,2       GET THE COUNT                                
         MHI   R1,3                LENGTH OF DEMOS                              
*                                                                               
         ZICM  R0,RFTCDEMB,4       ADDRESS OF SAVE AREA                         
         LR    RF,R1               LENGTH OF MOVE                               
         LR    RE,R3               FROM ADDRESS                                 
         MVCL  R0,RE               COPY DEMOS                                   
*                                                                               
*        GET LEVELS                                                             
*                                                                               
GETSHR20 DS    0H                                                               
         MVI   LINSW,C'L'          *** LEVELS ***                               
*                                                                               
         BAS   RE,EFFDEMS          DEVELOP  DEMO TYPES                          
*                                                                               
         LA    R5,RFTFLVLS                                                      
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETSHR30                                                         
*                                                                               
         ZICM  R5,RFTCDMLV,4                                                    
*                                                                               
GETSHR30 DS    0H                                                               
         GOTO1 =A(GETDEMO),RR=RELO  FORMAT THE DEMOS                            
*                                                                               
         MVC   RFTCDEMS(L'SVDEMLST),SVDEMLST    RESTORE DEMLST                  
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETSHR40                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R1,RFTCDMNM,2       GET THE COUNT                                
         MHI   R1,3                LENGTH OF DEMOS                              
*                                                                               
         ZICM  R0,RFTCDEMB,4       ADDRESS OF SAVE AREA                         
         LR    RF,R1               LENGTH OF MOVE                               
         LR    RE,R3               TO   ADDRESS                                 
         MVCL  RE,R0               RESTORE DEMOS                                
*                                                                               
*        GET SHARES                                                             
*                                                                               
GETSHR40 DS    0H                                                               
         MVI   LINSW,C'S'          *** SHARES ***                               
*                                                                               
         BAS   RE,EFFDEMS                      DEVELOP DEMO TYPES               
*                                                                               
         LA    R5,RFTFSHRS                                                      
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETSHR50                                                         
*                                                                               
         ZICM  R5,RFTCDMSH,4                                                    
*                                                                               
GETSHR50 DS    0H                                                               
         GOTO1 =A(GETDEMO),RR=RELO   FORMAT THE SHARES                          
*                                                                               
         MVC   RFTCDEMS(L'SVDEMLST),SVDEMLST    RESTORE DEMLST                  
*                                                                               
         CLI   RFTCDMIN,X'FF'      CHECK FOR ADDRESS PARAMETER                  
         BNE   GETSHR60                                                         
*                                                                               
         ZICM  R3,RFTCDEMA,4       POINT TO DEMO LIST                           
         ZICM  R1,RFTCDMNM,2       GET THE COUNT                                
         MHI   R1,3                LENGTH OF DEMOS                              
*                                                                               
         ZICM  R0,RFTCDEMB,4       ADDRESS OF SAVE AREA                         
         LR    RF,R1               LENGTH OF MOVE                               
         LR    RE,R3               TO ADDRESS                                   
         MVCL  RE,R0               RESTORE DEMOS                                
*                                                                               
GETSHR60 DS    0H                                                               
*                                                                               
GTSHRLVX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - RECALC'                  
***********************************************************************         
*                                                                     *         
*  RECALCULATE SHARES FOR PA                                          *         
*                                                                     *         
*                    (RATING X 10000)                                 *         
*                    ---------------   = SHARE   (ROUND IN DAT50)     *         
*                         LEVEL                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECALC   NTR1                                                                   
         LA    R2,RFTCDEMS                                                      
         LA    R3,RFTFSHRS                                                      
         LA    RE,RFTFLVLS                                                      
         LA    R5,RFTFDEMS                                                      
*                                                                               
         CLI   RFTCDMIN,X'FF'                                                   
         BNE   RC30                                                             
*                                                                               
         ZICM  R2,RFTCDEMA,4                                                    
         ZICM  R3,RFTCDMSH,4                                                    
         ZICM  RE,RFTCDMLV,4                                                    
         ZICM  R5,RFTCDMRT,4                                                    
*                                                                               
RC30     OC    0(4,RE),0(RE)       SKIP CALC IF LEVEL IS 0                      
         BZ    RC40                                                             
         L     R1,0(R5)            RATING                                       
         MH    R1,=H'10000'                                                     
         SR    R0,R0                                                            
         D     R0,0(RE)                                                         
         ST    R1,0(R3)                                                         
         SPACE 1                                                                
RC40     LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         LA    RE,4(RE)                                                         
         LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)       EXTRA END CHECK                              
         BZ    *+16                                                             
         CLI   0(R2),X'FF'         MORE DEMOS                                   
         BNE   RC30                                                             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - GETUPG'                  
***********************************************************************         
*                                                                     *         
*        CALCULATE DEMOS FOR AN UPGRADE EXPRESSION                    *         
*              MODELLED ON OVERNIGHT TRANSFERS                        *         
*                                                                     *         
*NTRY    R2==> BASE BOOK DESCRIPTION                                  *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GETUPG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
*       CREATE DEMO DAY/TIME ELEMENTS FROM HEADER DAY/TIMES                     
*                                                                               
         LA    R5,CEELS            X'CE' ELMS AREA                              
         XC    CEELS,CEELS         INIT AREA                                    
*                                                                               
         L     R6,RFTAIO1                                                       
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   GTUCETDN            NONE FOUND - SKIP PRINTING                   
*                                                                               
GTUCETLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
*        BUILD A DEMO DAY/TIME ELEMENTS                                         
*                                                                               
         USING RINVZEL,R5          ESTABLISH ELEMENT                            
*                                                                               
         MVI   RINVZCOD,X'CE'      SET ELEMENT CODE TO 'CE'                     
         MVI   RINVZLEN,10         SET ELEMENT LENGTH TO 10                     
         MVC   RINVZDAY,RIDTDAY    SET DAYS                                     
         MVC   RINVZTIM,RIDTTIME   SET START AND END TIMES                      
         MVC   RINVZBK,RFTCBKVL-RFTCBKS(R2)    SET BOOK                         
         NI    RINVZBK,X'FF'-X'08' TURN OFF TIME PERIOD BIT                     
*                                                                               
GTUCETCN DS    0H                                                               
*                                                                               
         LA    R5,10(R5)           BUMP TO NEXT ELMENT BUILD AREA               
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         BE    GTUCETLP                                                         
*                                                                               
DIEUP    EQU   *                                                                
*                                                                               
GTUCETDN DS    0H                  END OF DAY-TIME ELEMENTS                     
*                                                                               
         GOTO1 =A(PURE),RR=RELO       LOOK FOR PURE DETAILS                     
*                                                                               
GETUPGX  DS    0H                                                               
*&&DO                                                                           
*   TEST                                                                        
         MVC   DIEUP(2),=X'0000'                                                
*&&                                                                             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER  PURE'                     
*******************************************************************             
*                                                                 *             
*              ROUTINES TO FIND PURE DETAILS                      *             
*                                                                 *             
*NTRY    R2 ==> SHARE BOOK DESCRIPTION                            *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
         DS    0D                                                               
PURE     NTR1  BASE=*,LABEL=*,WORK=(R7,PURWRKX-PURWRKD)                         
         USING PURWRKD,R7                                                       
*                                                                               
         LA    RE,DWRKAREA         CLEAR DOUBLE-WORD DEMO WORK AREA             
         LA    RF,DWRKAREL                                                      
         XCEF                                                                   
*                                                                               
         L     R4,RFTAWRK          SET POINTER TO INTERIM RECORD AREA           
*                                                                               
         LR    R3,R4                                                            
         A     R3,=F'5800'         CHECK PREVIOUS USE                           
         CLC   =C'ALIAS',0(R3)     INITIALIZED?                                 
         BE    PURE0020            YES                                          
         XCEF  (R4),6000           NO  - CLEAR FULL WORK AREA                   
         MVC   0(5,R3),=C'ALIAS'   SET INDICATOR                                
         B     PURE0040                                                         
PURE0020 EQU   *                                                                
         XCEF  (R4),5800           CLEAR WORK AREAS - LEAVE LAST 200            
PURE0040 EQU   *                                                                
*                                                                               
         ST    R4,AINTEREC                                                      
*                                                                               
         LA    R4,2000(R4)                                                      
         ST    R4,ADEMWRK                                                       
*                                                                               
         LA    R4,2000(R4)                                                      
         ST    R4,AIUNWORK                                                      
*                                                                               
         LA    R4,0500(R4)                                                      
         ST    R4,AEXPWORK         !!! THIS IS OVERLAID JUST BELOW              
*                                      WITH AIO2 ADDRESS   !!!                  
*                                                                               
*   NOTE:  ALIAS DATA BEGINS AT 5800 INTO THIS WORKAREA.                        
*        POSITIONS 1-5 OF THE AREA ARE AN INITIALIZER FLAG: "ALIAS"             
*        THERE ARE THEN A POSSIBLE 11 ENTRIES OF 17 CHARS EACH:                 
*        POSITIONS 1-5     =  STATION CALL LETTERS + MEDIA                      
*        POSITIONS 6-17    =  ALIAS OR EMPTY (BIN ZEROS)                        
*        AFTER 11 ENTRIES, A DELIMITER OF X'FFFFFFFF' IS INSERTED               
*        'AALIAS' IS SET TO A(FIRST 17-CHARACTER ENTRY).                        
*                                                                               
*           BILL UHR JAN10/05                                                   
*                                                                               
         LA    R4,1305(R4)         BUMP TO ALIAS AREA                           
         ST    R4,AALIAS           SET A(ALIAS STORAGE)                         
         LA    R4,187(R4)          SET ALIAS DELIMITER                          
         MVC   0(4,R4),=X'FFFFFFFF'                                             
*                                                                               
         L     R4,RFTAIO2          BUILD NEW DATA RECORD                        
         ST    R4,ADATAREC         SET POINTER TO DATA RECORD                   
         ST    R4,AEXPWORK                                                      
*                                                                               
         XCEF  (R4),2000           CLEAR RECORD AREA                            
*                                                                               
         USING RINVREC,R4          ESTABLISH 1ST REC AS INVENTORY HDR           
*                                                                               
         L     RF,RFTAIO1          POINT TO INVENTORY HEADER RECORD             
         MVC   RINVKEY(RINVKSPR-RINVKEY),0(RF)   START WITH HEADER KEY          
*                                                                               
         MVI   TPCNT,0             SET COUNTERS                                 
         MVI   HITCNT,0            THIS IS NOW A Y/N                            
         MVI   PCNT,0                                                           
         MVI   VARSW,C'N'          SET MORE THAN 2 PROGRAM SWITCH               
*                                                                               
         XC    PROGEL,PROGEL                                                    
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMULATORS                     
*                                                                               
*        SET UP DBLOCK                                                          
*                                                                               
         LA    R8,DBLOCKA1                                                      
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
*                                                                               
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         MVC   DBXTSCTL,RFTPRCSN                                                
*                                                                               
         LA    RF,DBXTTIDX         SET NEXT EXTENSION ADDRESS                   
         STCM  RF,15,DBXTNEXT                                                   
         DROP  R1                                                               
*                                                                               
         TM    RFTCNTL,RFTCIMPQ    IF DEMOS TO BE BASED ON IMPS                 
         BO    *+8                                                              
         TM    RFTSRMPP+RMPIMPSB,RMPIMPSA                                       
         BNO   *+8                                                              
         MVI   DBTAPEP,C'Y'           TURN ON FLAG                              
*                                                                               
         MVC   DBSELAGY,RFTCREP                                                 
         MVC   DBAREC,ADEMWRK          A(DEMO WORK AREA)                        
         MVC   DBCOMFCS,RFTACOM                                                 
         MVC   DBSELSRC,RFTCSRC                                                 
*                                                                               
*   SEQUENCING OF PROCESSING HAS BEEN REARRANGED TO SIMPLIFY                    
*        CODING OF OVERNIGHT PROCESSING REQUIREMENTS                            
*                                                                               
         CLC   RFTCBKYR-RFTCBKS(2,R2),=X'5A0C'                                  
*                                  SHR BK = DEC/90 (OVNT UPGRADE)?              
         BNE   PURE0041            NO  - REGULAR OVERNIGHT BOOK                 
         LA    RF,DUPGOVNT(R2)     YES - POINT TO UPGRD BOOK LIST               
         MVC   DBBTYPE,4(RF)       INSERT BOOK TYPE (SURVEY)                    
         B     PURE004X                                                         
PURE0041 EQU   *                                                                
         MVC   DBBTYPE,RFTCBKSV-RFTCBKS(R2)   BOOK TYPE (SURVEY)                
PURE004X EQU   *                                                                
         MVC   DBSELSTA,RINVKSTA                                                
*                                                                               
*   HAS AN ALIAS FOR THIS STATION BEEN FOUND?                                   
*                                                                               
         OC    SVALISTA,SVALISTA                                                
         BZ    PURE0060            NO                                           
         MVC   DBSELSTA,SVALISTA   YES - REPLACE STATION WITH ALIAS             
         CLC   SVALIMKT,SPACES     ANY EXTENSION (CABLE OUTLET)?                
         BNH   PURE0060            NO  - TREAT AS NORMAL STATON                 
         MVC   DBSELALF,SVALIMKT   REPLACE / INSERT ALPHA MKT                   
***      CLI   DBBTYPE,C'W'        WIRED CABLE?                                 
***      BE    PURE0060            YES - LEAVE IT ALONE                         
***      MVI   DBBTYPE,C'C'        SET TO 'CABLE OUTLET'                        
PURE0060 EQU   *                                                                
*                                                                               
         XC    OVRNTDUB,OVRNTDUB   SET UP PASS-BACK COUNTER                     
*                                                                               
         MVC   WORK(1),RFTCBKFL-RFTCBKS(R2)                                     
         MVI   OVRNTTYP,C'T'       SET INDICATOR TO 'TIME PERIOD'               
*                                                                               
         CLI   WORK,C'O'                                                        
*                                  OVERNIGHT TIME PERIOD?                       
         BE    PURE0042            YES                                          
         CLI   WORK,C'o'                                                        
*                                  OVERNIGHT TIME PERIOD BLOCK?                 
         BE    PURE0042            YES                                          
         MVI   OVRNTTYP,C'P'       SET INDICATOR TO 'PAV'                       
         CLI   WORK,C'V'                                                        
*                                  OVERNIGHT PAV?                               
         BE    PURE0042            YES                                          
         CLI   WORK,C'v'                                                        
*                                  OVERNIGHT PAV BLOCK?                         
         BNE   PURE0058            NO                                           
*                                  SETUP OVERNIGHT EXTENDED BLOCK               
PURE0042 EQU   *                                                                
         MVI   OVUPGFLG,0          SET OVERNIGHT UPGRADE OFF                    
         CLC   RFTCBKYR-RFTCBKS(2,R2),=X'5A0C'                                  
*                                  SHARE BOOK = DEC/90 (UPGRADE)?               
         BNE   PURE0043            NO  - REGULAR OVERNIGHT BOOK                 
         MVI   OVUPGFLG,X'FA'      SET OVERNIGHT UPGRADE ON                     
         LA    R2,DUPGOVNT(R2)     YES - POINT TO UPGRD BOOK LIST               
PURE0043 EQU   *                                                                
         GOTO1 =A(SETOVBKS),DMCB,(R2),RR=RELO                                   
*&&DO                                                                           
*   TEST                                                                        
         L     RF,RFTAIO1          POINT TO INVENTORY I/O AREA                  
         USING RINVREC,RF                                                       
         CLC   RINVKINV,=C'4125'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         MVI   DBSELMED,C'O'                                                    
         B     PURE0059                                                         
PURE0058 EQU   *                                                                
         MVI   OVRNTTYP,0          CLEAR OUT INDICATOR                          
         MVC   DBSELBK,RFTCBKYR-RFTCBKS(R2)   SHARE BOOK                        
         MVI   DBSELMED,C'T'                                                    
*                                                                               
PURE0059 EQU   *                                                                
         MVI   DBBEST,C'B'                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
*                                                                               
         CLI   RFTAMODE,RFTADIRQ   FETCH BY DEMOS DIRECTLY                      
         BNE   PURE0080            NO - DON'T SUPPORT DATE FETCH                
*                                                                               
         OC    RFTCDTES,RFTCDTES   DATE SET?                                    
         BZ    PURE0080            NO                                           
*                                                                               
******************************************************************              
*    OCT20/05 - UNIVISION HAS RECONSIDERED TREATING TP DATA IN THE              
*        SAME MANNER AS T4 DATA.  PER L. HENDY, THE CODE IS                     
*        DEACTIVATED.  WHEN THEY CHANGE THEIR MIND AGAIN, IT CAN                
*        BE REACTIVATED AS APPROPRIATE.                                         
******************************************************************              
*                                                                               
*&&DO                                                                           
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKFT                                    
*                                  TYPICAL TIME PERIOD BOOK?                    
         BE    PURE0080            YES - DON'T SET WEEK USER FLAG               
*&&                                                                             
******************************************************************              
*                                                                               
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKF4                                    
*                                  TIME PERIOD (T4) BOOK?                       
         BE    PURE0080            YES - DON'T SET WEEK USER FLAG               
*                                                                               
         MVC   DBSEL1WK,RFTCDTES                                                
*                                                                               
PURE0080 DS    0H                                                               
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKFI   IF INVENTORY BOOK                
         BNE   PURE0100                                                         
*                                                                               
         MVC   DBSELAGY,RFTPAREP                                                
*                                                                               
         OC    RINVKINV,RINVKINV                                                
         BZ    PURE0500                                                         
*                                                                               
         MVC   DBFILE,=C'IUN'                   - CHANGE FILE NAME              
         MVI   DBSELMED,C'U'                    - CHANGE MEDIA                  
         MVI   DBSELDAY,0          DEMAND FUDGE                                 
         MVC   DBSELTIM,=AL2(0600,2959)                                         
*                                                                               
         MVC   DBSELINV,RINVKINV                                                
         GOTO1 VDATCON,DMCB,(3,RINVKSTD),(2,DBSELDAT)                           
*                                                                               
*        FIND KSRC                                                              
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,RFTCSRC                SET RATING SERVICE                
         MVC   GSIBITS,RFTCBKVL-RFTCBKS(R2)   SET BOOKVAL BITS                  
         MVC   GSIBKTYP,RFTCBKSV-RFTCBKS(R2)  SET BOOK TYPE                     
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT,RFTACOM,RR=RELO              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   PURE0500                                                         
*                                                                               
*        MVC   DBSTYPE,GSOKSRC     SET KSRC IN KEY                              
         MVI   DBSTYPE,X'FF'       SET RSR/QLF/BTP IN DBEXTEND                  
*                                                                               
         L     R1,DBEXTEND         GET NEXT EXTENSION ADDRESS                   
PURE0090 DS    0H                                                               
         L     R1,DBXTNEXT-DBXTTID(R1)                                          
         USING DBXINVWK,R1                      INVENTORY EXTENSION             
         CLC   DBXIWID,=C'RINV'    INVENToRY EXTENSION?                         
         BE    PURE0095            YES - REUSE IT                               
         OC    DBXINVWK(DBXIWNXT-DBXINVWK),DBXINVWK                             
*                                  SLOT ALREADY IN USE?                         
         BNZ   PURE0090            YES - CHECK NEXT SLOT                        
*                                  NO  - SLOT IS EMPTY                          
PURE0095 DS    0H                                                               
         XC    0(DBXINVWL,R1),0(R1)                                             
*                                  CLEAR AREA FOR DATA                          
         MVC   DBXIWID,=C'RINV'    INSERT DATA TYPE INDICATOR                   
         LR    RF,R1                                                            
         LA    RF,DBXINVWL(RF)     CALCULATE NEXT OPEN ADDRESS                  
         STCM  RF,15,DBXIWNXT      SAVE A(NEXT EXTEND AREA)                     
         MVC   DBXIKRSR,GSIRSVC                                                 
         MVC   DBXIKQLF,GSIBITS                                                 
         MVC   DBXIKBTP,GSIBKTYP                                                
         DROP  R1                                                               
*                                                                               
         B     PURE0180                                                         
*                                                                               
PURE0100 DS    0H                                                               
         MVC   DBFILE,=C'PAV'                  DEFAULT FILE                     
*                                                                               
         CLI   OVRNTTYP,C'T'       OVERNIGHT TP?                                
         BE    PURE0102            YES - TREAT AS REG TP HERE                   
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKFT                                    
*                                  IF TIME PERIOD BOOK                          
         BNE   *+14                                                             
PURE0102 DS    0H                                                               
         MVC   DBFILE,=C'TP '      - CHANGE FILE NAME                           
         MVI   DBTPTT,C'T'         - SET TP OPTION                              
*                                                                               
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKF4                                    
*                                  IF TIME PERIOD (4WK AV)                      
         BNE   PURE0120                                                         
         MVC   DBFILE,=C'TP '      - CHANGE FILE NAME                           
         MVI   DBTPTT,C'P'         - SET 4WK OPTION                             
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
PURE0120 DS    0H                                                               
         L     R1,DBEXTEND         GET NEXT EXTENSION ADDRESS                   
PURE0125 DS    0H                                                               
         L     R1,DBXTNEXT-DBXTTID(R1)                                          
         USING DBXTLD,R1                        DAY/TIME EXTENSION              
         CLC   =C'DYTM',DBXTLID    SLOT USED AS DYTM?                           
         BE    PURE0127            YES - USE IT AGAIN                           
         OC    DBXTLD(DBXTLNXT-DBXTLD),DBXTLD                                   
*                                  SLOT ALREADY IN USE?                         
         BNZ   PURE0125            YES - CHECK NEXT SLOT                        
*                                  NO  - SLOT IS EMPTY                          
PURE0127 EQU   *                                                                
         XC    DBXTLD(DBXTLIST-DBXTLD),DBXTLD   CLEAR HEADER                    
         MVC   DBXTLID,=C'DYTM'                 SET EXTENSION ID                
         LA    R3,DBXTLIST                      A(DAYS/TIMES LIST)              
         XC    DBXTLIST,DBXTLIST                CLEAR 1ST ENTRY                 
*                                                                               
         L     R6,RFTAIO1          A(INVENTORY HEADER)                          
         MVI   ELCODE,X'02'        FIND FIRST DAY/TIME ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   PURE0160            END OF ELMENTS                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
PURE0140 DS    0H                                                               
         MVC   0(1,R3),RIDTDAY     PASS DAY/TIME                                
         MVC   1(4,R3),RIDTTIME                                                 
         DROP  R6                                                               
*                                                                               
         LA    R3,L'DBXTLIST(R3)   NEXT PLACE IN LIST                           
         BAS   RE,NEXTEL                                                        
         BE    PURE0140                                                         
*                                 JRD - END OF LIST IS NICE                     
         XC    0(L'DBXTLIST,R3),0(R3)                                           
*                                                                               
*   ADD A BLOCK OF SPACE IN CASE DYTM GETS REUSED WITH MORE                     
*        DATA                                                                   
*                                                                               
         LA    R3,64(R3)          CALCULATE A(NEXT EXPANSION SLOT)              
         ST    R3,DBXTLNXT                                                      
         DROP  R1                                                               
*                                                                               
PURE0160 DS    0H                                                               
*                                                                               
*        CREATE SHARE DATA                                                      
*                                                                               
PURE0180 DS    0H                                                               
         ST    R8,MTHCFACS         INITIALIZE BLOCK FOR VDEMOMTH                
         XC    MTHFCTR,MTHFCTR                                                  
*                                                                               
         LA    R0,3                MAX 3 EXTRA BOOKS AVERAGED FOR SHARE         
         LA    R3,RFTCBKLQ(R2)     FIRST OF EXTRA BOOKS IN AVG                  
*                                                                               
PURE0200 DS    0H                                                               
*                                                                               
         MVI   INVNOUPT,0          THIS IS NOT AN INV RECD W/UPT                
         CLC   DBFILE,=C'IUN'      LOOKING UP INV RECD?                         
         BNE   *+18                                                             
         OC    ARAVLNEL,ARAVLNEL                                                
         BNZ   *+8                                                              
         MVI   INVNOUPT,1          THIS IS AN INV RECD W/OUT UPT                
*                                                                               
         GOTO1 =A(CHKOVRNT),DMCB,(R8),RR=RELO                                   
         MVI   OVERRFLG,C'N'       SET FLAG ERROR = NO                          
*                                                                               
         GOTO1 VDEMAND,DMCB,DBLOCK,DHOOK                                        
         OC    ARAVLNEL,ARAVLNEL   DONE IF NO UPGRADE ELEMENT                   
         BZ    PURE0220                                                         
*                                                                               
         CLI   OVUPGFLG,X'FA'      OVERNIGHT UPGRADE IN PROGRESS?               
         BE    PURE0220            YES - EXTEND BKS AVERAGED                    
*                                                                               
         MVC   DBSELBK,0(R3)       GET NEXT BOOK IN AVERAGE                     
         LA    R3,2(R3)            POINT TO NEXT BOOK                           
*                                                                               
         OC    DBSELBK,DBSELBK     DONE IF NO MORE BOOKS                        
         BZ    *+8                                                              
         BCT   R0,PURE0200         AVERAGE IN NEXT BOOK                         
*                                                                               
PURE0220 DS    0H                                                               
         CLI   HITCNT,0            ANY LUCK                                     
         BE    PURE0500                                                         
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BNE   PURE0500            NOT EOF, IT WAS A REAL ERROR                 
*                                                                               
         MVI   DBERROR,0           RESET ERROR FIELD                            
*                                                                               
         CLI   INVNOUPT,1                                                       
         BNE   *+12                                                             
         L     R4,DBAREC           RECD GOTTEN IN HOOK                          
         B     PURE0380            JUST RETURN THE INV RECD FOUND               
*                                                                               
*        UNWEIGHT THE INTERIM RECORD AND SHARES BEFORE 'FF' CALL                
*                                                                               
         MVC   MTHFCTR+2(2),DBDIVSOR                                            
         MVC   MTHIFIL,=C'INV'     FORCE BOOK ELEMENT LOOKUP                    
         MVC   MTHOFIL,=C'INV'     FORCE BOOK ELEMENT LOOKUP                    
*                                                                               
         L     RF,AINTEREC                                                      
         ST    RF,DBAREC                                                        
*                                                                               
         LA    RF,34(RF)                                                        
         ST    RF,DBAQUART                                                      
*                                                                               
         CLI   DBSELMED,C'O'       OVERNIGHTS?                                  
         BE    PURE222             YES                                          
         GOTO1 VDEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC               
         B     PURE224                                                          
*                                                                               
PURE222  DS    0X                                                               
* FOR OVERNIGHTS, TO PREVENT OVERFLOW USE DOUBLE-WORD ARITHMETIC                
* DIVIDE DOUBLE-WORD VALUES IN DWRKAREA BY WEIGHT                               
* AFTER THIS CALL,AINTEREC HAS A DEMO-STYLE RECORD WITH RESULT VALUES           
         GOTO1 VDEMOMTH,DMCB,=C'DDI',DWRKAREA,AINTEREC,MATHFAC                  
PURE224  DS    0X                                                               
*                                                                               
         BAS   RE,DIVSHR                                                        
*                                                                               
         L     R4,AINTEREC         UPGRADE INTERIM RECORD                       
*                                                                               
         OC    PROGEL,PROGEL       ANY PROGRAMS TO ADD                          
         BZ    PURE0240                                                         
*                                                                               
         LA    R5,PROGEL                                                        
         USING PROGELD,R5                                                       
*                                                                               
         MVI   PCODE,X'01'                                                      
         MVI   PELLEN,PNAME2-PROGELD    FIND ELEMENT LENGTH                     
*                                                                               
         CLC   PNAME1,PNAME2       IF MORE THAN ONE PROGRAM                     
         BE    *+18                                                             
         MVI   PNAME1+7,C'/'          SET NAME SEPARATOR                        
         MVC   PNAME1+8(7),PNAME2     SET SECOND NAME                           
         MVI   PNAME1+15,C' '         CLEAR LAST BYTE                           
*                                                                               
         MVI   PLIN,1                                                           
         CLI   OVRNTTYP,0          OVERNIGHT DATA REQUEST?                      
         BE    PURE0230            NO                                           
         MVC   WORK(3),0(R2)       OVERNIGHT DATA CONTAINS THREE                
*                                     BYTE BINARY DATE IN FIELD                 
         B     PURE0235            YES                                          
PURE0230 EQU   *                                                                
         MVC   WORK(2),RFTCBKYR-RFTCBKS(R2) DISPLAY BASE BOOK (MMMYY)           
         MVI   WORK+2,X'01'                                                     
PURE0235 EQU   *                                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(6,DUB)                                    
*                                                                               
         MVC   PMON,DUB                                                         
         MVC   PYR,DUB+4                                                        
*                                                                               
         MVC   PINVCODE,=C'PAV'    DEFAULT FILE                                 
*                                                                               
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BNE   *+10                                                             
         MVC   PINVCODE,=C'TP '         CHANGE DESIGNATION                      
*                                                                               
         CLI   DBTPTT,C'T'         TEST FOR TYPICAL TIME                        
         BNE   *+10                                                             
         MVC   PINVCODE,=C'TT '         CHANGE DESIGNATION                      
*                                                                               
         MVI   PEQS,C'='                                                        
         OC    PFBK(9),SPACES                                                   
         DROP  R5                                                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),PROGEL,(R6),RR=RELO                         
*                                                                               
PURE0240 DS    0H                                                               
*                                                                               
*        ADD UPGRADE ELEMENT                                                    
*                                                                               
         OC    ARAVLNEL,ARAVLNEL   SKIP IF NO UPGRADE ELEMENT                   
         BZ    PURE0260                                                         
*                                                                               
         LR    R6,R4                                                            
*                                                                               
*                                                                               
*   FOR AN OVERNIGHT UPGRADE, THE RECORD IN THE DELIVERY AREA                   
*        IS INCOMPLETE.  THIS DUMMY DATA IS INSERTED TO PERMIT                  
*        THE RECORD TO PROCESS CORRECTLY IN SUBSEQUENT DEMO                     
*        ROUTINE CALLS.                                                         
*                                                                               
         MVC   24(2,R6),=X'D75A'                                                
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         ICM   RF,15,ARAVLNEL                                                   
         LA    RF,11(RF)           POINT TO 05 ELEMENT                          
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),(RF),(R6),RR=RELO                           
*                                                                               
PURE0260 DS    0H                                                               
*    ADD IN A CE ELEMENT                                                        
*        FIRST DELETE ALL CURRENT ONES                                          
*                                                                               
         LR    R6,R4               POINT TO INVENTORY RECORD                    
         MVI   ELCODE,X'CE'        SET ELEMENT TYPE                             
         BAS   RE,GETEL            FIND FIRST ELEMENT OF TYPE                   
         BNE   PURE0300            NONE FOUND                                   
*                                                                               
PURE0280 DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),(R6),0,RR=RELO   DELETE ELEMENT             
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         BE    PURE0280                                                         
*                                                                               
PURE0300 DS    0H                                                               
*                                                                               
*        ADD DAY/TIME ELEMENTS FROM INVENTORY RECORD                            
*                                                                               
         LA    R5,CEELS            POINT TO DAY/TIME ELEMENTS                   
         LA    R0,8                MAX NUMBER OF ELEMENTS                       
         USING RINVZEL,R5          ESTABLISH DAY/TIME ELEMENT                   
PURE0320 DS    0H                                                               
         CLI   RINVZCOD,0          DONE IF END OF ELEMENTS FOUND                
         BE    PURE0340                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),RINVZEL,(R6),RR=RELO  ADD ELEMENT           
         LA    R5,10(R5)           BUMP TO NEXT ELEMENT                         
         LA    R6,10(R6)           BUMP TO NEXT INSERTION POINT                 
         BCT   R0,PURE0320                                                      
         DROP  R5                                                               
*                                                                               
PURE0340 DS    0H                                                               
*                                                                               
*        DO UPGRADE                                                             
*                                                                               
         OC    ARAVLNEL,ARAVLNEL   SKIP IF NO UPGRADE ELEMENT                   
         BZ    PURE0360                                                         
*                                                                               
         NI    11(R4),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
*                                                                               
         ICM   RF,15,ARAVLNEL                                                   
         LA    RF,11(RF)           POINT TO 05 ELEMENT                          
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         TM    RFTCNTL,RFTCIMPQ    IF DEMOS TO BE BASED ON IMPS                 
         BO    *+8                                                              
         TM    RFTSRMPP+RMPIMPSB,RMPIMPSA                                       
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         MVC   DEMODUB(4),=C'RDB=' PASS A(DBLOCK)                               
         STCM  R8,15,DEMODUB+4                                                  
*                                                                               
         LA    RE,34(R4)           BUILD DEMUP 1ST PARM                         
         ST    RE,DMCB                                                          
         CLC   DBFILE-DBLOCK(3,R8),=C'IUN'                                      
         BNE   *+8                                                              
         MVI   DMCB,C'I'                                                        
*                                                                               
         MVI   BYTE,0              CLEAR OVERNIGHT AVERAGE FLAG                 
         GOTO1 VDEMUP,DMCB,,((R0),(RF)),(OVUPGFLG,RFTACOM),DEMODUB,    X        
               HOMSHR                                                           
*                                                                               
         OI    11(R4),X'40'                                                     
*                                                                               
PURE0360 DS    0H                                                               
*                                                                               
*        REPLACE CD ELEMENT WITH DUMMY ELEMENT                                  
*                                                                               
         LA    R5,CDEL             BUILD AND ADD A CD ELEMENT                   
         XC    CDEL,CDEL                                                        
         USING RINVCEL,R5                                                       
*                                                                               
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
*                                                                               
         MVC   RINVCODE,SPACES                                                  
*                                                                               
         CLI   DBFILE,C'T'         TEST FOR TIME PERIOD                         
         BNE   *+10                                                             
         MVC   RINVCODE,=C'TP'                                                  
*                                                                               
         CLI   DBTPTT,C'T'         TEST FOR TYPICAL TIME                        
         BNE   *+10                                                             
         MVC   RINVCODE,=C'TT'                                                  
*                                                                               
         LR    R6,R4               POSITION ELEMENT INSERTION                   
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),CDEL,(R6),RR=RELO                           
*                                                                               
*        COPY RESULTS TO DEMO TRACK I/O AREA                                    
*                                                                               
PURE0380 L     RF,RFTAIO2                                                       
         MOVE  ((RF),2000),0(R4)                                                
         MVC   KEY,0(R4)                                                        
*                                                                               
PURE0500 DS    0H                                                               
         NI    RFTFUPEX,X'FF'-X'40'                                             
*                                  TURN OFF OVERFLOW ERROR FLAG                 
         CLI   OVERRFLG,C'Y'       REGISTER OVERFLOW ON EARLIER CALL?           
         BNE   PURE0520            NO                                           
         OI    RFTFUPEX,X'40'      TURN ON  OVERFLOW ERROR FLAG                 
PURE0520 DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER  DHOOK'                    
*******************************************************************             
*                                                                 *             
*              HOOK TO HANDLE DEMO RECORDS                        *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
DHOOK    NTR1                                                                   
         CLI   INVNOUPT-PURWRKD(R7),1       INV RECD W/OUT MANIPULTN            
         BNE   DHOK0020                     JUST RELEASE AS IS.                 
         AI    PCNT,1                                                           
         MVI   HITCNT,1                                                         
         B     DHOK0900                                                         
*                                                                               
DHOK0020 L     R4,AINTEREC                                                      
         USING RINVKEY,R4          ESTABLISH INVENTORY KEY                      
*                                                                               
         CLI   0(R4),0             TEST FOR INITIALIZED INTERIM REC             
         BNE   DHOK0080                                                         
*                                                                               
         L     RF,RFTAIO1          POINT TO INVENTORY HEADER RECORD             
         MVC   RINVKEY,0(RF)       USE HEADER KEY                               
*                                                                               
         CLC   DBFILE,=C'IUN'                 INVENTORY FILE?                   
         BNE   DHOK0030                       NO                                
*                                                                               
         L     R1,DBEXTEND         GET NEXT EXTENSION ADDRESS                   
DHOK0025 DS    0H                                                               
         L     R1,DBXTNEXT-DBXTTID(R1)                                          
         USING DBXINVWK,R1                      INVENTORY EXTENSION             
         CLC   DBXIWID,=C'RINV'    INVENTORY EXTENSION?                         
         BNE   DHOK0025                                                         
         MVC   RINVKRSR,DBXIKRSR   SET RATING SOURCE                            
         MVC   RINVKQLF,DBXIKQLF   SET QUALIFIER                                
         MVC   RINVKBTP,DBXIKBTP   SET BOOKTYPE                                 
         B     DHOK0060                                                         
         DROP  R1                                                               
*                                                                               
*        FIND KSRC                                                              
*                                                                               
DHOK0030 DS    0H                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,RFTCSRC                SET RATING SERVICE                
         MVC   GSIBKTYP,DBBTYPE               SET BOOK TYPE                     
         MVI   GSIQLF,C' '         DEFAULT TO NORMAL BOOK                       
*                                                                               
*   check this out:  will be a problem here coming in from an                   
*        overnight book or block                                                
*                                                                               
         CLI   OVRNTTYP,C'T'       OVERNIGHT TIME PERIOD?                       
         BE    DHOK0040            YES -                                        
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKFT                                    
*                                  TIME PERIOD FILE?                            
         BE    DHOK0040                                                         
         CLI   RFTCBKFL-RFTCBKS(R2),RFTCBKF4                                    
*                                  TIME PERIOD (4WK AV)?                        
         BNE   DHOK0040                                                         
DHOK0040 EQU   *                                                                
         MVI   GSIQLF,C'T'         SWITCH TO TIME PERIOD                        
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'Q',GSRCIN),GSRCOUT,RFTACOM,RR=RELO              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   DHOK0060                                                         
*        MVC   RINVKSRC,GSOKSRC    SET KSRC IN KEY                              
*SMY*    MVC   RINVKRSR,GSIRSVC                                                 
         MVC   RINVKRSR,GSORSVC                                                 
         MVC   RINVKQLF,GSOBITS                                                 
*SMY*    MVC   RINVKBTP,GSIBKTYP                                                
         MVC   RINVKBTP,GSOBKTYP                                                
*                                                                               
DHOK0060 DS    0H                                                               
         MVC   RINVKBK,RFTCBKYR-RFTCBKS(R2)  ADD BOOK TO KEY                    
*                                                                               
DHOK0080 MVC   MTHFCTR+2(2),DBFACTOR                                            
         AI    PCNT,1                                                           
         MVI   HITCNT,1                                                         
*                                                                               
         MVC   BLOCK(16),SPACES                                                 
*                                                                               
         CLC   DBFILE,=C'IUN'                 INVENTORY FILE?                   
         BNE   DHOK0100                       NO                                
*                                                                               
         GOTO1 VDEFINE,DMCB,=C'TRAK',DBLOCK,BLOCK                               
         B     DHOK0120                                                         
*                                                                               
DHOK0100 DS    0H                                                               
         GOTO1 VDEFINE,DMCB,=C'PROGRAM',DBLOCK,BLOCK                            
*                                                                               
DHOK0120 DS    0H                                                               
*                                                                               
*              MAD BY QUARTER HOUR WEIGHT INTO INTERIM RECORD                   
*                                                                               
         L     R4,AIUNWORK         INIT WORK AREAS                              
         XCEF  (R4),500                                                         
*                                                                               
         L     R4,AEXPWORK                                                      
         XCEF  (R4),1516                                                        
*                                                                               
         GOTO1 VGETIUN,DMCB,(10,DBLOCK),AEXPWORK                                
*                                                                               
         MVC   DBNUMVLS,=H'320'                                                 
*                                                                               
         L     R4,AEXPWORK                                                      
         USING IUNREC,R4                                                        
*                                                                               
         CLC   DBFILE,=C'IUN'        INVENTORY FILE?                            
         BE    DHOK0140              YES - LEAVE THE RECORD ALONE               
*                                                                               
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
*                                                                               
DHOK0140 DS    0H                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                      
*                                                                               
         L     R3,DBAQUART         SAVE CURRENT VALUES                          
         L     R4,DBAREC                                                        
         L     RE,AIUNWORK                                                      
         ST    RE,DBAREC                                                        
*                                                                               
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
*                                                                               
         LA    RF,OTFORMAT         DEFAULT FORMAT FIELDS                        
         MVC   OTFORMAT,OFORMAT                                                 
*                                                                               
         CLI   DBTAPEP,C'Y'        IF DEMOS BASED ON IMPRESSIONS                
         BNE   *+10                                                             
         MVC   OTFORMAT,OFORMATI   SWITCH FORMAT FIELDS                         
*                                                                               
         GOTO1 VDEMAINT,DMCB,=C'PUT',DBLOCK,AEXPWORK,(RF)                       
         DROP  R4                                                               
DHOK0160 EQU   *                                                                
*                                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
         MVC   MTHIFIL,=C'PAV'      KEEP FILE FORMAT OF INPUT RECORD            
         MVC   MTHOFIL,=C'INV'                                                  
*                                                                               
*   TEST                                                                        
         CLI   OVERRFLG,C'Y'       REGISTER OVERFLOW ON EARLIER CALL?           
         BE    DHOK0170            YES - NO MORE DEMOMTH DONE                   
*   TEST END                                                                    
*                                                                               
         CLI   DBSELMED,C'O'       OVERNIGHTS?                                  
         BE    DHOK0165            YES                                          
         GOTO1 VDEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
         B     DHOK0170                                                         
                                                                                
DHOK0165 DS    0X                                                               
* FOR OVERNIGHTS, TO PREVENT OVERFLOWS USE DOUBLE-WORD ARITHMETIC               
* MULTIPLY DEMOS BY DBFACTOR AND ADD TO DOUBLE-WORD ACCUMS AT DWRKAREA          
         GOTO1 VDEMOMTH,DMCB,=C'DMA',DBAREC,DWRKAREA,MATHFAC                    
*                                                                               
DHOK0170 EQU   *                                                                
*                                                                               
*   FOR OVERNIGHTS, A CONDITION WAS DISCOVERED WHERE THE VOLUME OF              
*        DATA CAUSED A REGISTER OVERFLOW DURING THE CALCULATIONS,               
*        THEREBY INVALIDATING THE CONTENTS.  UNTIL THE ROUTINES                 
*        CAN BE REWRITTEN AS PACKED ARITHMETIC, AN ERROR CODE                   
*        WILL BE RETURNED TO INDICATE THAT THE CONDITION WAS                    
*        ENCOUNTERED, AND THE DATA WILL BE NULLED.  SUBSEQUENT                  
*        PROCESSING OF THIS DATA SHOULD BE CURTAILED.                           
*                                                                               
         TM    DBERROR,X'40'       REG OVERFLOW?                                
         BNO   DHOK0180            NO                                           
         MVI   OVERRFLG,C'Y'       YES - SET FLAG ERROR = YES                   
*                                                                               
*                                                                               
DHOK0180 DS    0H                                                               
         ST    R4,DBAREC           RESTORE DBLOCK STUFF                         
         ST    R3,DBAQUART                                                      
*                                                                               
*****    CLC   BLOCK(4),=C'AVG.'   FILTER OUT THIS NAME                         
*****    BE    DHOK0900                                                         
*                                                                               
         LA    R2,PROGEL                                                        
         USING PROGELD,R2                                                       
*                                                                               
         OC    BLOCK(16),SPACES                                                 
         CLC   BLOCK(16),SPACES                                                 
         BNH   DHOK0900                                                         
*                                                                               
         MVC   PNAME2,BLOCK        CAPTURE LAST PROGRAM NAME                    
*                                                                               
         CLC   PNAME1,SPACES       CAPTURE FIRST PROGRAM NAME                   
         BH    *+10                                                             
         MVC   PNAME1,BLOCK                                                     
*                                                                               
DHOK0900 DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER  GETSHR'                   
*******************************************************************             
*                                                                 *             
*        ROUTINE TO GET SHARES AND UPDATE ACCUMULATORS            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
GETSHR   NTR1                                                                   
*                                                                               
         MVC   DUB(2),DBACTBK                                                   
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHR                        
*                                                                               
         MVC   DBACTBK,DUB         RESTORE BOOK VALUE                           
*                                                                               
         LA    R1,HOMSHR           POINT TO OUTPUT AREA                         
         LA    RE,TOTSHR           POINT TO ACCUMS                              
         LA    R0,3                                                             
*                                                                               
GETSHRLP L     RF,0(R1)                                                         
         MH    RF,MTHFCTR+2        MULTIPLY SHARES BY WEIGHTING                 
         A     RF,0(RE)            UPDATE SHARE ACCUMS                          
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT OUTPUT VALUE                            
         LA    RE,4(RE)            NEXT ACCUM                                   
         BCT   R0,GETSHRLP                                                      
GETSHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER  DIVSHR'                   
*******************************************************************             
*                                                                 *             
*        ROUTINE TO DIVIDE SHARES BY TOTAL WEIGHTING              *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
DIVSHR   NTR1                                                                   
*                                                                               
         LA    R0,3                COUNTER                                      
         LA    R1,TOTSHR           POINT TO SHARE ACCUMS                        
         LA    R2,HOMSHR           OUTPUT AREA                                  
*                                                                               
DIVSHR2  L     RF,0(R1)                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                ROUNDED DIVIDE                               
         D     RE,MTHFCTR                                                       
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         ST    RF,0(R2)            UNWEIGHTED VALUE TO OUTPUT                   
         LA    R1,4(R1)            POINT TO NEXT SHARE                          
         LA    R2,4(R2)                                                         
         BCT   R0,DIVSHR2                                                       
*                                                                               
DIVSHRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - OLD STYLE RATES'         
***********************************************************************         
* ROUTINE TO RETURN NEW RATES IN THE OLD FORMAT                                 
***********************************************************************         
OLDRATE  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,X'06'        FIND RATE ELEMENTS                           
         L     R6,RFTAIO1                                                       
         USING RIMAELEM,R6         ESTABLISH MASTER AVAIL RATE ELEMENT          
         BAS   RE,GETEL                                                         
         BNE   HDRRTFDN            NO RATE ELEMENTS                             
*                                                                               
         LA    R0,RFTCRTEN         MAX EIGHT RATE CARDS REQUESTED               
         LA    R2,RFTCRTES         POINT TO REQUESTED RATE CARD LISTS           
R        USING RFTCRTES,R2                                                      
*                                                                               
         LA    R4,RFTFRTEN         MAX EIGHT RATES RETURNED                     
         LA    R3,RFTFRTES         POINT TO RETURNED RATES                      
*                                                                               
HDRRTFLP DS    0H                                                               
         OC    0(RFTCRTSL,R2),0(R2) DONE IF END OF LIST REACHED                 
         BZ    HDRRTFDN                                                         
*                                                                               
         CLC   RIMAREP,RFTCREP     MATCH ON REQUESTING REP                      
         BNE   HDRRTRCN                                                         
*                                                                               
HDRRTCLP DS    0H                  FILTER ON REQUESTED RATES                    
         CLC   RIMACDE,0(R2)       MATCH ON RATE CODE                           
         BNE   HDRRTRCN                                                         
*                                                                               
         CLC   RIMALNTH+1(1),R.RFTCSLN MATCH ON SPOT LENGTH                     
         BNE   HDRRTRCN                                                         
*                                                                               
         MVC   SVINVKEY,KEY                                                     
         XC    KEY,KEY                                                          
IKEYD    USING RINVKEY,KEY                                                      
*                                                                               
*                                  COPY KEY UP TO INV STRT DATE                 
         MVC   KEY(RINVKRTP-RINVKEY),SVINVKEY                                   
*                                  RATE RECORD                                  
         MVI   IKEYD.RINVKRTP,C'Z'                                              
         MVC   IKEYD.RINVKNUM,RIMANUM   EQUATE NUMBER                           
*                                                                               
         EDIT  (B1,R.RFTCQYR),(2,WORK),ZERO=NOBLANK,FILL=0                      
         MVC   WORK+2(4),=C'0101'                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,DUB)                                    
         MVC   IKEYD.RINVKYR,DUB   YEAR - IN BINARY                             
         DROP  R6,IKEYD                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND MATCH?                                 
         BNE   HDRRTICN            NO - GET NEXT INPUT RATE                     
*                                                                               
         MVC   AIO,RFTAIO2         SET I/O AREA                                 
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         USING RIAVL,R6                                                         
*                                                                               
HDRRTC10 DS    0H                                                               
         BNE   HDRRTC30                                                         
         CLC   RIAVQTR,R.RFTCQQTR    SAME QUARTER?                              
         BNE   HDRRTC20                                                         
         OC    RIAVAMT,RIAVAMT     ANY AMOUNT?                                  
         BZ    HDRRTC20                                                         
*                                                                               
         MVC   RFTFRTCD-RFTFRTES(L'RFTFRTCD,R3),R.RFTCRTCD    RTE CD            
         MVC   RFTFRSLN-RFTFRTES(L'RFTFRSLN,R3),R.RFTCSLN     SLN               
         MVC   RFTFRYR-RFTFRTES(L'RFTFRYR,R3),R.RFTCQYR       YEAR              
         MVC   RFTFRQTR-RFTFRTES(L'RFTFRQTR,R3),R.RFTCQQTR    QTR               
         MVC   RFTFRRTE-RFTFRTES(L'RFTFRRTE,R3),RIAVAMT       RATE              
         LA    R3,RFTFRTSL(R3)     BUMP TO NEXT RATE AREA                       
         BCTR  R4,0                DECREMENT RETURNED RATE ENTRIES              
         B     HDRRTC30                                                         
*                                                                               
HDRRTC20 DS    0H                                                               
         BAS   RE,NEXTEL           GET NEXT WEEKLY RATE                         
         B     HDRRTC10                                                         
*                                                                               
HDRRTC30 XC    KEY,KEY                                                          
         MVC   KEY(32),SVINVKEY                                                 
         GOTO1 HIGH                                                             
         MVC   AIO,RFTAIO1                                                      
         GOTO1 GETREC                                                           
         B     HDRRTICN                                                         
*                                                                               
HDRRTRCN DS    0H                                                               
         BAS   RE,NEXTEL           FIND NEXT RATE ELEMENT                       
         BE    HDRRTCLP                                                         
         B     HDRRTICN            NO MORE - GET NEXT INPUT RATE                
*                                                                               
HDRRTICN DS    0H                                                               
         L     R6,RFTAIO1          RESET RATE EL POINTER                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL            HAS TO BE THERE                              
*                                                                               
         LA    R2,RFTCRTSL(R2)     BUMP TO NEXT INPUT RATE                      
         LA    R0,RFTCRTX                                                       
         CR    R2,R0                                                            
         BL    HDRRTFLP                                                         
         DROP  R                                                                
*                                                                               
HDRRTFDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - NEW STYLE RATES'         
***********************************************************************         
* ROUTINE TO RETURN NEW RATES                                                   
*                                                                               
* NOTE: CURYEAR IS ANOTHER NAME FOR BYTE2                                       
***********************************************************************         
NEWRATE  NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,RFTCRDRC      A(INPUT RATE)                                
         BZ    NEWRATEX            DONE IF NO ADDRESS                           
*                                                                               
***  THIS KEY IS USED LATER TO RETRIEVE DEMOS                                   
***      MVC   SVINVKEY,KEY                                                     
*** USE WORK +40 TO SAVE THE KEY INSTEAD SINCE WORK IS CL80                     
*** AND THE KEY IS CL32                                                         
         MVC   WORK+40(L'KEY),KEY                                               
NRATE010 DS    0H                                                               
         OC    0(RFTCRDSL,R2),0(R2) DONE IF END OF LIST REACHED                 
         BZ    NEWRATEX                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(8,RFTCRDDT),(0,WORK)                               
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+10,VGETDAY,VADDAY                     
         GOTO1 VDATCON,DMCB,(0,WORK+16),(3,DUB)                                 
*                                                                               
         MVC   CURYEAR,DUB         SAVE AWAY CURRENT YEAR TO PROCESS            
*                                                                               
         MVI   ELCODE,X'06'        FIND RATE ELEMENTS                           
         L     R6,RFTAIO1                                                       
         USING RIMAELEM,R6         ESTABLISH MASTER AVAIL RATE ELEMENT          
         BAS   RE,GETEL                                                         
         BNE   NEWRATEX            NO RATE ELEMENTS                             
*                                                                               
NRATE020 DS    0H                                                               
         CLC   RIMAREP,RFTCREP     MATCH ON REQUESTING REP                      
         BNE   NRATE030                                                         
*                                                                               
         CLC   RIMACDE,0(R2)       MATCH ON RATE CODE                           
         BNE   NRATE030                                                         
*                                                                               
         CLI   8(R2),C'M'          MATCH ON MINUTES                             
         BNE   *+12                                                             
         TM    RIMALNTH,X'80'                                                   
         BZ    NRATE030                                                         
*                                                                               
         CLC   RIMALNTH+1(1),9(R2) MATCH ON SPOT LENGTH                         
         BNE   *+14                                                             
         MVC   SVEQUNUM,RIMANUM    SAVE AWAY EQUATE #                           
         B     NRATE100            FOUND                                        
*                                                                               
NRATE030 DS    0H                                                               
         BAS   RE,NEXTEL           FIND NEXT RATE ELEMENT IN HEADER             
         BE    NRATE020                                                         
*                                                                               
NRATE040 DS    0H                                                               
         LA    R2,RFTCRDSL(R2)     BUMP TO NEXT INPUT RATE                      
         B     NRATE010                                                         
*-------------------------------------------------------------------            
NRATE100 DS    0H                                                               
         XC    KEY,KEY                                                          
IKEYD    USING RINVKEY,KEY                                                      
         L     RE,RFTAIO1                                                       
*                                  COPY KEY UP TO INV STRT DATE                 
         MVC   KEY(RINVKRTP-RINVKEY),0(RE)                                      
         MVI   IKEYD.RINVKRTP,C'Z' RATE RECORD                                  
         MVC   IKEYD.RINVKNUM,SVEQUNUM  EQUATE NUMBER                           
*                                                                               
         MVC   IKEYD.RINVKYR,CURYEAR   YEAR - IN BINARY                         
         DROP  R6,IKEYD                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND MATCH?                                 
         BNE   NRATE162            NO - GET NEXT INPUT                          
*                                                                               
         MVC   AIO,RFTAIO2         SET I/O AREA                                 
         GOTO1 GETREC                                                           
*                                                                               
         XC    INVEFFST(6),INVEFFST                                             
*                                  CLEAR INVENTORY EFF DATE CHECK AREA          
         OC    RFTFEFST,RFTFEFST   EFFECTIVE START DATE ENTERED?                
         BZ    NRATE103            NO  - DON'T FOOL WITH DATES                  
         GOTO1 VDATCON,DMCB,(2,RFTFEFST),(0,WORK)                               
*                                  CONVERT TO EBCDIC                            
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                  GET DAY OF WEEK                              
         ZIC   RE,DMCB             DAY OF WEEK RETURNED                         
         BCTR  RE,R0               DECREMENT BY 1                               
         LNR   RE,RE               NEGATE FOR SUBTRACTION                       
         ST    RE,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,WORK+0,WORK+12                                       
         GOTO1 VDATCON,DMCB,(0,WORK+12),(19,WORK)                               
*                                  CONVERT YYMMDD EBCDIC TO JULIAN              
         MVC   INVEFFST,WORK       SAVE JULIAN INVENTORY START DATE             
*                                  (THIS IS MONDAY OF WEEK)                     
         OC    RFTFEFEN,RFTFEFEN   EFFECTIVE END   DATE ENTERED?                
         BZ    NRATE103            NO  - LEAVE IT AS ZERO                       
         GOTO1 VDATCON,DMCB,(2,RFTFEFEN),(0,WORK)                               
*                                  CONVERT TO EBCDIC                            
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                  GET DAY OF WEEK                              
         ZIC   RE,DMCB             DAY OF WEEK RETURNED                         
         BCTR  RE,R0               DECREMENT BY 1                               
         LNR   RE,RE               NEGATE FOR SUBTRACTION                       
         ST    RE,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,WORK+0,WORK+12                                       
         GOTO1 VDATCON,DMCB,(0,WORK+12),(19,WORK)                               
*                                  CONVERT YYMMDD EBCDIC TO JULIAN              
         MVC   INVEFFEN,WORK       SAVE JULIAN INVENTORY END   DATE             
*                                  (THIS IS MONDAY OF WEEK)                     
NRATE103 EQU   *                                                                
         MVC   SVRFTDTS,RFTCRDDT   SAVE REPORT START/END DATES                  
*                                                                               
         OC    INVEFFST,INVEFFST   INV EFFECTIVE START DATE PRESENT?            
         BZ    NRATE106            NO  - NO DATE REPLACEMENT                    
         CLC   RFTCRDDT(3),INVEFFST   YES - PROP DATE START < INVEFFST?         
         BNL   NRATE104            NO                                           
         MVC   RFTCRDDT(3),INVEFFST   YES - REPLACE THE DATES                   
NRATE104 EQU   *                                                                
         OC    INVEFFEN,INVEFFEN   INV EFFECTIVE END   DATE PRESENT?            
         BZ    NRATE106            NO  - NO DATE REPLACEMENT                    
         CLC   RFTCRDDT+3(3),INVEFFEN   YES - PROP DATE ST < INVEFFST?          
         BNH   NRATE106            NO                                           
         MVC   RFTCRDDT+3(3),INVEFFEN   YES - REPLACE THE DATES                 
NRATE106 EQU   *                                                                
         LA    R4,RFTFRDEN                                                      
         LA    R3,RFTFRDWK                                                      
         LR    RE,R3                                                            
         LHI   RF,RFTFDDAT-RFTFRDWK                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,RFTAIO2                                                       
         MVI   ELCODE,X'03'                                                     
         USING RIAVL,R6                                                         
         BAS   RE,GETEL                                                         
         BNE   NRATE160            NO MORE WEEK RATE ELEMENTS                   
         BAS   RE,FIXRATE          TEMPORARY SELF-CORRECT CODE                  
         B     NRATE120                                                         
*                                                                               
NRATE110 DS    0H                                                               
         CHI   R4,RFTFRDEN         FOUND ANY RATES YET?                         
         BE    NRATE120                                                         
         CLC   RFTFRDQT,RIAVQTR    NEW QUARTER?                                 
         BE    NRATE120                                                         
*                                                                               
         MVI   RFTMODE,RFTNRTEQ    NEW QUARTER - RETURN PREV QTR RATES          
         BAS   RE,HOOK                                                          
*                                                                               
         LA    R4,RFTFRDEN                                                      
         LA    R3,RFTFRDWK                                                      
         LR    RE,R3                                                            
         LHI   RF,RFTFDDAT-RFTFRDWK                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
NRATE120 DS    0H                                                               
*                                                                               
         CLC   RIAVWEEK,RFTCRDDT   BEFORE START DATE FILTER?                    
         BNL   NRATE130            NO                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(8,RIAVWEEK),(0,WORK)                               
         L     RF,RFTACOM                                                       
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,WORK,WORK+10,7                                         
         GOTO1 VDATCON,DMCB,(0,WORK+10),(19,DUB)                                
         CLC   DUB(3),RFTCRDDT     FALL BETWEEN BROADCAST WEEKS?                
         BH    NRATE140                                                         
         B     NRATE150            NO - GO GET NEXT WEEK                        
*                                                                               
NRATE130 DS    0H                                                               
         OC    RFTCRDDT+3(3),RFTCRDDT+3    ANY END DATE FILTER?                 
         BZ    *+14                        NO                                   
         CLC   RIAVWEEK,RFTCRDDT+3         AFTER END DATE FILTER END?           
         BH    NRATE160            FINISHED W/ RECORD - CALL HOOK               
*                                                                               
NRATE140 DS    0H                                                               
         STCM  R2,15,RFTFRDRC      A(RATE CARD LIST ENTRY)                      
IKEYD    USING RINVKEY,KEY                                                      
         MVC   RFTFRDYR,IKEYD.RINVKNUM  YEAR                                    
         MVC   RFTFRDQT,RIAVQTR    QUARTER                                      
RATE     USING RFTFRDWK,R3                                                      
         MVC   RATE.RFTFRDWK,RIAVWEEK   WEEK                                    
         MVC   RATE.RFTFRDRT,RIAVAMT    RATE COST                               
         DROP  RATE,IKEYD                                                       
*                                                                               
         BCTR  R4,0                DECREMENT COUNTER                            
*                                                                               
         LA    R3,RFTFRDSL(R3)     NEXT RETURN RATE BLOCK                       
NRATE150 DS    0H                                                               
         BAS   RE,NEXTEL           GET NEXT WEEKLY RATE ELEMENT                 
         BNE   NRATE160            NO MORE WEEK RATE ELEMENTS                   
*                                                                               
         LTR   R4,R4                                                            
         BNZ   NRATE110            GET NEXT WEEKLY RATE ELEMENT                 
*                                                                               
         CLC   RFTFRDQT,RIAVQTR    NEW QUARTER?                                 
         BNE   NRATE110                                                         
*                                                                               
* FOR NOW FALL THROUGH ON 14 WEEK QUARTER. JRD                                  
*        DC    H'0'                LONG QUARTER                                 
*                                                                               
NRATE160 DS    0H                                                               
         MVC   RFTCRDDT,SVRFTDTS   RESET DATES OF PROPOSAL                      
*                                                                               
         CHI   R4,RFTFRDEN         FOUND ANY RATES YET?                         
         BE    NRATE162                                                         
*                                                                               
         MVI   RFTMODE,RFTNRTEQ    <<<<<< NEW STYLE RATES                       
         BAS   RE,HOOK                                                          
*-------------------------------------------------------------------            
NRATE162 DS    0H                                                               
         OC    RFTCRDDT+3(3),RFTCRDDT+3 ANY END DATE FILTER?                    
         BZ    NRATE040            NO - GO GET NEXT INPUT RATE                  
*                                                                               
         GOTO1 VDATCON,DMCB,(8,RFTCRDDT+3),(3,DUB)                              
         CLC   CURYEAR,DUB         FINISHED ALL YEARS?                          
         BNL   NRATE040            YES                                          
*                                                                               
         ZIC   RF,CURYEAR          GO GET NEXT YEAR'S RATE RECORD               
         LA    RF,1(RF)                                                         
         STC   RF,CURYEAR                                                       
*                                                                               
         XC    DUB,DUB             BET BROADCAST START DATE FOR CURYEAR         
         MVC   DUB(1),CURYEAR                                                   
         MVC   DUB+1(2),=X'010E'                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+10,VGETDAY,VADDAY                     
         GOTO1 VDATCON,DMCB,(0,WORK+10),(19,DUB)                                
*                                                                               
**JRD    MVC   RFTCRDDT(3),DUB                                                  
*                                                                               
         B     NRATE100                                                         
*                                                                               
NEWRATEX DS    0H                                                               
*****    MVC   KEY(L'SVINVKEY),SVINVKEY                                         
         MVC   KEY,WORK+40                                                      
         XIT1                                                                   
         EJECT                                                                  
*        *                                                                      
*   FIXRATE:  EXAMINES FIRST PERIOD (WEEK OR WHATEVER) IN RATE                  
*        RECORD VS SECOND PERIOD.  IF NOT SAME, EXAMINES FIRST                  
*        PERIOD AMOUNT, COMPARES AGAINST EFFECTIVE DATE OF RECORD.              
*        IF SAME, AMOUNT FROM SECOND PERIOD IS INSERTED INTO                    
*        FIRST PERIOD.  (BUG IN RMP/GLOBAL WAS INSERTING THE                    
*        EFFECTIVE DATE OF THE RECORD ERRONEOUSLY INTO THE                      
*        RATE RECORD).  BILL  /  DEC20/2006.                                    
*                                                                               
FIXRATE  NTR1                                                                   
         LR    R7,R6               FIND NEXT ELEMENT                            
         ZIC   RF,1(R6)                                                         
         AR    R7,RF                                                            
         CLI   0(R7),3             NEXT AN X'03'?                               
         BNE   FRAT0200            NO  - CAN'T BE FIXED, IF WRONG               
         CLC   RIAVAMT-RIAVL(4,R6),RIAVAMT-RIAVL(R7)                            
*                                  AMOUNT SAME IN BOTH BUCKETS?                 
         BE    FRAT0200            YES - LEAVE AS IS.                           
         L     R4,RFTAIO2          SET A(INVENTORY RECORD)                      
         USING RINVREC,R4                                                       
         GOTO1 VDATCON,DMCB,(3,RINVKSTD),(2,DUB)                                
         CLC   RIAVAMT+1-RIAVL(2,R6),DUB                                        
*                                  VALUE IN BUCKET 1 = EFFECTIVE DATE?          
         BNE   FRAT0200            NO  - CAN'T FIX IT: COULD BE VALUE           
         MVC   RIAVAMT-RIAVL(4,R6),RIAVAMT-RIAVL(R7)                            
*                                  YES - SET 2ND BUCKET VALUE INTO              
*                                     FIRST BUCKET                              
FRAT0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BANNER   DC    C'BILLUHR: JUN14/2007 - RATE DATES'                              
         TITLE 'REFETCH - INVENTORY RECORD RETRIEVER - RFTWRKD'                 
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RFTWRKD  DSECT                                                                  
VCALLOV  DS    V                                                                
VCHOPPER DS    V                                                                
VDATCON  DS    V                                                                
VDATAMGR DS    V                                                                
VDEFINE  DS    V                                                                
VDEMOUT  DS    V                                                                
VDEMOVAL DS    V                                                                
VDEMAND  DS    V                                                                
VDEMAINT DS    V                                                                
VDEMOCON DS    V                                                                
VDEMOMTH DS    V                                                                
VDEMUP   DS    V                                                                
VGETIUN  DS    V                                                                
*                                                                               
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VGETBRD  DS    V                                                                
VNSIWEEK DS    V                                                                
VDAYPAK  DS    V                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
CURYEAR  DS    0X                  SPAME AS BYTE2                               
BYTE2    DS    X                                                                
SVEQUNUM DS    0X                                                               
BYTE3    DS    X                                                                
RELO     DS    F                                                                
USERRD   DS    F                                                                
SVAPARM  DS    F                                                                
*                                                                               
PARMS    DS    0XL24                                                            
PARM1    DS    F                                                                
PARM2    DS    F                                                                
PARM3    DS    F                                                                
         DS    3F                                                               
*                                                                               
DMCB     DS    6F                  DATAMGR PARAMETER LIST                       
DMWORK   DS    XL96                WORKAREA FOR DATAMGR                         
WORK     DS    XL80                                                             
*                                                                               
AIO      DS    A                   A(I/O AREA)                                  
ABUFFSTR DS    A                   A(START OF TEXT BUFFER)                      
ABUFF    DS    A                   A(NEXT AVAILABLE LINE IN BUFFER)             
ATXTWRK  DS    A                   A(TEXT WORKAREA)                             
*                                                                               
SVDTMA   DS    A                   A(CURRENT ITEM IN DPT/DAY/TIME LIST)         
SVDPT    DS    CL1                 CURRENT DAYPART                              
SVNUMDPT DS    XL1                 NUMBER OF REQUESTED DEMOS                    
SVSOURCE DS    CL1                 CURRENT SOURCE                               
SVDEMLST DS    XL(25*3)            DEMO CODES SAVEAREA                          
*                                                                               
SVBBK    DS    0XL3                BASE BOOK FOR UPGRADES                       
SVBBKSRC DS    XL1                 SOURCE                                       
SVBBKYM  DS    XL2                 BOOK YM                                      
*                                                                               
ACTDEMOS DS    XL96                DEMO WORKAREA                                
ARAVLNEL DS    A                   A(RAVLNEL)                                   
*                                                                               
ELCODE   DS    X                   ELEMENT ID FOR GETEL                         
*                                                                               
OPTION   DS    CL1                 TEXT TYPE                                    
SVPGMCD  DS    XL(L'RINVCODE)      PROGRAM CODE SAVEAREA                        
MULTSW   DS    XL1                                                              
LINSW    DS    XL1                 'L' - LEVELS                                 
*                                  'S' - SHARES                                 
*                                                                               
SVWRPOPT DS    XL1                 WRAP OPTION SAVEAREA                         
*                                                                               
PRINTOPT DS    XL1                 PRINT OPTION                                 
PROPTXFQ EQU   X'80'               PRINT TEXT FILTERS                           
PROPTUGQ EQU   X'02'               BOOK IS AN UPGRADE                           
*                                                                               
SVLST    DS    CL10                VALID ARB, NSI OR SRC SOURCE CODES           
INVSRC   DS    XL1                 1ST BYTE FROM BOOKVAL                        
SKIPREC  DS    XL1                 FLAG TO SKIP RECORD OR NOT                   
*                                                                               
         DS    0D                                                               
KEY      DS    XL32                KEY AREA                                     
KEYSAVE  DS    XL32                KEY SAVE AREA                                
*                                                                               
SVPINVKY DS    XL32                PASSIVE POINTER SAVEAREA                     
SVINVKEY DS    XL32                INVENTORY KEY   SAVEAREA                     
*                                                                               
TPCNT    DC    AL1(0)                                                           
HITCNT   DC    AL1(0)              Y/N DEMO HOOKS FLAG(0/1)                     
PCNT     DC    AL1(0)                                                           
VARSW    DS    C                                                                
PROGEL   DS    CL255                                                            
CEELS    DS    9CL10               DAY/TIMES ELEMENT BUILD AREA                 
         ORG   CEELS                                                            
CEEL     DS    CL10                                                             
         ORG                                                                    
CDEL     DS    CL10                                                             
         SPACE 1                                                                
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    F                                                                
MTHFCTR  DS    F                                                                
MTHIFIL  DS    CL3                                                              
MTHOFIL  DS    CL3                                                              
MTHOSRC  DS    CL3                                                              
         SPACE 1                                                                
MILEDIT  DS    V                                                                
ADATAREC DS    A                                                                
AINTEREC DS    A                                                                
ADEMWRK  DS    A                                                                
AIUNWORK DS    A                                                                
AEXPWORK DS    A                                                                
AALIAS   DS    A                   ALIASES:  11 * (5 / STA, 12 / ALIAS)         
*                                     TOTAL = 187 + DELIMITER                   
HOMSHR   DS    3F                                                               
TOTSHR   DS    3F                                                               
DEMODUB  DS    D                                                                
*                                                                               
OTFORMAT DS    XL10                                                             
*                                                                               
BLOCK    DS    CL80                WORKAREA                                     
*                                                                               
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC FOR FILTER BOOK                        
*                                                                               
GFRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GFIRSVC  DS    CL1                 RATING SERVICE                               
GFIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
****GFIKSRC  DS    CL1                 RINVKSRC FOR KEY                         
GFIBITS  DS    XL1                 BOOKVAL BITS                                 
GFIBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GFRCINL  EQU   *-GFRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GFRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GFORSVC  DS    CL1                 RATING SERVICE                               
GFOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
*****GFOKSRC  DS    CL1                 RINVKSRC FOR KEY                        
GFOBITS  DS    XL1                 BOOKVAL BITS                                 
GFOBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GFRCOUTL EQU   *-GFRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
         DS    0F                                                               
DBLOCKA1 DS    CL(L'DBLOCK)        OVERLAY DBLOCK                               
         DS    CL4                 DEMAND SCREWS THIS UP                        
DBEXTRA1 DS    XL512               --EXTRA ADDRESS BLOCK                        
*                                                                               
         DS    0D                                                               
*                                                                               
AUPELEM  DS    A                   A(UPGRADE ELEMENT)                           
OVRNTDUB DS    D                   OVERNIGHT DUB RETURN VALUES                  
*                                                                               
SHRSRC   DS    CL1                 RATING SERVICE OF SHARE BOOK                 
SHRBK    DS    XL2                 SHARE BOOK                                   
SHRBTYP  DS    CL1                 SHARE BOOK BOOKTYPE                          
*                                                                               
SVALISTA DS    CL5                                                              
SVALIMKT DS    CL3                                                              
OVRNTDTE DS    CL2                 TEMP STORE: DBSELBK O'RIDE                   
OVRNTDAY DS    CL1                 TEMP STORE: DBSELDAY O'RIDE                  
*                                                                               
OVRNTTYP DS    CL1                 OVERNIGHT TYPE STORAGE                       
OVUPGFLG DS    CL1                 OVERNIGHT UPGRADE FLAG                       
*                                  0     =  NOT OVERNIGHT AVG                   
*                                  X'FA' = OVERNIGHT AVG                        
OVERRFLG DS    CL1                 OVERNIGHT ERROR   FLAG                       
*                                                                               
INVEFFST DS    XL3                 INVENTORY EFFECTIVE START: JULIAN            
INVEFFEN DS    XL3                 INVENTORY EFFECTIVE END  : JULIAN            
SVRFTDTS DS    XL6                 SAVE JULIAN DATES OF REPORT                  
         DS    0F                                                               
DWRKAREA DS    4000X               DOUBLE-WORD DEMO WORK AREA                   
DWRKAREL EQU   *-DWRKAREA          AREA LENGTH                                  
*                                                                               
RFTWRKL  EQU   *-RFTWRKD                                                        
*                                                                               
PURWRKD  DSECT                                                                  
INVNOUPT DS    X                   INV RECD: NO MANIPULATION OF DATA            
PURWRKX  EQU   *                                                                
*                                                                               
* PROGRAM NAME TEXT ELEMENT DSECT                                               
*                                                                               
PROGELD  DSECT                                                                  
PCODE    DS    X                                                                
PELLEN   DS    X                                                                
PLIN     DS    X                                                                
         DS    CL3                 SPARE                                        
PFBK     DS    0CL5                FROM BOOK                                    
PMON     DS    CL3                                                              
PYR      DS    CL2                                                              
         DS    CL1                 SPARE                                        
PINVCODE DS    CL2                                                              
PEQS     DS    CL1                                                              
PNAME1   DS    CL16                FIRST PROGRAM NAME                           
PNAME2   DS    CL16                SECOND PROGRAM NAME                          
PROGELX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR SPGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMESHR  DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
DUPGOVNT EQU   11+14+1             DISPLACE IN UPGRADE TO OVERNIGHT             
*                                     BOOK LIST                                 
LENVALS  EQU   NUMVALS*4                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE REFETCHD                                                       
*                                                                               
         ORG   RFTSAVE                                                          
RFTPAREP DS    XL2                 PARENT REP CODE                              
RFTSRMPP DS    XL8                 RMP PROGRAM PROFILE                          
*                                                                               
         EJECT                                                                  
*RERMPPROF                                                                      
*        PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
*        PRINT ON                                                               
*DEDBLOCK                                                                       
DBLOCKD  DSECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
*        PRINT ON                                                               
*DEDBEXTRAD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
*        PRINT ON                                                               
*REGENINVA                                                                      
*        PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
*        PRINT ON                                                               
*REGENAVL                                                                       
*        PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
*        PRINT ON                                                               
*REGENREPA                                                                      
*        PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
*        PRINT ON                                                               
*REGENSTA                                                                       
*        PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
*        PRINT ON                                                               
*DDCOMFACS                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
*        PRINT ON                                                               
*DDCOREQUS                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
*        PRINT ON                                                               
*DEMTABD                                                                        
*        PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REFETCH   12/07/12'                                      
         END                                                                    
