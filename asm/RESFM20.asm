*          DATA SET RESFM20    AT LEVEL 126 AS OF 05/01/02                      
*PHASE T81820A,*                                                                
*INCLUDE NUMVAL                                                                 
         TITLE 'T81820 - RESFM20 - BUDGET SPREADER'                             
***********************************************************************         
*                                                                     *         
*        RESFM20 (T81820) --- BUDGET SPREADER MAINT/LIST/REPORT       *         
*                                                                     *         
*        SCREEN FOR MAINTENANCE: RESFMBF                              *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*  ALLOCATES BUDGET $'S FOR A GIVEN YEAR. STATION/OFFICE ARE          *         
*  OPTIONAL FOR OVERNIGHT RUN, BUT 1 MUST BE GIVEN IF DOING 'NOW'.    *         
*                                                                     *         
*  SPLITS MAY BE EVEN (DIVIDE TOTAL $'S BY 12) OR BY SPECIFIC         *         
*  PERCENTAGES. (0.00 - 100.)  PERCENTAGES MUST TOTAL 100.00 EXACTLY. *         
*                                                                     *         
*  EXTRA DOLLARS (FROM ROUNDING) ARE ADDED TO 1ST NON-0 MONTH.        *         
*                                                                     *         
*                                                                     *         
*  TWO MODES:  ALLOCATE OR DE-ALLOCATE.                               *         
*                                                                     *         
*  ALLOCATE - TAKES MONEY FROM TOTAL BUDGET FIELD AND SPREADS         *         
*             ACROSS MONTHS AS INDICATED.  BUDGET RECORDS ALREADY     *         
*             'SPREAD' ARE EXCEPTIONS.                                *         
*                                                                     *         
*  DE-ALLOCATE - REMOVES MONEY FROM EACH MONTH.  RECORDS WHERE        *         
*                MONTHLY $'S DON'T SUM TO RECORD TOTAL ARE EXCEPTIONS *         
*                                                                     *         
*                                                                     *         
*  OPTION: VERIFICATION REPORT ONLY - A 'SOFT' RUN (NO RECORD         *         
*          UPDATES).  REPORTS ON EXCEPTIONS ONLY.                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 11/16/89  PJS  ORIGINAL DEVELOPMENT.                                *         
*                                                                     *         
* 01/16/90  PJS  CLEAN UP PAGE BREAKS (NEAT BLOCKING)                 *         
*                                                                     *         
* 03/01/91  BU   PERMIT SPREAD BY CONTRACT TYPE.  ADD 'FORCE' FEATURE.*         
*                                                                     *         
* 04/02/91  BU   TIGHTEN UP END-OF-RECORD PROCESSING.                 *         
*                                                                     *         
* 01/09/96  WSB  ADD OPTION TO ENTER ACTUAL DOLLAR AMOUNTS RATHER     *         
*                THAN PERCENTAGES                                     *         
*                                                                     *         
*                     *** END TOMBSTONE ***                           *         
***********************************************************************         
         EJECT                                                                  
T81820   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1820**,R7,RR=R3   <<--- NOTE R7 = 2ND BASE REG               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
*                                                                               
*- MAINLINE -- IF WE DON'T FIND A MODE WE LIKE, GET OUT ASAP!                   
*                                                                               
         LA    R1,MODELIST                                                      
MAIN100  CLC   =F'0',0(R1)                                                      
         BE    EXIT                NO MODE MATCH.  OUT.                         
*                                                                               
         CLC   MODE(1),0(R1)                                                    
         BE    MAIN120                                                          
*                                                                               
         LA    R1,4(R1)            NEXT TABLE ENTRY                             
         B     MAIN100                                                          
*                                                                               
*- CALL PROCESS ROUTINE                                                         
MAIN120  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         A     RF,RELO             RELOCATE                                     
         BASR  RE,RF                                                            
*                                                                               
EXIT     XIT1                                                                   
         SPACE 2                                                                
MODELIST DS    0F                  AL1(MODE EQUATE),AL3(ROUTINE)                
         DC    AL1(VALKEY),AL3(VKEY)                                            
         DC    AL1(PRINTREP),AL3(SPREADER)                                      
         DC    F'0'                EOT                                          
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         XC    FILTERS(LFILTERS),FILTERS                                        
         SPACE 2                                                                
*                                                                               
*- VALIDATE YEAR.  MUST BE 4 NUMERIC CHARACTERS                                 
         LA    R2,BUDYEARH                                                      
         LA    RE,INVYEAR                                                       
         LA    RF,L'INVYEAR                                                     
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),4             4 BYTES OF INPUT?                            
         BNE   ERREXIT                                                          
*                                                                               
         TM    4(R2),X'08'         FIELD IS VALID NUMERIC                       
         BZ    ERREXIT                                                          
*                                                                               
         MVC   HEADYEAR,8(R2)      SAVE FOR HEADLINE PRINTING                   
*                                                                               
         MVC   FILTYEAR,8+2(R2)    YEAR FILTER = LAST 2 YEAR DIGITS             
         CLI   FILTYEAR,C'0'       FIRST DECADE/YEAR 2000?                      
         BNE   VKEY0020            NO                                           
         MVI   FILTYEAR,X'FA'      YES - SET TO INTERNAL FORMAT                 
         B     VKEY0100                                                         
VKEY0020 EQU   *                                                                
         CLI   FILTYEAR,C'1'       2ND   DECADE/YEAR 2000?                      
         BNE   VKEY0040            NO                                           
         MVI   FILTYEAR,X'FB'      YES - SET TO INTERNAL FORMAT                 
         B     VKEY0100                                                         
VKEY0040 EQU   *                                                                
         CLI   FILTYEAR,C'2'       3RD   DECADE/YEAR 2000?                      
         BNE   VKEY0100            NO                                           
         MVI   FILTYEAR,X'FC'      YES - SET TO INTERNAL FORMAT                 
         B     VKEY0100                                                         
*                                                                               
*   NOTE  - THIS TAKES CARE OF 2000-2029.  BEYOND THAT, I FRANKLY               
*        DON'T CARE.   BILL UHR.  FEB 26, 1999.                                 
*                                                                               
VKEY0100 EQU   *                                                                
                                                                                
         SPACE 2                                                                
*                                                                               
*- VALIDATE OFFICE.                                                             
         LA    R2,BUDOFFH                                                       
         LA    RE,INVOFF                                                        
         LA    RF,L'INVOFF                                                      
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0200                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           RECORD ID                                    
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREXIT             OFFICE NOT ON FILE                           
*                                                                               
         MVC   FILTOFF,8(R2)       SAVE OFFICE FILTER                           
         SPACE 2                                                                
*                                                                               
*- VALIDATE STATION                                                             
VKEY0200 EQU   *                                                                
         LA    R2,BUDSTAH                                                       
         LA    RE,INVSTATN                                                      
         LA    RF,L'INVSTATN                                                    
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0250                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),AGENCY                                                 
         OC    KEY+22(5),SPACES                                                 
*                                                                               
         ZIC   R0,5(R2)            PARSE OUT POSSIBLE '-'                       
         LA    RF,8(R2)                                                         
         LA    RE,KEY+22                                                        
VKEY0210 CLI   0(RF),C'-'                                                       
         BNE   VKEY0215                                                         
*                                                                               
         LA    RF,1(RF)            SKIP THE '-'                                 
         LA    RE,KEY+26           BAND MUST GO HERE                            
         BCTR  R0,0                LESS 1 ON IPT LEN (THE '-')                  
         LTR   R0,R0                                                            
         BZ    VKEY0220            NOTHING AFTER THE '-' (WABC- )               
         LA    R6,1                OUT OF LOOP AFTER NEXT MOVE.                 
*                                                                               
VKEY0215 MVC   0(1,RE),0(RF)       MOVE DATA TO KEY                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VKEY0210                                                      
*                                                                               
*- IF BAND = 'T' (TELEVISION) THEN BLANK OUT BAND IN KEY.                       
VKEY0220 EQU   *                                                                
         CLI   KEY+26,C'T'                                                      
         BNE   VKEY0225                                                         
         MVI   KEY+26,C' '         TV IS BLANK ON FILE                          
*                                                                               
VKEY0225 GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREXIT             NOT ON FILE                                  
*                                                                               
         MVC   FILTSTA,KEY+22      SAVE STATION FILTER                          
         SPACE 2                                                                
*                                                                               
*- CHECK IF ANY CONTRACT TYPES ENTERED.  IF NOT, WERE ANY REQUIRED?             
VKEY0250 EQU   *                                                                
         LA    R2,BUDCTYPH         CONTRACT TYPE INPUT HEADER                   
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VKEY0255            YES - SKIP PROFILE CHECK                     
         LA    R5,SVPGPBIT         PROGRAM PROFILE BITS                         
         LA    R4,64               ADJUST: COVER REG SETTING                    
         SR    R5,R4               IS CONHEADH - 64!!                           
         TM    0(R5),X'40'         1TH BIT ON?                                  
         BNO   VKEY0300            NOT REQUIRED                                 
         B     VKEY0257            NO  - NO INPUT = ERROR                       
VKEY0255 EQU   *                                                                
         GOTO1 SCANNER,DMCB,(0,(R2)),(5,CTYPWORK)                               
         MVC   CTYPCTR,DMCB+4      SAVE INPUT COUNT FOR LATER                   
         ZIC   R5,DMCB+4           TAKE INPUT COUNT                             
         LTR   R5,R5               ANY FIELDS FOUND?                            
         BNZ   VKEY0258            YES                                          
VKEY0257 EQU   *                                                                
         LA    RE,CTYPNG           CON TYPE NEEDED MSG                          
         LA    RF,L'CTYPNG                                                      
         STM   RE,RF,AERRMSG                                                    
         B     ERREXIT             EXIT WITH MESSAGE                            
VKEY0258 EQU   *                                                                
         LA    R3,CTYPWORK+12      POINT TO DATA IN SCANNER BLK                 
         LA    R1,CTYPARAY         A(STORAGE AREA FOR CON TYPES)                
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           SET CODE FOR CON TYPE                        
         MVC   KEY+24(2),AGENCY    SET REP CODE (FROM 'AGENCY')                 
VKEY0260 EQU   *                                                                
         MVC   KEY+26(1),0(R3)     INSERT CONTRACT TYPE CODE                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VKEY0270            NOT FOUND - ERROR OFF                        
         MVC   0(1,R1),0(R3)       SAVE CONTRACT TYPE IN ARRAY                  
         LA    R3,32(R3)           BUMP A(TYPES) IN SCANNER BLK                 
         LA    R1,1(R1)            BUMP A(CONTRACT TYPE ARRAY)                  
         BCT   R5,VKEY0260         GO BACK FOR NEXT                             
         B     VKEY0300            ALL OKAY - PROCEED                           
VKEY0270 EQU   *                                                                
         LA    RE,CONTYPNF         CONTRACT TYPE NOT FOUND MSG                  
         LA    RF,L'CONTYPNF                                                    
         STM   RE,RF,AERRMSG                                                    
         B     ERREXIT             EXIT OUT                                     
*                                                                               
*- IF WE ARE 'NOW' MODE, USER MUST SPECIFY OFFICE OR STATION                    
VKEY0300 EQU   *                                                                
         OC    FILTOFF,FILTOFF                                                  
         BNZ   VKEY0320                                                         
*                                                                               
         OC    FILTSTA,FILTSTA                                                  
         BNZ   VKEY0320                                                         
*                                                                               
         LA    R2,BUDOFFH                                                       
         LA    RE,NEEDFILT                                                      
         LA    RF,L'NEEDFILT                                                    
         STM   RE,RF,AERRMSG                                                    
         TM    WHEN,X'40'          NOW = X'40'                                  
         BO    ERREXIT                                                          
         SPACE 2                                                                
*                                                                               
*- VALIDATE PROCESS MODE: ALLOCATE OR DE-ALLOCATE                               
VKEY0320 EQU   *                                                                
         LA    R2,BUDMODEH                                                      
         LA    RE,INVMODE                                                       
         LA    RF,L'INVMODE                                                     
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),1             MUST BE 1 (AND ONLY 1) BYTE INPUT            
         BNE   ERREXIT                                                          
*                                                                               
         CLI   8(R2),C'A'          ALLOCATE?                                    
         BE    VKEY0324                                                         
         CLI   8(R2),C'D'          DE-ALLOCATE?                                 
         BE    VKEY0324                                                         
*                                                                               
         B     ERREXIT                                                          
*                                                                               
VKEY0324 MVC   PROCMODE,8(R2)      SAVE PROCESS MODE                            
         SPACE 2                                                                
*                                                                               
*- VALIDATE FORCE MODE: YES OR NO                                               
VKEY0330 EQU   *                                                                
         MVI   FORCMODE,C'N'       SET FORCE MODE TO NO                         
         LA    R2,BUDFORCH                                                      
         LA    RE,FALLOC           FORCE ALLOCATION ERROR MSG                   
         LA    RF,L'FALLOC                                                      
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),0             NO INPUT: FORCE = NO                         
         BE    VKEY0340                                                         
*                                                                               
         CLI   5(R2),1             MUST BE 1 (AND ONLY 1) BYTE INPUT            
         BNE   ERREXIT                                                          
*                                                                               
         CLI   8(R2),C'Y'          ALLOCATE?                                    
         BE    VKEY0334                                                         
         CLI   8(R2),C'N'          DE-ALLOCATE?                                 
         BE    VKEY0334                                                         
*                                                                               
         B     ERREXIT                                                          
*                                                                               
VKEY0334 MVC   FORCMODE,8(R2)      SAVE FORCE MODE                              
         SPACE 2                                                                
*                                                                               
*- VERIFICATION REPORT ONLY?                                                    
VKEY0340 EQU   *                                                                
         LA    R2,BUDVERH                                                       
         LA    RE,YORN                                                          
         LA    RF,L'YORN                                                        
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   8(R2),C'Y'          YES?                                         
         BE    VKEY0360                                                         
         CLI   8(R2),C'N'          NO?                                          
         BE    VKEY0360                                                         
*                                                                               
         B     ERREXIT                                                          
*                                                                               
VKEY0360 MVC   SOFTRUN,8(R2)       SOFT/HARD INDICATOR                          
         SPACE 2                                                                
*                                                                               
*- USE DOLLARS?                                                                 
         MVI   USEDOLL,C'N'        ASSUME USING PERCENTAGES                     
         LA    R2,BUDDOLH                                                       
         LA    RE,YORN                                                          
         LA    RF,L'YORN                                                        
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   5(R2),0             DON'T REQUIRE THEM TO ENTER IT               
         BE    VKEY0400                                                         
*                                                                               
         CLI   8(R2),C'Y'          YES?                                         
         BE    VKEY0370                                                         
         CLI   8(R2),C'N'          NO?                                          
         BE    VKEY0370                                                         
*                                                                               
         B     ERREXIT             REQUIRE YES OR NO                            
*                                                                               
VKEY0370 MVC   USEDOLL,8(R2)       DOLLAR INDICATOR                             
*                                                                               
         SPACE 2                                                                
*                                                                               
*- EVEN SPLIT?  DEFAULT = NO                                                    
VKEY0400 EQU   *                                                                
         CLI   PROCMODE,C'D'                                                    
         BE    VKEY0550            SPLITS DON'T APPLY TO DEALLOC.               
*                                                                               
         LA    R2,BUDEVENH                                                      
         LA    RE,YORN                                                          
         LA    RF,L'YORN                                                        
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         MVI   EVEN,C'N'           ASSUME NOT EVEN SPLIT                        
*                                                                               
         CLI   5(R2),0             USE DEFAULT                                  
         BE    VKEY0450                                                         
*                                                                               
         CLI   8(R2),C'Y'          YES?                                         
         BE    VKEY0420                                                         
         CLI   8(R2),C'N'          NO?                                          
         BE    VKEY0420                                                         
*                                                                               
         B     ERREXIT                                                          
*                                                                               
VKEY0420 LA    RE,NOTWIDOL                                                      
         LA    RF,L'NOTWIDOL                                                    
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   USEDOLL,C'N'                                                     
         BE    *+12                                                             
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BE    ERREXIT             CAN'T HAVE EVEN SPLIT IF DOLLARS             
*                                                                               
         MVC   EVEN,8(R2)          EVEN SPLIT INDICATOR                         
*                                                                               
VKEY0450 EQU   *                   DISPLAY J.I.C. USING DEFAULT                 
         MVC   8(1,R2),EVEN                                                     
         FOUT  (R2)                                                             
         SPACE 2                                                                
*                                                                               
*- VALIDATE 12 MONTH VALUES (PERCENTAGES OR DOLLARS)                            
*  0-100.   (BLANK = 0)                                                         
         CLI   USEDOLL,C'Y'        USE PROPER ERROR MESSAGE                     
         BE    VKEY0452                                                         
*                                                                               
         LA    RE,INVALLOC                                                      
         LA    RF,L'INVALLOC                                                    
         B     VKEY0454                                                         
*                                                                               
VKEY0452 LA    RE,INVNUM                                                        
         LA    RF,L'INVNUM                                                      
*                                                                               
VKEY0454 STM   RE,RF,AERRMSG                                                    
*                                                                               
         LA    R3,12               LOOP FOR 12 MONTHS                           
         XC    MONALLOC(12*4),MONALLOC                                          
         XC    TOTALLOC,TOTALLOC                                                
         MVI   ANYALLOC,C'N'                                                    
*                                                                               
         LA    R2,BUDM01H          JANUARY ALLOCATION SCREEN FIELD              
         LA    R4,MONALLOC         PUT BINARY ALLOCATIONS HERE                  
*                                                                               
VKEY0460 BAS   RE,VALPCT           VALIDATE 1 FIELD                             
         BNZ   ERREXIT                                                          
*                                                                               
         MVC   0(4,R4),DMCB+4      SAVE BINARY ALLOCATION VALUE                 
         LA    R4,4(R4)                                                         
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               A(MONTH LABEL)   (PROTECTED)                 
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               A(NEXT PECENTAGE FIELD)                      
         BCT   R3,VKEY0460                                                      
*                                                                               
         CLI   USEDOLL,C'Y'        IF DOLLARS, SKIP PERCENTAGE STUFF            
         BE    VKEY0550                                                         
*                                                                               
*- EVEN SPLIT/ALLOCATION CROSS EDITS                                            
*        CAN'T HAVE BOTH                                                        
*        MUST HAVE 1 OR THE OTHER                                               
*                                                                               
         LA    R2,BUDEVENH                                                      
         LA    RE,PICK1                                                         
         LA    RF,L'PICK1                                                       
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLI   EVEN,C'N'                                                        
         BE    VKEY0500                                                         
*                                                                               
         CLI   ANYALLOC,C'Y'       EVEN SPLIT. NO ALLOC ALLOWED                 
         BE    ERREXIT                                                          
         B     VKEY0550                                                         
         SPACE                                                                  
*                                                                               
*- NOT EVEN SPLIT.  MUST HAVE ALLOCATION                                        
VKEY0500 EQU   *                                                                
         LA    R2,BUDM01H          POINT TO 1ST MONTH ALLOC FIELD               
         CLI   ANYALLOC,C'Y'                                                    
         BNE   ERREXIT             NEED 1 OR THE OTHER                          
*                                                                               
*- TOTAL ALLOCATION MUST = 100.00 PERCENT.                                      
         LA    RE,NOT100                                                        
         LA    RF,L'NOT100                                                      
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         CLC   TOTALLOC,=F'10000'  100.00 (DECIMALS STORED IN NUMBER)           
         BNE   ERREXIT                                                          
         SPACE 2                                                                
*                                                                               
*- END OF VALIDATIONS                                                           
VKEY0550 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- VALIDATE 1 ALLOCATION PERCENTAGE FIELD.                                      
*  INPUT: R2 = A(FIELD HEADER)                                                  
*                                                                               
*  RETURN: CC 0 = VALID, ^0 = ERROR                                             
*          ANYALLOC MARKED W/'Y' IF ANY INPUT                                   
*          VALID ALLOCATION ADDED TO TOTALLOC FULLWORD                          
*          DMCB+4 = BINARY ALLOCATION VALUE (2 IMPLIED DECIMALS)                
*                                                                               
VALPCT   NTR1                                                                   
         XC    DMCB+4(4),DMCB+4    ASSUME NO INPUT                              
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    VPCTGOOD            NO INPUT = 0 PERCENT                         
*                                                                               
         MVI   ANYALLOC,C'Y'       INPUT ENTERED IN ALLOC FLD                   
*                                                                               
         LA    R4,8(R2)            A(DATA TO VALIDATE)                          
*                                                                               
         MVI   NUMDEC,2            ASSUME PERCENT (2 DECIMAL PLACES)            
         CLI   USEDOLL,C'N'                                                     
         BE    *+8                                                              
*                                                                               
         MVI   NUMDEC,0            IF DOLLARS, NO DECIMAL PLACES                
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,(NUMDEC,(R4)),(R3),RR=RELO                       
         CLI   DMCB,X'0'                                                        
         BNE   VPCTBAD                                                          
*                                                                               
         CLC   DMCB+4(4),=F'0'     MIN VALUE                                    
         BL    VPCTBAD                                                          
*                                                                               
         CLI   USEDOLL,C'Y'                                                     
         BE    *+14                NO MAX VALUE IF DOLLARS ARE USED             
*                                                                               
         CLC   DMCB+4(4),=F'10000' MAX VALUE (100.00%)                          
         BH    VPCTBAD                                                          
*                                                                               
         L     RF,TOTALLOC         ADD TO RUNNING TOTAL                         
         A     RF,DMCB+4                                                        
         ST    RF,TOTALLOC                                                      
*                                                                               
VPCTGOOD SR    R0,R0               CC 0                                         
         B     VPCTEXIT                                                         
*                                                                               
VPCTBAD  LTR   RD,RD               CC ^0                                        
VPCTEXIT XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
SPREADER DS    0H                                                               
         CLI   USEDOLL,C'N'                                                     
         BE    *+8                                                              
*                                                                               
         BAS   RE,DOLTOPCT         CONVERT DOLLARS TO PERCENTS                  
*                                                                               
         BAS   RE,SPINIT           INITIALIZATION                               
         BNZ   SPEXIT                                                           
*                                                                               
SPR100   BAS   RE,NEXTBUD          FIND 1ST/NEXT BUDGET REC                     
         BNZ   SPR200              EOF                                          
*                                                                               
         BAS   RE,CALC             DO ALLOCATION/DEALLOCATION                   
         BNZ   SPR180                                                           
*                                                                               
         BAS   RE,MARKREC          UPDATE THE FILE                              
*                                                                               
SPR180   EQU   *                                                                
         GOTO1 SPOOL,DMCB,(R8)     LEAVE A BLANK LINE                           
         B     SPR100              LOOP BACK FOR NEXT                           
         SPACE                                                                  
*                                                                               
*- END OF RUN, CLEAN UP AND EXIT                                                
SPR200   EQU   *                                                                
         BAS   RE,CLEANUP                                                       
SPEXIT   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- DOLLARS TO PERCENTS                                                          
*                                                                               
*     THIS ROUTINE TAKES THE MONTHLY DOLLAR ALLOCATIONS, FINDS THE              
*     TOTAL, AND THEN DIVIDES EACH MONTH FIGURE TO FIND THE PERCENT             
*     OF THE TOTAL THAT IT IS (ROUNDED OFF TO 2 DECIMAL PLACES).                
*     THE PERCENTAGE IS THEN PUT BACK IN THE MONTHLY BUCKET TO BE USED          
*     LATER.                                                                    
*                                                                               
DOLTOPCT NTR1                                                                   
         LA    R2,MONALLOC         MONTHLY ALLOCATIONS                          
         LA    R3,12               ADD UP 12 MONTHS                             
         SR    R4,R4               RUNNING TOTAL ACCUM                          
*                                                                               
DOLTO10  A     R4,0(R2)            ADD TO TOTAL                                 
         LA    R2,4(R2)            NEXT BUDGET AMOUNT                           
         BCT   R3,DOLTO10                                                       
*                                                                               
         LTR   R4,R4               IF IT'S 0, GET OUT (DON'T DIV BY 0)          
         BZ    DOLEXIT                                                          
*                                                                               
         CVD   R4,DUB                                                           
         ZAP   PACKTOT,DUB         PACKED VERSION OF TOTAL                      
*                                                                               
         LA    R2,MONALLOC                                                      
         LA    R3,12                                                            
DOLTO20  L     R4,0(R2)            MONTHLY VALUE                                
         CVD   R4,DUB                                                           
         ZAP   PACKMON,DUB                                                      
         MP    PACKMON,=P'100000'  3 DECIMAL PLACES (TO ROUND OFF)              
         DP    PACKMON,PACKTOT                                                  
         SRP   PACKMON(L'PACKMON-L'PACKTOT),64-1,5     ROUND OFF                
         ZAP   DUB,PACKMON(L'PACKMON-L'PACKTOT)                                 
         CVB   R4,DUB                                                           
         ST    R4,0(R2)            PUT BACK IN MONTHLY BUCKET                   
         LA    R2,4(R2)            GET NEXT VALUE                               
         BCT   R3,DOLTO20                                                       
*                                                                               
DOLEXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- SPREADER INITIAL                                                             
*                                                                               
SPINIT   NTR1                                                                   
         LA    R1,HEDSPECS         FILE SPECS                                   
         ST    R1,SPECS                                                         
*                                                                               
         LA    R1,HOOK             HEADLINE ROUTINE                             
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    RPTBKTS(LRPTBKTS),RPTBKTS      0 BKTS, ACCUMS, ETC.              
*                                                                               
*- READ REP RECORD TO GET FISCAL YEAR START MONTH                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),AGENCY    REP POWER CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD?                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING REPRECD,R2                                                       
         MVC   FYSTART,RREPFMON                                                 
         DROP  R2                                                               
*                                                                               
         XC    KEY,KEY             INDICATE 1ST PASS TO READER                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- NEXTBUD - READ 1ST/NEXT BUDGET KEY AND RECORD.                               
*            APPLY YEAR, STATION AND OFFICE FILTERS                             
*                                                                               
*  INPUT:  KEY - X'0' = 1ST PASS.                                               
*  RETURN: AIO - BUDGET RECORDS RETURNED HERE                                   
*  CC:     ^0  - END OF FILE.                                                   
*                                                                               
NEXTBUD  NTR1                                                                   
         LA    R2,KEY                                                           
         USING RBUDKEY,R2                                                       
*                                                                               
         CLI   KEY,0               1ST PASS?                                    
         BNE   NEXTSEQ                                                          
*                                                                               
*- BUILD STARTING KEY.  ALWAYS SEED ID, REP, YEAR.  STATION IF GIVEN.           
*  ALWAYS SEED ID, REP, YEAR.  STATION AND OFFICE IF GIVEN.                     
*                                                                               
         MVI   RBUDKEY,X'13'       REC ID                                       
         MVC   RBUDKREP,AGENCY     REP CODE                                     
         MVC   RBUDKYR,FILTYEAR    2 CHAR ALPHA YEAR                            
*                                                                               
         OC    FILTSTA,FILTSTA                                                  
         BZ    NEXT100                                                          
*                                                                               
         MVC   RBUDKSTA,FILTSTA    STATION FILTER GIVEN, USE IT                 
*                                                                               
         OC    FILTOFF,FILTOFF                                                  
         BZ    NEXT100                                                          
*                                                                               
         MVC   RBUDKTEM,FILTOFF    TEAM/OFFICE GIVEN                            
*                                                                               
NEXT100  GOTO1 HIGH                                                             
         B     NEXT200                                                          
*                                                                               
NEXTSEQ  GOTO1 SEQ                                                              
*                                                                               
NEXT200  EQU   *                                                                
         CLC   KEY(20),KEYSAVE     = THRU YEAR?                                 
         BNE   NEXTEOF             END OF DATA.                                 
*                                                                               
*- DIRECTORY FILTERS                                                            
         OC    FILTSTA,FILTSTA                                                  
         BZ    NEXT220                                                          
*                                                                               
         CLC   RBUDKSTA,FILTSTA    STATION FILTER                               
         BNE   NEXTSEQ                                                          
*                                                                               
NEXT220  OC    FILTOFF,FILTOFF                                                  
         BZ    NEXT240                                                          
*                                                                               
         CLC   RBUDKTEM,FILTOFF    OFFICE/TEAM FILTER                           
         BNE   NEXTSEQ                                                          
*                                                                               
*- READ IN RECORD.                                                              
NEXT240  EQU   *                                                                
         DROP  R2                                                               
         L     RE,AIO              INITIALIZE READ-AREA                         
         LA    RF,1000                                                          
         XCEF                                                                   
         GOTO1 GETREC                                                           
*                                                                               
         L     RF,#READ            BUMP # RECS READ COUNTER                     
         A     RF,=F'1'                                                         
         ST    RF,#READ                                                         
*                                                                               
         SR    R0,R0               EXIT WITH 0 CC                               
         B     NEXTEXIT                                                         
*                                                                               
NEXTEOF  LTR   RD,RD               END OF DATA (^0) CC                          
NEXTEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- CALC -- DO ACTUAL ALLOCATION OR DE-ALLOCATION OF RECORD.                     
*                                                                               
DALLOC$  EQU   RBUD$TOT-RBUDELEM                                                
DBUDTYP  EQU   RBUDTYPE-RBUDELE2                                                
DBUDJAN  EQU   RBUDSJAN-RBUDELEM                                                
DBUDSTA  EQU   RBUDSTA-RBUDELEM                                                 
DSTATOT$ EQU   RBUDSTOT-RBUDELEM                                                
DBUDTAG  EQU   RBUDTAG-RBUDELEM                                                 
*                                                                               
CALC     NTR1                                                                   
         L     R2,AIO                                                           
         USING RBUDREC,R2                                                       
         ZICM  R3,RBUDLEN,2        CALC A(END OF RECORD)                        
         AR    R3,R2               L(REC) + A(REC) = A(EOR)                     
         LA    R2,RBUDELEM         A(BASIC BUDGET ELEMENT)                      
*                                                                               
         DROP  R2                                                               
*                                                                               
         CLI   CTYPCTR,0           CONTRACT TYPES ENTERED?                      
         BNE   CALC0055            YES - SKIP BASIC BUDGET ELEMENT              
         B     CALC0014            NO  - PROCESS BASIC TYPE ONLY                
*                                                                               
*  NOTE:  CALC0010 IS NEVER ENTERED FROM ABOVE.  IF NO CONTRACT                 
*      TYPES HAVE BEEN ENTERED, THE ARRAY OF TYPES IS IGNORED, AND              
*      THE UN-TYPED (BASIC BUDGET) ELEMENT IS PROCESSED.  IF TYPES              
*      HAVE BEEN ENTERED, THE UN-TYPED (BASIC BUDGET) ELEMENT IS                
*      SKIPPED BEFORE THE FIRST CONTRACT TYPE ELEMENT IS CHECKED.               
*                                                                               
CALC0010 EQU   *                                                                
         LA    R4,CTYPARAY         A(CONTRACT TYPES ENTERED)                    
         ZIC   R5,CTYPCTR          #(CONTRACT TYPES ENTERED)                    
CALC0012 EQU   *                                                                
         CLI   0(R2),X'02'         CONTRACT TYPE ELEMENT?                       
         BNE   CALC0050            NO  - SKIP IT                                
         CLC   0(1,R4),DBUDTYP(R2) ELEMENT TYPE ENTERED?                        
         BE    CALC0014            YES - PROCESS IT                             
         LA    R4,1(R4)            BUMP A(CONTRACT TYPES ENTERED)               
         BCT   R5,CALC0012         NO  - TEST NEXT                              
         B     CALC0050            NOT FOUND - SKIP IT                          
CALC0014 EQU   *                                                                
         CLI   FORCMODE,C'Y'       FORCE ACTION REQUESTED?                      
         BE    CALC0020            YES - SKIP EXCEPTION CHECKING                
         BAS   RE,CHECKREC         CHECK FOR EXCEPTION RECORDS                  
         BNZ   CALC0050            ERROR REPORTED IN CHECKREC                   
*                                                                               
CALC0020 EQU   *                                                                
         CLI   PROCMODE,C'A'                                                    
         BNE   CALC0030                                                         
         BAS   RE,CCALLOC          ALLOCATION MODE.                             
         B     CALC0040                                                         
*                                                                               
CALC0030 EQU   *                                                                
         CLI   PROCMODE,C'D'                                                    
         BNE   CALC0099                                                         
         BAS   RE,CCDALLOC         DE-ALLOCATION MODE                           
         B     CALC0040                                                         
*                                                                               
CALC0040 EQU   *                                                                
         BAS   RE,REPORT           REPORT WHAT WE'RE DOING                      
*                                                                               
CALC0050 EQU   *                                                                
         CLI   CTYPCTR,0           CONTRACT TYPES ENTERED?                      
         BE    CALC0060            NO  - ONLY BASIC ELEMENT DONE                
CALC0055 EQU   *                                                                
         ZIC   R4,1(R2)            GET ELEMENT LENGTH                           
         AR    R2,R4               ADD LENGTH TO ADDRESS                        
         CLI   0(R2),X'00'         END OF RECORD?                               
         BE    CALC0060            YES - EXIT                                   
         CR    R2,R3               TEST AGAIN                                   
         BL    CALC0010            EOR NOT REACHED                              
*                                                                               
CALC0060 EQU   *                                                                
         SR    R0,R0                                                            
         B     EXIT                                                             
*                                                                               
CALC0099 EQU   *                                                                
         DC    H'0'                UNDEFINED PROCESSING MODE.                   
         EJECT                                                                  
*                                                                               
*- DE-ALLOCATION MODE.                                                          
*  PUT SUM OF MONTHLY AMOUNTS IN BUDGET TOTAL FIELD                             
*  0 OUT MONTHLY BUCKETS AND TOTALS                                             
*  SET RECORD TAG FOR DEALLOCATED.                                              
*                                                                               
*  R2  = A(BUDGET ELEMENT IN PROCESS)                                           
*                                                                               
CCDALLOC NTR1                                                                   
*                                                                               
         XC    DBUDSTA(48,R2),DBUDSTA(R2)                                       
         XC    DBUDJAN(48,R2),DBUDJAN(R2)                                       
         XC    DSTATOT$(4,R2),DSTATOT$(R2)                                      
         MVI   DBUDTAG(R2),C'D'        DE-ALLOCATED BY SPREADER                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*- ALLOCATION MODE.                                                             
*  CHECK FOR EVEN SPLITS OR MONTHLY PERCENTAGE SPLITS                           
*  BOTH EVEN AND SPECIFIED SPLITS ADD ROUNDING/ADJUSTMENT                       
*       TO FIRST NON-0 MONTHLY BUCKET.  IF ALL BUCKETS ARE 0                    
*       ADJUSTMENT ADDED TO JANUARY.                                            
*                                                                               
*  R2  = A(BUDGET ELEMENT IN PROCESS)                                           
*                                                                               
CCALLOC  NTR1                                                                   
         MVC   BUDAMT,DALLOC$(R2)  DOLLARS TO BE SPREAD TO WK AREA              
*                                                                               
         CLI   EVEN,C'Y'                                                        
         BE    CCAL500             EVEN SPLIT.                                  
*                                                                               
*- SPECIFIC ALLOCATION  (MONTHLY PERCENTAGES)                                   
         LA    R3,DBUDJAN(R2)      PUT RESULT HERE                              
         LA    R4,12               LOOP FOR 12 MONTHS                           
         LA    R5,MONALLOC         PERCENTAGES ARE HERE. 0.00 - 100.00          
*                                                                               
CCAL100  EQU   *                                                                
         CLI   FYSTART,0                                                        
         BNE   *+6                                                              
         DC    H'0'                WHERE'D IT GO?                               
*                                                                               
         GOTO1 ALLOC,DMCB,BUDAMT,(R5),(R3)                                      
         LA    R5,4(R5)            NEXT %                                       
         LA    R3,4(R3)            NEXT MONTH BKT                               
         BCT   R4,CCAL100                                                       
         B     CCAL600             GO TO FINAL ADJUSTMENTS                      
         SPACE                                                                  
*                                                                               
*- EVEN SPLIT (MONTHLY AMT = 1/12 OF TOTAL)                                     
CCAL500  EQU   *                                                                
         SR    RE,RE                                                            
         L     RF,BUDAMT           TOTAL $                                      
         D     RE,=F'12'                                                        
         LA    R1,6                                                             
         CR    RE,R1               NEED TO ROUND UP REMAINDER?                  
         BL    *+8                                                              
         A     RF,=F'1'            ROUND UP                                     
*                                                                               
         ST    RF,FULL                                                          
*                                                                               
*- LOAD MONTHLY AMOUNT INTO MONTHLY BUCKETS.                                    
         LA    RE,DBUDJAN(R2)                                                   
         LA    RF,12                                                            
CCAL520  MVC   0(4,RE),FULL                                                     
         LA    RE,4(RE)                                                         
         BCT   RF,CCAL520                                                       
         SPACE                                                                  
*                                                                               
*- MONEY HAS BEEN SPREAD OVER MONTHS.  MAKE ANY ADJUSTMENTS NEEDED              
*  SO IT SUMS TO ORIGINAL BUDGET AMOUNT.                                        
*                                                                               
*  -- ADJUSTMENT MADE TO 1ST NON-0 MONTH BUCKET       --                        
*  -- IF ALL MONTHS ARE 0, ADJUSTMENT MADE TO JANUARY --                        
*                                                                               
CCAL600  EQU   *                                                                
         CLI   FYSTART,0                                                        
         BNE   *+6                                                              
         DC    H'0'                WHERE'D IT GO?                               
*                                                                               
         BAS   RE,ADDUPBUD         ADD UP MONTHLY $                             
*                                                                               
         CLC   DALLOC$(4,R2),SUMTOT                                             
         BE    CCAL700             NO ADJUSTMENT NEEDED                         
*                                                                               
         MVC   FULL,DALLOC$(R2)    ADJUSTMENT = TOTAL ALLOC                     
         L     R4,FULL                                                          
         S     R4,SUMTOT           MINUS MONTHLY SUM                            
*                                                                               
*- FIND MONTH TO ADJUST (1ST NON-0 OR JAN)                                      
         LA    R3,DBUDJAN(R2)                                                   
         LR    R1,R3               SAVE A(JAN) J.I.C ALL ARE 0                  
         LA    R0,12                                                            
CCAL620  OC    0(4,R3),0(R3)                                                    
         BNZ   CCAL640             FOUND NON-0 MONTH                            
         LA    R3,4(R3)                                                         
         BCT   R0,CCAL620                                                       
         LR    R3,R1               ALL 0, USE JANUARY                           
*                                                                               
CCAL640  MVC   FULL,0(R3)          MONTH $ AMOUNT                               
         A     R4,FULL             + ADJUSTMENT (CAN BE NEGATIVE)               
         ST    R4,FULL                                                          
         MVC   0(4,R3),FULL        PUT BACK INTO RECORD                         
*                                                                               
*- DOUBLECHECK ADJUSTMENT.                                                      
         BAS   RE,ADDUPBUD                                                      
         CLC   SUMTOT,DALLOC$(R2)                                               
         BE    *+6                                                              
         DC    H'0'                ADJUSTMENT NEEDS ADJUSTING!                  
*                                                                               
*- FIT MONTHLY BUCKETS INTO REP FISCAL CALENDAR.                                
*                                                                               
CCAL700  EQU   *                                                                
         MVC   DSTATOT$(4,R2),SUMTOT     SAVE BUDGET TOTAL                      
*                                                                               
         MVC   DBUDSTA(48,R2),DBUDJAN(R2)                                       
*                                  SEED BUCKETS WITH JAN-DEC DATA               
*                                                                               
*- ZERO BUCKETS FROM JAN THRU (FY START MONTH-1)                                
*  (IF FISCAL YEAR STARTS IN JAN, WE'RE DONE)                                   
         CLI   FYSTART,0                                                        
         BNE   *+6                                                              
         DC    H'0'                WHERE'D IT GO?                               
*                                                                               
         ZIC   RF,FYSTART          REP FISCAL YEAR START MONTH (1-12)           
         BCTR  RF,0                LESS 1                                       
         LTR   RF,RF                                                            
         BZ    CCAL750             FY STARTS IN JAN. DONE.                      
*                                                                               
         LA    RE,DBUDSTA(R2)                                                   
CCAL720  MVC   0(4,RE),=F'0'       0 OUT BUCKET                                 
         LA    RE,4(RE)                                                         
         BCT   RF,CCAL720                                                       
*                                                                               
CCAL750  EQU   *                                                                
         MVI   DBUDTAG(R2),C'A'    TAG RECORD AS ALLOCATED.                     
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*- CHECK CONTRACT TYPE FOR EXCEPTIONS.  IF EXCEPTION,                           
*  PRINT IT OUT ON REPORT ALONG WITH REASON.                                    
*                                                                               
*  EXCEPTION DEFINITION DEPENDS ON RUN MODE.                                    
*                                                                               
*  ALLOCATION MODE:                                                             
*  1) CONTRACT TYPE HAS BUDGETS ALREADY ALLOCATED                               
*                                                                               
*  DE-ALLOCATION MODE:                                                          
*  1) ALLOCATED BUDGETS DO NOT SUM TO ALLOCATION TOTAL.                         
*                                                                               
*  R2  = ADDRESS OF ELEMENT IN PROCESS                                          
*                                                                               
CHECKREC NTR1                                                                   
*                                                                               
         CLI   PROCMODE,C'A'                                                    
         BE    CKALLOC             ALLOCATION MODE.                             
*                                                                               
         CLI   PROCMODE,C'D'                                                    
         BE    CKDALLOC            DE-ALLOCATION MODE                           
*                                                                               
         DC    H'0'                UNDEFINED PROCESSING MODE.                   
*                                                                               
*- ALLOCATION MODE                                                              
CKALLOC  EQU   *                                                                
         LA    RE,NOMONEY                                                       
         LA    RF,L'NOMONEY                                                     
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         OC    DALLOC$(4,R2),DALLOC$(R2)   BUDGET $ TO ALLOCATE?                
         BNZ   CKA100              YES                                          
*                                                                               
*- BE A SPORT.....TRY TO GET MONEY FROM JANUARY                                 
         OC    DBUDJAN(4,R2),DBUDJAN(R2)                                        
         BZ    CKERR               NOTHING TO ALLOCATE                          
*                                                                               
         MVC   DALLOC$(4,R2),DBUDJAN(R2)   SEED BUDGET TOTAL                    
         XC    DBUDJAN(4,R2),DBUDJAN(R2)   AND 0 JANUARY                        
*                                                                               
CKA100   EQU   *                                                                
         LA    RE,DIDALLOC                                                      
         LA    RF,L'DIDALLOC                                                    
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         LA    RE,DBUDJAN(R2)     JAN BUDGET $                                  
         LA    R0,12               12 MONTHS TO CHECK                           
*                                                                               
CKA200   CLC   =XL4'00',0(RE)      BUDGET AMOUNT 0?                             
         BNE   CKERR                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,CKA200                                                        
         B     CKGOOD              RECORD IS OK.                                
         SPACE                                                                  
*                                                                               
*- DE-ALLOCATION MODE                                                           
*  IF RECORD ALLOCATION AMOUNT IS 0, ACCEPT ALLOCATION ANYWAY                   
CKDALLOC EQU   *                                                                
         OC    DALLOC$(4,R2),DALLOC$(R2)                                        
         BZ    CKGOOD                                                           
*                                                                               
         LA    RE,BADSUM                                                        
         LA    RF,L'BADSUM                                                      
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         BAS   RE,ADDUPBUD         FIND BUDGET TOTAL (SUM OF MONTHS)            
         CLC   SUMTOT,DALLOC$(R2)  SUM TOTAL = ALLOCATION TOTAL?                
         BNE   CKERR                                                            
*                                                                               
CKGOOD   SR    R0,R0                                                            
         B     CKEXIT                                                           
CKERR    EQU   *                                                                
         BAS   RE,DISPBUD          FORMAT REC ON PRINT LINE                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     RF,#ERR             COUNT # OF ERRORS FOUND                      
         A     RF,=F'1'                                                         
         ST    RF,#ERR                                                          
*                                                                               
         LTR   RD,RD               ^0 CC                                        
CKEXIT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- REPORT -- DISPLAY BUDGET RECORD AND MONTHLY BUCKETS.                         
*                                                                               
REPORT   NTR1                                                                   
         LA    RE,SPACES           NO MESSAGE TO PRINT                          
         LA    RF,1                                                             
         STM   RE,RF,AERRMSG                                                    
*                                                                               
         MVI   ALLOWLIN,4          NEED 4 LINES ON PAGE                         
*                                                                               
         BAS   RE,DISPBUD          FORMAT BUDGET REC                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,DISPMON          PRINT MONTHLY AMOUNTS                        
*                                                                               
         MVI   ALLOWLIN,0          RESET FOR NEXT PRINTING                      
*                                                                               
         SR    R0,R0                                                            
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*- MARKREC -- WRITE BACK RECORD UNLESS THIS IS TEST RUN ONLY                    
*                                                                               
MARKREC  NTR1                                                                   
         L     RF,#UPDATED         COUNT UPDATES                                
         A     RF,=F'1'                                                         
         ST    RF,#UPDATED                                                      
*                                                                               
         CLI   SOFTRUN,C'Y'        VERIFY ONLY (TEST)                           
         BE    EXIT                                                             
*                                                                               
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*- CLEANUP                                                                      
CLEANUP  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     BLANK LINE                                   
*                                                                               
         MVC   P(20),L#READ        NUMBER RECORD READ                           
         EDIT  #READ,(6,P+22)                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(20),L#UPDATE      NUMBER UPDATED                               
         EDIT  #UPDATED,(6,P+22)                                                
         CLI   SOFTRUN,C'Y'                                                     
         BNE   CLEAN20                                                          
         MVC   P+35(33),=C'** TEST ONLY -- FILE UNCHANGED **'                   
CLEAN20  GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(20),L#ERR         NUMBER EXCEPTIONS                            
         EDIT  #ERR,(6,P+22)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(13),=C'END OF REPORT'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
*                                                                 *             
*-  MISC. SUBROUTINES                                             *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
*                                                                               
*- ADDUPBUD -- FIND BUDGET TOTAL BY SUMMING 12 MONTHLY AMOUNTS                  
*                                                                               
*  INPUT: AIO = A(BUDGET REC)                                                   
*  RETURN SUMTOT = TOTAL AMOUNT. (BINARY DOLLARS, NO PENNIES)                   
*                                                                               
*  R2  =  A(ELEMENT IN PROCESS)                                                 
*                                                                               
ADDUPBUD NTR1                                                                   
*                                                                               
         LA    RE,DBUDJAN(R2)      JANUARY BUCKET.                              
         LA    R0,12               ADD UP 12 MONTHS                             
         SR    RF,RF               RUNNING TOTAL ACCUM                          
*                                                                               
ADDUP20  MVC   SUMTOT,0(RE)        PUT ON WORD BOUNDARY                         
         A     RF,SUMTOT                                                        
         LA    RE,4(RE)            NEXT BUDGET AMOUNT                           
         BCT   R0,ADDUP20                                                       
*                                                                               
         ST    RF,SUMTOT           PASS BACK BUD TOTAL                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*- DISPBUD -- FORMAT BUDGET RECORD FOR PRINTING                                 
*             (DOES NOT PRINT!)                                                 
*                                                                               
*  R2  = A(ELEMENT IN PROCESS)                                                  
*                                                                               
DISPBUD  NTR1                                                                   
         L     R4,AIO                                                           
         USING RBUDREC,R4                                                       
*                                                                               
         LA    R3,P                PRINT LINE DSECT                             
         USING LINED,R3                                                         
*                                                                               
         MVC   LSTA,RBUDKSTA       STATION                                      
         CLI   RBUDKSTA+4,C' '                                                  
         BE    DISBUD20            TV STATION. SKIP THE '-BAND'                 
*                                                                               
         MVI   LDASH,C'-'          BAND FOR RADIO                               
         MVC   LBAND,RBUDKSTA+4                                                 
*                                                                               
DISBUD20 MVC   LTEM,RBUDKTEM       TEAM/OFFICE                                  
         MVC   LTYP,DBUDTYP(R2)    BUDGET CONTRACT TYPE                         
*                                                                               
         PRINT GEN                                                              
         L     R1,DALLOC$(R2)                      A(ALLOCATION $)              
         EDIT  (R1),(15,L$TOT),COMMAS=YES          ALLOC $                      
         L     R1,DSTATOT$(R2)                     A(STATION TOTAL $)           
         EDIT  (R1),(15,LSTOT),COMMAS=YES          TOTAL $                      
         PRINT NOGEN                                                            
*                                                                               
         LM    RE,RF,AERRMSG                                                    
         BCTR  RF,0                                                             
         EX    RF,DISPERR          DISPLAY ERROR MSG                            
*                                                                               
         B     EXIT                                                             
         SPACE                                                                  
DISPERR  MVC   LMSG(0),0(RE)                                                    
         DROP  R3,R4                                                            
         SPACE 2                                                                
*                                                                               
*- DISPMON -- PRINT MONTHLY BUDGET AMOUNTS (PRINTS ON 2 LINES)                  
*                                                                               
*  R2  =  A(ELEMENT IN PROCESS)                                                 
DISPMON  NTR1                                                                   
         LA    R2,DBUDJAN(R2)                                                   
*                                                                               
         USING LINE2D,R3           R3 COVERS PRINT ENTRY                        
*                                                                               
         LA    R4,MONTHLIT                                                      
         LA    R6,2                2 PRINT LINES                                
*                                                                               
DISMON10 EQU   *                                                                
         LA    R3,P                POINT TO START OF PRINT LINE                 
         LA    R5,6                6 MONTHS/LINE                                
DISMON20 EQU   *                                                                
         MVC   L2MON,0(R4)         MONTH LITERAL                                
         EDIT  (4,0(R2)),(10,L2MON$),COMMAS=YES                                 
*                                                                               
         LA    R4,3(R4)            NEXT MONTH LITERAL                           
         LA    R2,4(R2)            NEXT BUDGET $ BUCKET                         
         LA    R3,L2LNTRY(R3)      NEXT A(OUTPUT LINE)                          
         BCT   R5,DISMON20                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R6,DISMON10                                                      
         SR    R0,R0                                                            
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
*- ALLOC -- APPLY ALLOCATION PERCENTAGE TO A DOLLAR FIGURE                      
*                                                                               
*  INPUT:  P1 = A(FULL WORD DOLLAR FIELD)                                       
*          P2 = A(FULL WORD PERCENTAGE, 2 DECIMALS IMPLIED)                     
*          P3 = A(FULL WORD OUTPUT)                                             
*                                                                               
ALLOC    NTR1                                                                   
         L     R3,8(R1)            A(OUTPUT)                                    
         L     R2,4(R1)            A(PERCENTAGE)                                
         L     R1,0(R1)            A(DOLLAR FIELD)                              
*                                                                               
         SR    RE,RE               RE/RF = DOLLAR AMOUNT                        
         L     RF,0(R1)                                                         
*                                                                               
         M     RE,0(R2)            DOLLARS X PERCENTAGE                         
         D     RE,=F'10000'        /100.00                                      
*                                                                               
         L     R2,=F'5000'         NEED TO ROUND?                               
         CR    RE,R2                                                            
         BL    ALLOC20             NO                                           
         A     RF,=F'1'            ROUND UP                                     
ALLOC20  EQU   *                                                                
         STCM  RF,15,0(R3) PASS BACK RESULT                                     
         XIT1                                                                   
         SPACE 2                                                                
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,40,C'AUTOMATED BUDGET ALLOCATION'                             
         SSPEC H2,40,C'---------------------------'                             
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R1,H4                                                            
         USING H4D,R1                                                           
         MVC   H4YRLIT,=C'YEAR'                                                 
         MVC   H4YRDATA,HEADYEAR                                                
*                                                                               
         LA    R1,H5                                                            
         USING H5D,R1                                                           
         MVC   H5SPLIT,EVENLIT                                                  
         CLI   EVEN,C'Y'                                                        
         BE    HOOK10                                                           
         MVC   H5SPLIT,ALLOCLIT                                                 
*                                                                               
HOOK10   EQU   *                                                                
         DROP  R1                                                               
         MVC   H8(LHEADING),HEADING                                             
         MVC   H9(LHEADING),UNDRLINE                                            
         B     EXIT                                                             
         SPACE 2                                                                
ERREXIT  MVC   CONHEAD(9),=C'* ERROR *'                                         
         LM    RE,RF,AERRMSG       RE=A(MGR), RF=MSG LEN                        
         BCTR  RF,0                                                             
         EX    RF,MSGMOVE          MESSAGE TO SCREEN                            
*                                                                               
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
MSGMOVE  MVC   CONHEAD+10(0),0(RE)                                              
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- ERROR MESSAGES FOR RECORD VALIDATION                                         
*                                                                               
INVYEAR  DC    C'YEAR MUST BE 4 NUMERIC CHARACTERS'                             
INVSTATN DC    C'INVALID STATION'                                               
INVOFF   DC    C'INVALID OFFICE'                                                
NEEDFILT DC    C'MUST SPECIFY STATION OR OFFICE FILTER'                         
INVMODE  DC    C'MUST SPECIFY ''A'' (ALLOCATE) OR ''D'' (DE-ALLOCATE)'          
YORN     DC    C'INPUT MAY BE ''Y'' OR ''N'' ONLY'                              
PICK1    DC    C'MUST CHOOSE EITHER EVEN SPLIT OR ALLOCATED SPLIT'              
NOT100   DC    C'ALLOCATIONS MUST SUM TO 100.00 PERCENT'                        
INVALLOC DC    C'ALLOCATION PERCENTAGE MAY BE 0.00 TO 100.00'                   
CTYPNG   DC    C'CONTRACT TYPE(S) ARE REQUIRED'                                 
CONTYPNF DC    C'CONTRACT TYPE ENTERED IS NOT ON FILE'                          
FALLOC   DC    C'FORCE ALLOCATION REQUIRES Y OR N ONLY'                         
INVNUM   DC    C'VALUE MUST BE NUMERIC WITH NO DECIMALS'                        
NOTWIDOL DC    C'THIS OPTION MAY NOT BE USED WITH DOLLARS'                      
*                                                                               
         SPACE 2                                                                
*                                                                               
*- ERROR MESSAGES USED DURING PROCESS                                           
*                                                                               
NOMONEY  DC    C'NO DOLLARS IN BUDGET RECORD'                                   
DIDALLOC DC    C'MONTHLY BUDGET ALREADY ALLOCATED'                              
BADSUM   DC    C'MONTHLY BUDGETS DO NOT SUM TO ALLOCATION TOTAL'                
L#READ   DC    CL20'# OF RECORDS READ   '                                       
L#UPDATE DC    CL20'# OF RECORDS UPDATED'                                       
L#ERR    DC    CL20'# OF EXCEPTIONS     '                                       
*                                                                               
MONTHLIT DC    C'JAN',C'FEB',C'MAR',C'APR',C'MAY',C'JUN'                        
         DC    C'JUL',C'AUG',C'SEP',C'OCT',C'NOV',C'DEC'                        
         SPACE                                                                  
HEADING  DS    0C                  REPORT HEADING LINES                         
         DC    CL2' '                                                           
         DC    CL7'STATION'                                                     
         DC    CL1' '                                                           
         DC    CL3'OFF'                                                         
         DC    CL3' '                                                           
         DC    CL3'TYP'                                                         
         DC    CL3' '                                                           
         DC    CL15'   ALLOCATION $'                                            
         DC    CL4' '                                                           
         DC    CL15'MONTHLY TOTAL $'                                            
         DC    CL4' '                                                           
         DC    CL50'STATUS MESSAGE'                                             
LHEADING EQU   *-HEADING                                                        
         SPACE 2                                                                
UNDRLINE DS    0C                                                               
         DC    CL2' '                                                           
         DC    CL7'-------'                                                     
         DC    CL1' '                                                           
         DC    CL3'---'                                                         
         DC    CL3' '                                                           
         DC    CL3'---'                                                         
         DC    CL3' '                                                           
         DC    CL15'   ------------'                                            
         DC    CL4' '                                                           
         DC    CL15'---------------'                                            
         DC    CL4' '                                                           
         DC    CL50'--------------'                                             
         SPACE 2                                                                
EVENLIT  DC    CL16'EVEN SPLIT'                                                 
ALLOCLIT DC    CL16'ALLOCATED SPLIT'                                            
         SPACE 2                                                                
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMBFD                                                                      
* REGENBUD                                                                      
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
*                                                                               
*   TWA STORED DATA                                                             
*                                                                               
         ORG   CONHEADH+3508       USE LAST 12 BYTES OF SCREEN AREA             
SVPGENTY DS    0CL12                                                            
SVPGREP  DS    CL2                 REP/AGENCY/USER POWER CODE                   
SVPGP#   DS    CL1                 SFM PROGRAM NUMBER (18)                      
         DS    CL1                                                              
SVPGPBIT DS    CL8                 64 PROFILES                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMBFD                                                       
         EJECT                                                                  
       ++INCLUDE REGENBUD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
*                                                                               
AERRMSG  DS    A                   A(ERROR MESSAGE TEXT)                        
LERRMGR  DS    F                   LENGTH OF ERROR MESSAGE TEXT                 
*                                                                               
MONALLOC DS    12F                 MONTHLY ALLOCATIONS (JAN-DEC)                
TOTALLOC DS    F                   TOTAL ALLOCATION. BINARY, 1 DECIMAL          
*                                                                               
FILTERS  DS    0C                **RUN FILTERS                                  
*                                                                               
FILTYEAR DS    CL2                 YEAR                                         
FILTOFF  DS    CL2                 OFFICE CODE                                  
FILTSTA  DS    CL5                 STATION CODE                                 
*                                                                               
LFILTERS EQU   *-FILTERS         **LENGTH OF FILTERS                            
*                                                                               
PROCMODE DS    CL1                 A=ALLOCATE, D=DE-ALLOCATE                    
FORCMODE DS    CL1                 Y=FORCE, N=DON'T FORCE                       
SOFTRUN  DS    CL1                 Y=VERIFY ONLY, N=MARK THE FILE.              
USEDOLL  DS    CL1                 Y=USE DOLLARS,N=USE PERCENTAGES              
EVEN     DS    CL1                 Y = EVEN SPLIT                               
ANYALLOC DS    CL1                 Y = ALLOCATION DATA ENTERED                  
NUMDEC   DS    CL1                 NUMBER OF DECIMAL PLACES FOR NUMVAL          
CTYPCTR  DS    XL1                 CONTRACT TYPE COUNT ON INPUT                 
CTYPARAY DC    CL5'     '          CONTRACT TYPES INPUT                         
CTYPWORK DS    CL160               SCANNER WORK AREA                            
*                                                                               
HEADYEAR DS    CL4                 YEAR TO PRINT IN HEADLINE                    
         SPACE                                                                  
*                                                                               
*- WORK AREA USED BY SPREADER ROUTINE (ACTUAL PROCESSING)                       
*                                                                               
RPTBKTS  DS    0C                  ACCUMS, BKTS, COUNTERS, ETC                  
*                                                                               
#READ    DS    F                   NUMBER OF BUDGET RECS READ                   
#ERR     DS    F                   NUMBER OF EXCEPTION RECORDS                  
#UPDATED DS    F                   NUMBER OF RECORDS UPDATED                    
*                                                                               
SUMTOT   DS    F                   SUMMATION OF MONTHLY BUD AMOUNTS             
BUDAMT   DS    F                   BUDGET CALCULATION WK FLD                    
PACKMON  DS    PL10                PACKED STORAGE FOR MONTH VALUE               
PACKTOT  DS    PL6                 PACKED STORAGE FOR TOTAL SUM                 
*                                                                               
FYSTART  DS    XL1                 REP FISCAL YEAR START MONTH (1-12)           
         SPACE                                                                  
LRPTBKTS EQU   *-RPTBKTS     <<--- INSERT NEW FLDS ABOVE THIS LINE              
         SPACE 2                                                                
*                                                                               
*- BUDGET RECORD PRINT LINE DSECT                                               
LINED    DSECT                                                                  
         DS    CL2                                                              
LSTA     DS    CL4                                                              
LDASH    DS    CL1                                                              
LBAND    DS    CL1                                                              
         DS    CL2                                                              
LTEM     DS    CL2                                                              
         DS    CL5                                                              
LTYP     DS    CL1                                                              
         DS    CL4                                                              
L$TOT    DS    CL15                                                             
         DS    CL4                                                              
LSTOT    DS    CL15                                                             
         DS    CL4                                                              
LMSG     DS    CL50                                                             
         SPACE                                                                  
*                                                                               
*- MONTHLY BUCKET DISPLAY LINE DSECT                                            
LINE2D   DSECT                                                                  
         DS    CL4                                                              
L2MON    DS    CL3                 MONTH LITERAL                                
         DS    CL2                                                              
L2MON$   DS    CL12                MONTHLY $                                    
L2LNTRY  EQU   *-LINE2D            LENGTH OF 1 MONTH DISPLAY ENTRY              
         SPACE 2                                                                
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA         REP RECORD DSECT                             
         SPACE 2                                                                
H4D      DSECT                     4TH HEADING LINE                             
         DS    CL92                                                             
H4YRLIT  DS    CL4                                                              
         DS    CL1                                                              
H4YRDATA DS    CL4                                                              
         SPACE 2                                                                
H5D      DSECT                     5TH HEADING LINE                             
         DS    CL92                                                             
H5SPLIT  DS    CL16                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126RESFM20   05/01/02'                                      
         END                                                                    
