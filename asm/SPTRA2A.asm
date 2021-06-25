*          DATA SET SPTRA2A    AT LEVEL 105 AS OF 06/29/20                      
*PHASE T2162AC                                                                  
************************* NOTE                                                  
*                                  CK "BUY TOO BIG SPLIT?"                      
         TITLE 'T2162A - SPOT SEED COMMERCIALS'                                 
***********************************************************************         
*                                                                     *         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)  - GETPTN RTN PATTERN TABLE    *         
*                    GETPTN RTN READ PATTERNS                         *         
*             AIO2 - TABLE OF PATTERNS                                *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - SPTABLE POINTER                                         *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - PATTERN TABLE POINTER & WORK REG                        *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE REG                                         *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
                                                                                
***********************************************************************         
*                                                                     *         
* LEV 19 SMUR JUL24/03 SAVE LTD IN SPOT TBL FOR PARTIAL PAT ASSIGNMENT*         
* LEV 20 BGRI JUL28/04 SOX & ALLOW ALL SPOT LENGTHS IF NONE ENTERED   *         
* LEV 21 BGRI NOV17/04 MAKE COMML & PATTN TABLES LARGER 5000 TO 10000 *         
* LEV 22 BGRI DEC06/04 ADD ERR MSG FOR PATTN WITH NO COMMLS           *         
* LEV 23 BGRI MAR09/05 FIX BUG IN SEEDING PATTERNS                    *         
* LEV 25 SMUR DEC22/05 FIX P/B CMLS                                   *         
* LEV 26 SMUR JUN28/06 BACK OUT LEV 23, FIX SEEDING BY FLIGHT NOT EST *         
* LEV 27 SMUR JAN19/07 MAKE COMML TABLES LARGER 10000 TO 15000        *         
* LEV 28 SMUR JAN25/07 MAKE COMML TABLES LARGER 15000 TO 40000        *         
* LEV 29 MNAS FEB06/07 CHANGES FOR 6K SPOT BUYS                       *         
* LEV 32 SMUR DEC14/07 BYPASS TRAFFIC=NO BUYS                         *         
* LEV 34 SMUR MAY30/08 GIVE ERROR FOR PRD 'ALL-' REQUEST              *         
* LEV 35 SMUR JUN11/08 BYPASS THEATRICAL PRODUCTS AND BPAT RECORDS    *         
*        MHER OCT/08   PATTERNS BY DAYPART AND CLEAN UP               *         
* LEV 98 MNAS APR16/10 POINT TO SPT NOT STR WHEN READING MKT GRP RECS *         
* LEV 99 MNAS JUN18/10 CLEAR CML ASSIGNMENT FROM SPOT WHEN COMML REC  *         
*                      NOT FOUND (HAS BEEN PURGED) INSTEAD OF DUMPING *         
* LEV100 MNAS JAN22/13 MORE BANDS                                     *         
* LEV101 SMUR JAN06/16 NEW BAND CM FOR IHEART RADIO                   *         
* LEV102 SMUR OCT17/17 FIX DPT "TOO MANY DPTS" BUG                    *         
* SPEC-31359  SMUR JAN16/19 MAKE STATION TABLE LARGER FOR 499 BUYLINES*         
* SPEC-33227  SMUR FEB28/19 BUG FIX WHEN BUILDING DAYPART LIST        *         
* SPEC-46588  SMUR JUN29/20 BYPASS SPILL BUYS                         *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T2162A   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T2162A**,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         ST    R3,SPTRRR                                                        
*                                                                               
         BRAS  RE,GETSTOR          ALLOCATE ONLINE/OFFLINE STORAGE              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE SEED                                 
         BE    LRR                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VK       DS    0H                                                               
         MVI   SEEDTYPE,0          SET                                          
         LA    R1,0                   UP                                        
         AHI   R1,-4                    FOR                                     
         STH   R1,PRDNUM                   PROD=ALL                             
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALICLT                                                          
*                                                                               
         BAS   RE,RDPROF           GO GET PROFILE RECORD(S)                     
*                                                                               
         LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         CLI   5(R2),3                                                          
         BL    VK10                                                             
         CLC   =C'ALL',8(R2)       ALL PRODS?                                   
         BNE   VK10                                                             
*                                                                               
         TM    WHEN,X'10'          TEST OVERNIGHT                               
         BZ    OVERR                NO,ERROR                                    
*                                                                               
         CLI   11(R2),C'-'         ALL FOLLOWED BY LEN (ALL-30)                 
         BNE   VK10                                                             
*                                                                               
         CLI   12(R2),0            LENGTH IS ZERO                               
         BE    VK10                                                             
         CLI   12(R2),X'40'        LENGTH IS SPACE                              
         BE    VK10                                                             
*                                                                               
         LLC   R1,5(R2)            TOTAL INPUT LENGTH                           
         AHI   R1,-4               MINUS 4 FOR (ALL-)                           
         STC   R1,FLDH+5           SAVE LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),12(R2)       MOVE LENGTH                                  
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         GIVE ME CONTROL FOR ERROR HANDLING           
         GOTO1 VALISLN             VALIDATE LENGTH                              
         CLI   ERROR,0             ANY ERRORS?                                  
         BNE   INVSLNER             YES, INVALID LENGTH                         
         MVI   ERROPT,C'N'                                                      
*                                                                               
         MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
         OI    TRAPRDH+6,X'80'    FORCE TRANSMIT                                
*                                                                               
         OI    SEEDTYPE,PRDALL     PRD=ALL                                      
*                                                                               
VK08     BAS   RE,GETPRD           GET NEXT PROD FROM SVCLIST                   
         TM    SEEDTYPE,DONEALL    DONE?                                        
         BO    VK60                 YES, ALL DONE                               
*                                                                               
         BAS   RE,CTHTR            CHECK IF PROD IS THEATRICAL                  
         BZ    VK20                                                             
         BO    VK08                BYPASS THEATRICAL PROD                       
*                                                                               
VK10     NI    SECFLAG,X'FF'-PRDTHTR INIT THEATRICAL PRODUCT                    
         GOTO1 VALIPRD                                                          
         TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BO    THTRERR              YES, ERROR                                  
*                                                                               
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
*                                                                               
VK16     MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
*                                                                               
VK20     LA    R2,TRAPTRH          PARTNER                                      
         MVC   QPRD2,SPACES                                                     
         MVI   BPRD2,0                                                          
         MVI   BSLN2,0                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK28                                                             
         GOTO1 ANY                                                              
         CLC   =C'NONE',WORK       DON'T ALLOW ANY PIGGYBACK PRODS              
         BE    VK26                                                             
*                                                                               
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK24                YES                                          
*                                                                               
VK24     MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   BSLN2,WORK+4                                                     
         B     VK28                                                             
VK26     MVI   BPRD2,255                                                        
*                                                                               
* EDIT MARKET *                                                                 
*                                                                               
VK28     LA    R2,TRAMKTH          MARKET                                       
         XC    BMKTSTA,BMKTSTA                                                  
         XC    QMKT,QMKT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                                                             
*                                                                               
         GOTO1 VALIMKT                                                          
*                                                                               
VK30     OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* EDIT ESTIMATE *                                                               
*                                                                               
         LA    R2,TRAESTH          EST NUMBER                                   
*                                                                               
         XC    SVESTAB,SVESTAB                                                  
         MVI   BEST,0                                                           
         MVI   SVBEST,0            SAVE BEST                                    
         CLI   5(R2),0                                                          
         BNE   VK34                                                             
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BNE   VK40                                                             
         B     MISESTER                                                         
*                                                                               
VK34     GOTO1 ANY                                                              
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    VK34C                NEEDS EST                                   
         CLC   =C'NO ',WORK                                                     
         BE    VK40                                                             
         CLC   =C'ALL',WORK                                                     
         BE    BDESTPR             PROFILE NOT SET TO EST                       
*                                                                               
VK34C    CLC   =C'ALL',WORK                                                     
         BNE   VK34X                                                            
*                                                                               
         TM    WHEN,X'10'          TEST OVERNIGHT                               
         BZ    OVERR                NO,ERROR                                    
*                                                                               
         OI    SEEDTYPE,ESTALL     EST=ALL                                      
*                                                                               
         LA    R2,TRAPERH                                                       
         CLC   =C'ES',8(R2)                                                     
         BE    PERERR              MUST ENTER PERIOD                            
*                                                                               
         BRAS  RE,VPER             GET PERIOD START AND END DATES               
*                                                                               
         BRAS  RE,BLDEST           BUILD ESTIMATE TABLE                         
*                                                                               
         L     R1,AESTTABL         START OF ESTIMATE TABLE                      
         ST    R1,SVESTNXT         SAVE ITS ADDRESS                             
         B     VK34E                                                            
*                                                                               
* GET NEXT ESTIMATES                                                            
*                                                                               
VK34D    NTR1                                                                   
         TM    SEEDTYPE,ESTALL     EST=ALL                                      
         BZ    VK34F                NO                                          
*                                                                               
VK34E    BAS   RE,GETEST           GET ESTIMATE                                 
         BE    VK40                                                             
*                                                                               
         TM    SEEDTYPE,PRDALL     IF PRD=ALL                                   
         BO    VK34F                GET NEXT PRD                                
         TM    SEEDTYPE,DONEALL    DONE, NO MORE EST                            
         BO    VK60                YES, ALL DONE                                
*                                                                               
* GET NEXT PRODUCT                                                              
*                                                                               
VK34F    NI    SEEDTYPE,X'FF'-DONEALL  CLEAR ALL DONE FLAG                      
*                                                                               
         BAS   RE,GETPRD           GET NEXT PRODUCT                             
*                                                                               
         TM    SEEDTYPE,DONEALL    DONE, NO MORE PRDS                           
         BO    VK60                YES, ALL DONE                                
*                                                                               
         BAS   RE,CTHTR            CHECK IF PROD IS THEATRICAL                  
         BO    VK34F               BYPASS THEATRICAL PROD                       
*                                                                               
         TM    SEEDTYPE,ESTALL     EST=ALL                                      
         BZ    VK35                 NO                                          
         L     R1,AESTTABL         START OF ESTIMATE TABLE                      
         ST    R1,SVESTNXT                                                      
         B     VK34E               GO GET ALL EST FOR THIS PROD                 
*                                                                               
VK34X    GOTO1 VALINUM                                                          
*                                                                               
         MVC   BEST,ACTUAL         SET AS EST                                   
         MVC   SVBEST,BEST                                                      
*                                                                               
VK35     GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
*                                                                               
         TM    SEEDTYPE,PRDALL     PRD=ALL?                                     
         BZ    *+10                                                             
VK35B    MVC   SVKEY(8),KEY                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BE    VK35C                                                            
         TM    SEEDTYPE,PRDALL     PRD=ALL?                                     
         BZ    BDESTPR             NO, ERROR                                    
*                                                                               
VK35B1   BAS   RE,GETPRD           GET NEXT PRODUCT                             
         TM    SEEDTYPE,DONEALL    ARE WE DONE?                                 
         BZ    *+16                 NO                                          
         TM    SEEDTYPE,FOUNDONE   AT LEAST ONE FOUND ?                         
         BZ    BDESTPR              NO, ERROR                                   
         B     VK60                                                             
*                                                                               
         BAS   RE,CTHTR            CHECK IF PROD IS THEATRICAL                  
         BO    VK35B1              BYPASS THEATRICAL PROD                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),QPRD                                                    
*                                                                               
         B     VK35B                                                            
*                                                                               
VK35C    DS    0H                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   ESTCPYER                                                         
*                                                                               
VK38     GOTO1 DATCON,DMCB,(0,ESTART),(3,SVGENST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVGENEND)                                  
         DROP  R6                                                               
*                                                                               
* EDIT PERIOD DATES *                                                           
*                                                                               
VK40     DS    0H                                                               
         LA    R2,TRAPERH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
*                                                                               
         TM    SEEDTYPE,ESTALL     EST=ALL                                      
         BO    VK47                YES, GO VALIDATE OPTIONS                     
*                                                                               
         BRAS  RE,VPER             VALIDATE PERIOD/FLIGHT                       
*                                                                               
* EDIT OPTIONS - IF ANY *                                                       
*                                                                               
VK47     BRAS  RE,VOPT             VALIDATE OPTIONS                             
*                                                                               
         BRAS  RE,VREF             EDIT PATTERN REFS                            
*                                                                               
         TM    WHEN,X'20'          TEST SOON REQUEST                            
         BZ    VK50                 NO                                          
*                                                                               
         TM    OPTFLAG,OPTTEST     THIS A TEST RUN                              
         BO    VK50                 YES                                         
         MVC   GERROR,=Y(BDSOON)    NO - NOT SOONABLE                           
         LA    R2,CONWHENH                                                      
         BAS   RE,RELSTR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
VK50     TM    WHEN,X'40'          TEST NOW REQUEST                             
         BZ    VK54                 NO                                          
*                                                                               
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   NOTNOWER                                                         
         EJECT                                                                  
* READ ESTIMATE HEADERS AND BUILD LIST OF ELIGIBLE                              
*                                                                               
VK54     BRAS  RE,BLEST                                                         
         BE    VK60                                                             
         CLI   SVTBPR04,C'Y'       NO ESTIMATES ALLOWED?                        
         BNE   NOESTER                                                          
         MVI   SVESTAB,X'FF'       ALLOW ALL ESTIMATES                          
         MVC   SVESTAB+1(L'SVESTAB-1),SVESTAB                                   
*                                                                               
VK60     DS    0H                                                               
         CLI   SVBEST,0            SEED BY FLIGHT NOT EST                       
         BNE   EXIT                                                             
         MVI   BEST,0              YES, CLEAR EST                               
         B     EXIT                                                             
         EJECT                                                                  
*=======================================================                        
* FILL IN QPRD AND BPRD (FOR PRD=ALL REQUEST)                                   
* (R1 = DISPLACEMENT INTO PROD TABLE)                                           
*=======================================================                        
                                                                                
GETPRD   L     RF,ASVCLIST                                                      
         LH    R1,PRDNUM                                                        
         LA    R1,4(R1)            DSPL TO NEXT PRD                             
         STH   R1,PRDNUM                                                        
         AR    RF,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    GETPR20                                                          
GETPR10  MVI   BPRD,0                                                           
         OI    SEEDTYPE,DONEALL     DONE ALL PROD                               
         BR    RE                                                               
*                                                                               
GETPR20  DS    0H                                                               
         CLI   3(RF),X'FF'         POL?                                         
         BE    GETPR10             YES, DONE                                    
         TM    SEEDTYPE,PRDALL     PRD=ALL?                                     
         BZ    GETPR22             NO                                           
         CLC   =C'AAA',0(RF)       SKIP AAA                                     
         BE    GETPRD                                                           
*                                                                               
GETPR22  MVC   QPRD,0(RF)                                                       
         MVC   BPRD,3(RF)                                                       
         BR    RE                                                               
         SPACE 2                                                                
*===========================================================                    
* GET NEXT ESTIMATE FROM ESTIMATE TABLE                                         
*===========================================================                    
                                                                                
GETEST   L     RF,SVESTNXT                                                      
         USING ESTTBLED,RF                                                      
*                                                                               
GETEST10 C     RF,AESTTABX         END OF TABLE?                                
         BL    *+6                                                              
         DC    H'0'                MAKE ESTIMATE TABLE BIGGER                   
         CLI   0(RF),C' '                                                       
         BH    GETEST20                                                         
         OI    SEEDTYPE,DONEALL    DONE ALL ESTIMATES                           
         BR    RE                                                               
*                                                                               
GETEST20 DS    0H                                                               
         MVC   BEST,ESTNUM         ESTIMATE NUMBER                              
         MVC   SVGENDTS,ESTSTART   START AND END DATES                          
*                                                                               
         CLC   ESTPROD,QPRD        IS THIS IT                                   
         BL    GETEST30                                                         
         BE    *+10                                                             
         OI    SEEDTYPE,DONEALL    DONE ALL ESTIMATES                           
         BR    RE                                                               
*                                                                               
         LA    RF,ESTNEXT                                                       
         ST    RF,SVESTNXT         PRD/EST FOUND, SET ADDR FOR NEXT             
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GETEST30 LA    RF,ESTNEXT                                                       
         ST    RF,SVESTNXT                                                      
         B     GETEST10                                                         
         DROP  RF                                                               
         EJECT                                                                  
*=============================================================                  
* READ BUYS AND BUILD TABLE OF SPOTS                                            
*=============================================================                  
                                                                                
LRR      XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(4),BAGYMD     A-M/CLT/PRD                                  
         MVC   SVKEY+4(2),BMKT                                                  
*                                                                               
         ZAP   TBUYLNCT,=P'0'                                                   
         ZAP   TBUYUPCT,=P'0'                                                   
         ZAP   TSPTCT,=P'0'                                                     
         ZAP   TSPTASCT,=P'0'                                                   
         ZAP   TSPTUPCT,=P'0'                                                   
         ZAP   BUYLNCT,=P'0'                                                    
         ZAP   BUYUPCT,=P'0'                                                    
         ZAP   SPTCT,=P'0'                                                      
         ZAP   SPTASCT,=P'0'                                                    
         ZAP   SPTUPCT,=P'0'                                                    
*                                                                               
         MVI   LOG,0                                                            
         XC    SVBUYLIN,SVBUYLIN                                                
*                                                                               
         LAY   RE,HEADINGS                                                      
         ST    RE,SPECS                                                         
         LA    RF,HDHK                                                          
         ST    RF,HEADHOOK                                                      
*                                                                               
         L     R0,ACMLTAB          CLEAR COMML TABLE AREA                       
         L     R1,ACMLTABX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         OC    OPTMKTST,OPTMKTST   WAS MARKET ENTERED                           
         BZ    LRR00                                                            
         MVC   SVKEY+4(5),OPTMKTST                                              
*                                                                               
LRR00    XC    SPOTCT,SPOTCT       ZERO SPOT/UNCML CTS                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),SVKEY        MOVE IN NEXT STATION TO PROCESS              
*                                                                               
* CLEAR 1ST 2 WORK AREAS IN ACTIVITY LIST BUILD AREA *                          
*                                                                               
         L     R3,ASPTABLE                                                      
         USING SPTABLED,R3                                                      
         XC    SPTDATA(L'SPTDATA*2),SPTDATA                                     
         DROP  R3                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE          A-M/C/P                                  
         BE    LRR05                                                            
         TM    SEEDTYPE,ESTALL+PRDALL  SEED FOR ALL PRDS OR EST                 
         BNZ   *+12                                                             
         BAS   RE,RELSTR           RELEASE STORAGE                              
         B     EXIT                                                             
*                                                                               
         TM    SEEDTYPE,DONEALL        DONE ALL SEED                            
         BNO   *+12                                                             
         BAS   RE,RELSTR                                                        
         B     EXIT                                                             
*                                                                               
         BAS   RE,VK34D                                                         
         TM    SEEDTYPE,DONEALL                                                 
         BZ    LRR                                                              
         BAS   RE,RELSTR           RELEASE STORAGE                              
         B     EXIT                                                             
*                                                                               
LRR05    CLC   KEY(9),KEYSAVE                                                   
         BE    LRR20                                                            
*                                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   LRR07                                                            
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    LRR10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    LRR10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART RADIO                          
         BE    LRR10                                                            
                                                                                
LRR07    DS    0H                                                               
*MNMB                                                                           
         MVC   KEYSAVE+4(5),KEY+4                                               
         B     LRR20                                                            
*                                                                               
LRR10    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   LRR15                                                            
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    LRR10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    LRR10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART RADIO                          
         BE    LRR10                                                            
                                                                                
LRR15    DS    0H                                                               
*MNMB                                                                           
*                                                                               
LRR20    CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BNE   LRR90                GO CK IF SPOTS FOR STATION                  
*                                                                               
LRR26    LLC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BE    LRR10                NO                                          
*                                                                               
         CLI   BEST,0              IS THIS SPECIFIC ESTIMATE                    
         BE    LRR28                NO                                          
         CLC   BEST,KEY+9                                                       
         BNE   LRR10                                                            
*                                                                               
* GET BUY RECORD *                                                              
*                                                                               
LRR28    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
*                                                                               
         CLC   KEY+4(2),4(R4)      SAME MKT ON DIR AS IN REC?                   
         BNE   LRR10               NO, MUST BE SPILL BUY                        
         TM    15(R4),X'80'        TEST DELETED BUY                             
         BO    LRR10               YES - IGNORE                                 
                                                                                
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
                                                                                
         LLC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
                                                                                
* LOOK FOR TRAFFIC=NO                                                           
                                                                                
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
LRR30    BAS   RE,BUYEL                                                         
         BNE   LRR35                                                            
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    LRR10               BYPASS THIS BUY                              
         B     LRR30                                                            
*                                                                               
LRR35    MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SR    R5,R5               ZERO EQ ELEM CTR                             
*                                                                               
LRR40    BAS   RE,BUYEL                                                         
         BNE   LRR10                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   LRR40                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   LRR40                                                            
         CLC   ELDATE,2(R6)        SAME DATE AS LAST                            
         BE    LRR44                YES                                         
         SR    R5,R5               ZERO EQ ELEM CTR                             
*                                                                               
LRR44    MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         LA    R5,1(R5)            ADD TO ELEMENT CTR                           
         XC    SVPROD(10),SVPROD                                                
         MVC   SVBPRD(2),10(R6)                                                 
         LA    R1,SVPROD                                                        
         BAS   RE,FPROD                                                         
*                                                                               
         XC    DUB,DUB             USE AS PRODUCTS REVERSED FLAG                
*                                                                               
         CLI   1(R6),18            THIS PIGGYBACK PROD                          
         BNE   LRR46                                                            
         MVC   SVBPRD2(2),14(R6)                                                
         LA    R1,SVPROD2                                                       
         BAS   RE,FPROD                                                         
*                                                                               
         CLC   SVPROD,SVPROD2      SEE IF PRODS IN ALPHA ORDER                  
         BL    LRR46                                                            
*                                                                               
* REVERSE PRODUCT ORDER                                                         
*                                                                               
         MVC   DUB(5),SVPROD                                                    
         MVC   SVPROD(5),SVPROD2                                                
         MVC   SVPROD2(5),DUB                                                   
*                                                                               
LRR46    CLC   BPRD,SVBPRD         THIS PROD                                    
         BNE   LRR40                NO                                          
         CLI   BSLN,0              ALLOW ALL                                    
         BE    LRR46C                                                           
         CLC   BSLN,SVBSLN         THIS SPOT LEN                                
         BNE   LRR40                NO                                          
*                                                                               
LRR46C   DS    0H                                                               
         CLI   1(R6),18            THIS PIGGYBACK                               
         BE    LRR47                YES                                         
*                                                                               
* CHECK SINGLE PRODUCTS HERE *                                                  
*                                                                               
         CLI   1(R6),14            THIS 1 PROD                                  
         BE    *+6                  YES                                         
         DC    H'0'                                                             
         CLI   BPRD2,0             LIMIT ON P/B'S                               
         BE    LRR48                NO                                          
         CLI   BPRD2,255           NO P/B'S AT ALL (NONE)                       
         BE    LRR48                YES, PROCESS                                
         B     LRR40               BYPASS                                       
*                                                                               
* CHECK PIGGY BACK ELEMENTS HERE *                                              
*                                                                               
LRR47    CLI   BPRD2,0             LIMIT ON P/B'S                               
         BE    LRR48                NO                                          
         CLI   BPRD2,255           NO P/B'S AT ALL (NONE)                       
         BE    LRR40                YES, BYPASS                                 
         CLC   BPRD2,SVBPRD2                                                    
         BNE   LRR40                                                            
         CLI   BSLN2,0             LIMIT ON P/B'S SPOT LEN                      
         BE    LRR48                NO                                          
         CLC   BSLN2,SVBSLN2                                                    
         BNE   LRR40                                                            
*                                                                               
LRR48    MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLC   ELDATE,PERENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    LRR40                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   LRR50                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
LRR50    CLC   ELDATEX,PERSTP      TEST BEFORE FLIGHT/TELECAST                  
         BL    LRR40                                                            
         EJECT                                                                  
* SAVE SPOT IN TABLE *                                                          
*                                                                               
         XC    SPTWORK,SPTWORK                                                  
         LA    R3,SPTWORK                                                       
         USING SPTABLED,R3                                                      
*                                                                               
         AP    SPTCT,=P'1'                                                      
*                                                                               
         LLC   R0,BUYKEY+10                                                     
         TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINE NUM                         
         BZ    *+8                                                              
         ICM   R0,3,BUYKEY+10                                                   
         CLM   R0,3,SVBUYLIN                                                    
         BE    LRR50A                                                           
         STCM  R0,3,SVBUYLIN                                                    
         AP    BUYLNCT,=P'1'                                                    
*                                                                               
LRR50A   OC    DUB,DUB             WERE PRODUCTS REVERSED                       
         BZ    *+8                                                              
         OI    SPTFLAG,SPTPREV     SET ON PRODUCTS REVERSED FLAG                
*                                                                               
         MVC   SPTFTD,ELDATE                                                    
         MVC   SPTLTD,ELDATEX                                                   
*                                                                               
         STC   R5,SPTSPTN          SAVE ELEMENT COUNTER                         
*                                                                               
         MVC   SPTDAY,BDDAY                                                     
         MVC   SPTTIME,BDTIMST                                                  
         MVC   SPTEST,BUYKEST                                                   
*                                                                               
         LLC   R0,BUYKEY+10                                                     
         TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINE                             
         BZ    *+8                                                              
         ICM   R0,3,BUYKEY+10                                                   
         STCM  R0,3,SPTLINE                                                     
*                                                                               
         MVC   SPTDSKAD,KEY+14                                                  
         MVC   SPTPROD,SVPROD                                                   
         MVC   SPTBPRD,SVBPRD                                                   
         MVC   SPTSLN,SVBSLN                                                    
         MVC   SPTDPT,BDDAYPT                                                   
         MVC   SPTPROGT,BDPROGT                                                 
*                                                                               
         CLC   SPTEST,SVPEST       SAME AS PREVIOUS ESTIMATE                    
         BE    *+14                 YES                                         
         MVC   SVPEST,SPTEST       SAVE ESTIMATE                                
         MVI   FORCEHED,C'Y'       AND FORCE NEW PAGE                           
*                                                                               
         CLC   SPTBPRD,SVPPRD                                                   
         BE    *+14                                                             
         MVC   SVPPRD,SPTBPRD                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   1(R6),18            TEST PIGGYBACK                               
         BL    LRR51                                                            
         MVC   SPTPROD2,SVPROD2                                                 
         MVC   SPTBPRD2,SVBPRD2                                                 
         MVC   SPTSLN2,SVBSLN2                                                  
*                                                                               
LRR51    LR    RE,R6                                                            
         LLC   R0,1(R6)                                                         
         B     LRR54                                                            
*                                                                               
LRR52    LLC   R0,1(RE)                                                         
*                                                                               
LRR54    AR    RE,R0                                                            
         CLI   0(RE),13            THIS ANOTHER SPOT                            
         BNH   LRR60                YES                                         
         CLI   0(RE),X'18'         THIS A CML ASSIGN ELEMENT                    
         BNE   LRR52                NO, LOOK FURTHUR                            
         CLI   1(RE),9             THIS A DEALER TAG?                           
         BE    DLRTAGER             YES                                         
*                                                                               
         MVC   SPTCMLSQ(4),TRACCSQ-TRACID(RE)   SAVE BOTH CMML SEQ              
         MVC   SPTPREF,TRACREF-TRACID(RE)                                       
         NI    SPTPREF,X'7F'        SET OFF PRINTED ON INSTR FLAG               
*                                                                               
         OC    SPTCMLSQ(4),SPTCMLSQ  ARE COMMERCIALS ASSIGNED                   
         BZ    *+14                  NO                                         
         OI    SPTFLAG,SPTPRVA       SET ON PREVIOUS ASSIGN FLAG                
         AP    SPTASCT,=P'1'                                                    
*                                                                               
         OC    SPTPREF,SPTPREF     ASSIGNED FROM PATTERN                        
         BZ    *+8                                                              
         OI    SPTFLAG,SPTPRVPT    SET ON SOURCE WAS PATTERN                    
*                                                                               
         TM    TRACREF-TRACID(RE),X'80' WAS SPOT PRINTED                        
         BZ    *+8                                                              
         OI    SPTFLAG,SPTPRVPR    SET ON SPOT WAS PRINTED ON INSTR             
*                                                                               
LRR56    TM    SPTFLAG,SPTPREV     WERE PRODS SWAPPED TO ALPHA ORDER            
         BZ    LRR60                NO                                          
         MVC   SPTCMLSQ,TRACCSQ2-TRACID(RE)                                     
         MVC   SPTCMLS2,TRACCSQ-TRACID(RE)                                      
                                                                                
* TEST DATA IN TABLE ALREADY *                                                  
                                                                                
LRR60    L     R3,ASPTABLE                                                      
*                                                                               
LRR64    CLI   0(R3),0                                                          
         BE    LRR70                                                            
         CLC   0(L'SPTDATA,R3),SPTWORK CK EQUAL ENTRY                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SPTNEXT                                                       
         B     LRR64                                                            
*                                                                               
LRR70    C     R3,ASPTEND                                                       
         BH    SPTSIZER                                                         
*                                                                               
         LH    R1,SPOTCT           UPDATE TOTAL SPOTS COUNTER                   
         LA    R1,1(R1)                                                         
         STH   R1,SPOTCT                                                        
*                                                                               
         MVC   SPTDATA,SPTWORK                                                  
*                                                                               
         LA    R3,SPTNEXT                                                       
         C     R3,ASPTEND                                                       
         BH    SPTSIZER                                                         
         XC    0(L'SPTDATA,R3),0(R3)                                            
         B     LRR40                                                            
         DROP  R3,R4                                                            
                                                                                
* SORT SPOTS IN PRD SLN/PTR SLN, DATE ORDER FOR STATION                         
                                                                                
LRR90    MVC   SVKEY,KEY           SAVE FOR NEXT STATION/MARKET                 
*                                                                               
         MVC   SVMKTSTA,KEYSAVE+4                                               
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
* IF NO SPOTS READ, GO TO NEXT STATION                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,SPOTCT         GET NUMBER OF SPOTS                          
         BZ    NEXTSTA                                                          
*                                                                               
         OI    LOG,LOGSPOTS        SET ON SPOTS FOUND                           
         OI    SEEDTYPE,FOUNDONE   AT LEAST ONE FOUND (PRD/EST ALL)             
                                                                                
* SORT ON ALPHA PRD SLN/PTR SLN, DATE AND TIME *                                
                                                                                
         GOTO1 AQSORT,DMCB,ASPTABLE,(R2),L'SPTDATA,L'SPTSORT,0                  
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),QMKT,WORK                           
*                                                                               
         MVC   QSTA,WORK                                                        
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         XC    STANET,STANET                                                    
*                                                                               
         CLC   WORK+5(3),SPACES    IS THIS A CABLE HEAD                         
         BE    CREF                 NO                                          
*                                                                               
         MVC   STANET(4),QSTA                                                   
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),WORK+5                                               
         EJECT                                                                  
*==================================================================             
* SUBROUTINE TO CLEAR CMMLS BY PATTERN REFERENCE NUMBER(S)                      
*==================================================================             
                                                                                
CREF     DS    0H                                                               
         L     R3,ASPTABLE                                                      
         LH    R4,SPOTCT                                                        
         SR    RF,RF                                                            
         OC    CREFTABL,CREFTABL   ANY PAT REFS TO CLEAR                        
         BZ    GETPTN                                                           
*                                                                               
         USING SPTABLED,R3                                                      
*                                                                               
CREF10   OC    SPTFTD,SPTFTD       END OF TABLE                                 
         BZ    CREF40                                                           
*                                                                               
         LA    R0,15                                                            
         LA    R1,CREFTABL                                                      
*                                                                               
CREF20   CLC   SPTPREF,0(R1)                                                    
         BNE   CREF26                                                           
         XC    SPTPREF,SPTPREF                                                  
         XC    SPTCMLSQ,SPTCMLSQ                                                
*                                                                               
* CLEAR PREVIOUS ASSIGN, SOURCE = PATTERN, PRINTED ON INST FLAGS                
*                                                                               
         NI    SPTFLAG,X'FF'-SPTPRVA-SPTPRVPT-SPTPRVPR                          
*                                                                               
         LH    RE,2(R1)                                                         
         LA    RE,1(,RE)                                                        
         STH   RE,2(R1)                                                         
         BCTR  RF,0                                                             
         B     CREF30                                                           
*                                                                               
CREF26   LA    R1,4(,R1)                                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    CREF30                                                           
         BCT   R0,CREF20                                                        
*                                                                               
CREF30   LA    R3,SPTNEXT                                                       
         B     CREF10                                                           
*                                                                               
CREF40   LTR   RF,RF               WERE THERE ANY CLEARED                       
         BZ    GETPTN               NO                                          
*                                                                               
         MVC   PMKT,QMKT                                                        
*                                                                               
         MVC   PSTA(4),QSTA                                                     
         LA    R1,PSTA+4                                                        
         CLI   PSTA+3,C' '                                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),QSTA+4                                                   
*                                                                               
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   PSTA(8),STANET                                                   
*                                                                               
         LA    R2,PLIN+2                                                        
         LA    R4,15                                                            
         LA    R5,CREFTABL                                                      
         MVC   PDATE(7),=C'CLEARED'                                             
*                                                                               
CREF50   OC    0(2,R5),0(R5)                                                    
         BZ    CREF60                                                           
         OC    2(2,R5),2(R5)       ANY CLEARED                                  
         BZ    CREF56                                                           
*                                                                               
         LA    R1,PLIN+2                                                        
         CR    R1,R2                                                            
         BE    CREF52                                                           
*                                                                               
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         MVI   0(R1),C','                                                       
         LA    R2,1(,R2)                                                        
*                                                                               
CREF52   MVC   0(3,R2),=C'REF '                                                 
         LA    R2,4(,R2)                                                        
*                                                                               
         EDIT  (B2,(R5)),(4,(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVI   1(R2),C'='                                                       
         LA    R2,3(,R2)                                                        
*                                                                               
         EDIT  (B2,2(R5)),(4,(R2)),ALIGN=LEFT                                   
         AR    R2,R0                                                            
*                                                                               
         MVC   1(5,R2),SPOTS                                                    
*                                                                               
         LA    R2,7(,R2)                                                        
         XC    2(2,R5),2(R5)       CLEAR CT                                     
*                                                                               
         LA    R1,P+110                                                         
         CR    R2,R1                                                            
         BL    CREF56                                                           
*                                                                               
         TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    CREF54                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CREF54   MVC   P,SPACES                                                         
         LA    R2,PLIN+2                                                        
*                                                                               
CREF56   LA    R5,4(,R5)                                                        
         BCT   R4,CREF50                                                        
*                                                                               
CREF60   TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    CREF64                                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CREF64   MVC   P,SPACES                                                         
         EJECT                                                                  
*==================================================================             
* SUBROUTINE TO READ PATTERNS, THEN ASSIGN COMMERCIALS TO SPOTS                 
* BUILDS PATTERN TABLE IN AIO2, THEN ASSIGNS COMMLS TO SPOTS                    
*==================================================================             
                                                                                
GETPTN   BRAS  RE,FNDCMLS          GO FIND CMMLS CURRENTLY IN BUY               
*                                                                               
         L     R3,ASPTABLE                                                      
         USING SPTABLED,R3                                                      
         ST    R3,ACURRSPT                                                      
*                                                                               
GETP10   XC    DPCTABLE,DPCTABLE   INIT COPY CODE TABLE                         
*                                                                               
         L     R0,APTNTABL         CLEAR PATTERN TABLE AREA                     
         L     R1,APTNTABX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,APTNTABL                                                      
         USING PTNLISTD,R5                                                      
         MVI   PATSEQ,0                                                         
         XC    0(L'PTNLIST+1,R5),0(R5)                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SPTBPRD    PRD                                          
         MVC   KEY+6(1),SPTSLN     SLN                                          
         MVC   KEY+7(1),SPTBPRD2   PTR                                          
         MVC   KEY+8(1),SPTSLN2    SLN                                          
         MVC   KEY+9(1),SPTDPT     DAYPART                                      
*                                                                               
         MVC   DPCTABLE(1),SPTDPT      SAVE DPT IN DPC TABLE                    
         CLI   SVPROF+10,C'D'          COPY CODE = DAYPART ?                    
         BE    GETP20                                                           
*                                                                               
         MVC   KEY+9(1),SPTPROGT       PROGRAM ADJ CODE                         
         MVC   DPCTABLE(1),SPTPROGT    SAVE IN COPY CODE TABLE                  
         CLI   SVPROF+10,C'A'          COPY CODE = PROG ADJ CODE?               
         BE    GETP20                                                           
*                                                                               
         MVC   KEY+9(1),BEST       ESTIMATE                                     
         B     GETP20                                                           
*                                                                               
GETP16   SR    RE,RE                                                            
         ICM   RE,7,KEY+10         GET REF/SUBL                                 
         SRL   RE,10               DROP SUBLINE                                 
         LA    RE,1(RE)            BUMP LINE NUMBER                             
         SLL   RE,10                                                            
         STCM  RE,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    TEST REACHED END                             
         BZ    GETP30               YES                                         
*                                                                               
GETP20   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GETP30                                                           
*                                                                               
         L     R6,AIO1             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         TM    15(R6),X'03'        TEST INCOMPLETE OR BPAT                      
         BNZ   GETP16                                                           
*                                                                               
         BRAS  RE,BLDLIST          BUILD PATTERN TABLE ENTRY                    
         GOTO1 VSWITCH,=C'STR'     SWITCH TO SPOT MEDIA SYSTEM                  
         B     GETP16                                                           
         DROP  R5                                                               
*                                                                               
GETP30   DS    0H                                                               
         CLI   BEST,0              ANY EST EXPECTED                             
         BE    GETP40               NO                                          
         CLI   SVPROF11,C'E'       THIS COPY CODE = EST                         
         BE    GETP50               MUST HAVE CODED PTTNS                       
         CLI   SVPROF11,C'D'       THIS COPY CODE = DAYPART                     
         BE    GETP40               YES                                         
         CLI   SVPROF11,C'A'       THIS COPY CODE = PROG ADJ                    
         BE    GETP40               YES                                         
*                                                                               
         CLI   KEYSAVE+PATKCODE-PATKEY,0 ALREADY DONE NON-COPY CODED            
         BE    GETP40                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE                                                   
         B     GETP16                                                           
*                                                                               
GETP40   DS    0H                                                               
         L     R1,ADPCTABL         TABLE OF PROCESSED CODES                     
         L     RF,ADPCTEND         END OF DPC TABLE                             
*                                                                               
         LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       END OF TABLE                                 
         BZ    GETP50                                                           
*                                                                               
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BNE   GETP44                                                           
*                                                                               
GETP42   CLC   SPTDPT,0(R1)        DID WE DO THIS DPT YET                       
         BE    GETP40               YES, SKIP                                   
         LA    R1,1(R1)                                                         
         CR    R1,RF               REACHED END OF TABLE?                        
         BL    *+6                                                              
         DC    H'0'                DAYPART TABLE TOO SMALL                      
         CLI   0(R1),0             EMPTY?                                       
         BNE   GETP42               NO                                          
*                                                                               
         MVC   0(1,R1),SPTDPT      ADD DPT TO TABLE                             
*                                                                               
         MVC   KEY(9),KEYSAVE      RESTORE THE KEY                              
         MVC   KEY+9(1),SPTDPT     MOVE IN NEXT DAYPART                         
         B     GETP20                                                           
*                                                                               
GETP44   CLI   SVPROF+10,C'A'      COPY CODE = PROG ADJ CODE?                   
         BNE   GETP50                                                           
*                                                                               
GETP46   CLC   SPTPROGT,0(R1)      DID WE DO THIS ONE YET?                      
         BE    GETP40               YES, SKIP                                   
         LA    R1,1(R1)                                                         
         CR    RF,R1               REACHED END OF TABLE?                        
         BH    *+6                                                              
         DC    H'0'                DAYPART TABLE TOO SMALL                      
         CLI   0(R1),0             EMPTY?                                       
         BNE   GETP46                                                           
*                                                                               
         MVC   0(1,R1),SPTPROGT                                                 
*                                                                               
         MVC   KEY(9),KEYSAVE      RESTORE THE KEY                              
         MVC   KEY+9(1),SPTPROGT   PROGRAM ADJ CODE                             
         B     GETP20                                                           
*                                                                               
GETP50   BRAS  RE,BLDDATE          BLD PTTN LIST BY DATE & ASGN CMLS            
*                                                                               
         L     R3,ACURRSPT                                                      
         CLI   0(R3),0             AT END OF TABLE                              
         BNE   GETP10              NO, PROCESS NEXT PROD                        
         EJECT                                                                  
*======================================================                         
* UPDATE BUY RECS FROM SPOT TABLE                                               
*======================================================                         
                                                                                
         L     R3,ASPTABLE                                                      
         LH    R4,SPOTCT                                                        
*                                                                               
         USING SPTABLED,R3                                                      
                                                                                
* SORT ON DISK ADDRESS AND SPOT NUMBER *                                        
                                                                                
         GOTO1 AQSORT,DMCB,(R3),(R4),L'SPTDATA,L'SPTUSORT,             C        
               SPTDSKAD-SPTDATA                                                 
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
UPD20    MVC   KEY+14(4),SPTDSKAD                                               
         L     R6,AIO1                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',SYSFIL,KEY+14,(R6),             C        
               DMWORK                                                           
*                                                                               
         NI    LOG,X'FF'-LOGUPDSW  SET OFF RECORD UPDATE NEEDED                 
*                                                                               
         MVC   SVPROGN,BDPROGRM-BUYRECD(R6)                                     
         MVC   SVDAYPT,BDDAYPT-BUYRECD(R6)                                      
*                                                                               
UPD30    MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
         XC    ELDATE,ELDATE                                                    
         EJECT                                                                  
*=======================================================                        
* UPDATE ALL ELEMS FOR THIS BUY                                                 
*=======================================================                        
                                                                                
UPD40    BAS   RE,BUYEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    6(R6),X'C0'                                                      
         BNZ   UPD40                                                            
         CLI   1(R6),10            UNALLOCATED                                  
         BNH   UPD40                                                            
         CLC   2(2,R6),ELDATE                                                   
         BE    UPD44                                                            
         MVI   SPOTNUMB,0                                                       
         MVC   ELDATE,2(R6)                                                     
*                                                                               
UPD44    LLC   RE,SPOTNUMB                                                      
         LA    RE,1(RE)                                                         
         STC   RE,SPOTNUMB                                                      
*                                                                               
         CLC   2(2,R6),SPTFTD      SAME DATE                                    
         BNE   UPD40               NO                                           
         CLC   SPTSPTN,SPOTNUMB    SAME SPOT NUMBER                             
         BNE   UPD40               NO                                           
*                                                                               
         CLI   SPTBPRD2,0          IS THERE A PIGGYBACK PROD                    
         BE    UPD50                                                            
         CLI   1(R6),18                                                         
         BL    UPD40                                                            
*                                                                               
         TM    SPTFLAG,SPTPREV     WERE PRODUCTS REVERSED                       
         BO    UPD46                YES                                         
*                                                                               
         CLC   SPTBPRD,10(R6)      SAME BPRD                                    
         BNE   UPD40                                                            
         CLC   SPTSLN,11(R6)       SAME SLN                                     
         BNE   UPD40                                                            
         CLC   SPTBPRD2,14(R6)     SAME BPRD2                                   
         BNE   UPD40                                                            
         CLC   SPTSLN2,15(R6)      SAME SLN2                                    
         BNE   UPD40                                                            
         B     UPD54                                                            
                                                                                
* CK FOR PRODS WERE OUT OF ORDER *                                              
                                                                                
UPD46    CLC   SPTBPRD2,10(R6)     SAME BPRD                                    
         BNE   UPD40                                                            
         CLC   SPTSLN2,11(R6)      SAME SLN                                     
         BNE   UPD40                                                            
         CLC   SPTBPRD,14(R6)      SAME BPRD                                    
         BNE   UPD40                                                            
         CLC   SPTSLN,15(R6)       SAME SLN                                     
         BNE   UPD40                                                            
         MVC   DUB(4),SPTPROD                                                   
         MVC   DUB+4(1),SPTBPRD                                                 
         MVC   SPTPROD(4),SPTPROD2                                              
         MVC   SPTBPRD,SPTBPRD2                                                 
         MVC   SPTPROD2(4),DUB                                                  
         MVC   SPTBPRD2,DUB+4                                                   
         MVC   DUB(2),SPTCMLSQ                                                  
         MVC   SPTCMLSQ,SPTCMLS2                                                
         MVC   SPTCMLS2,DUB                                                     
         B     UPD54                                                            
*                                                                               
UPD50    CLI   1(R6),14                                                         
         BH    UPD40                                                            
         CLC   SPTBPRD,10(R6)      SAME BPRD                                    
         BNE   UPD40                                                            
         CLC   SPTSLN,11(R6)       SAME SLN                                     
         BNE   UPD40                                                            
                                                                                
* FOUND THE MATCHING SPOT - FORMAT PRINT LINE FOR IT *                          
                                                                                
UPD54    MVC   PMKT,QMKT                                                        
         MVC   PSTA(4),QSTA                                                     
         LA    R1,PSTA+4                                                        
         CLI   PSTA+3,C' '                                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),QSTA+4                                                   
*                                                                               
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   PSTA(8),STANET                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,SPTFTD),(4,PDATE)                                 
         SR    R0,R0                                                            
         ICM   R0,3,SPTLINE                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
         GOTO1 UNDAY,DMCB,SPTDAY,PDAY                                           
         GOTO1 UNTIME,DMCB,SPTTIME,PTIME                                        
*                                                                               
         MVC   PPROG,SVPROGN                                                    
         MVC   PDP,SVDAYPT                                                      
*                                                                               
         MVC   PPRD(3),SPTPROD                                                  
         LA    R2,PPRD+3                                                        
         CLI   SPTPROD+2,C' '                                                   
         BH    *+6                                                              
         BCTR  R2,0                                                             
         MVI   0(R2),C'-'                                                       
         LLC   R0,SPTSLN                                                        
         EDIT  (R0),(3,1(R2)),ALIGN=LEFT                                        
*                                                                               
         OC    SPTPROD2,SPTPROD2                                                
         BZ    UPD56                                                            
         MVC   PPRD2(3),SPTPROD2                                                
         LA    R2,PPRD2+3                                                       
         CLI   SPTPROD2+2,C' '                                                  
         BH    *+6                                                              
         BCTR  R2,0                                                             
         MVI   0(R2),C'-'                                                       
         LLC   R0,SPTSLN2                                                       
         EDIT  (R0),(3,1(R2)),ALIGN=LEFT                                        
*                                                                               
UPD56    OC    SPTCMLSQ,SPTCMLSQ   IS THERE A CMML                              
         BZ    UPD62                                                            
*                                                                               
         LA    R1,SPTCMLSQ                                                      
         BAS   RE,FCMLCD                                                        
         MVC   PCML,CMLTCML-CMLTDATA(RF)                                        
*                                                                               
UPD62    OC    SPTCMLS2,SPTCMLS2   IS THERE A CMML                              
         BZ    UPD64                                                            
*                                                                               
         LA    R1,SPTCMLS2                                                      
         BAS   RE,FCMLCD                                                        
         MVC   PCML2,CMLTCML-CMLTDATA(RF)                                       
*                                                                               
UPD64    EDIT  SPTPREF,(4,PREF),ALIGN=LEFT                                      
*                                                                               
         LR    R4,R6                                                            
UPD66    LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             THIS END OF RECORD                           
         BE    UPD70                YES, CREATE NEW ELEM                        
*                                                                               
         CLI   0(R4),13            THIS ANOTHER SPOT                            
         BNH   UPD70                YES, CREATE NEW ELEM                        
*                                                                               
         CLI   0(R4),X'18'         THIS A CML ASSIGN ELEMENT                    
         BE    UPD68                YES, UPDATE IT IF NEEDED                    
*                                                                               
         CLI   0(R4),X'1F'         THIS PAST SPOTS                              
         BH    UPD70                YES, CREATE NEW ELEM                        
         B     UPD66                NO, KEEP ON LOOKING                         
*                                                                               
UPD68    CLI   1(R4),9             THIS A DEALER TAG?                           
         BNE   *+6                  NO                                          
         DC    H'0'                                                             
*                                                                               
         OC    SPTCMLSQ(4),SPTCMLSQ  ANY COMMLS                                 
         BZ    UPD80                                                            
*                                                                               
         CLC   TRACCSQ-TRACID(4,R4),SPTCMLSQ  SAME COMMLS                       
         BNE   *+14                                                             
         MVC   PCMTS(8),=C'RETAINED'                                            
         B     UPD80                                                            
*                                                                               
         CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   *+12                                                             
         TM    SPTFLAG,SPTPRVA+SPTPRVPR  WAS SPOT PREV ASSIGNED/PRINTED         
         BNZ   UPD80                                                            
*                                                                               
* IF WE EVER HAVE TO TRACK DIFFERENT PATTERN SOURCE, USE THIS *                 
*                                                                               
*        CLC   TRACREF-TRACID(,R4),SPTPREF    SAME PATTERN REF #                
*        BE    UPD80                           YES                              
*        MVC   HALF,TRACREF-TRACID(R4)        SAVE PATTERN REF #                
*        NI    HALF,X'7F'                     DROP PRINTED FLAG                 
*        CLC   HALF,SPTPREF                   SAME PATTERN REF #                
*        BE    UPD80                                                            
*                                                                               
         MVC   TRACCSQ-TRACID(4,R4),SPTCMLSQ                                    
         MVC   TRACREF-TRACID(,R4),SPTPREF      SAVE PATTERN REF #              
*                                                                               
         MVC   PCMTS(8),=C'ASSIGNED'                                            
         OI    LOG,LOGUPDSW                    SET ON UPDATE NEEDED             
*                                                                               
         TM    SPTFLAG,SPTPRVA     IS PREVIOUS ASSIGN FLAG ON                   
         BZ    UPD80                                                            
         MVC   PCMTS(8),=C'REPLACED'                                            
         B     UPD80                                                            
*                                                                               
UPD70    DS   0H                                                                
         OC    SPTCMLSQ(4),SPTCMLSQ   IS THERE A CMML                           
         BNZ   UPD72                   YES, UPDATE                              
*                                                                               
         TM    SPTFLAG,SPTNOPT     NO PATTERN FOR THIS SPOT                     
         BZ    UPD80                   NO                                       
         MVC   PCMTS(10),=C'NO PATTERN'                                         
         B     UPD80                                                            
*                                                                               
UPD72    XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVI   TRACID-TRACID(R1),X'18'    ELCODE                                
         MVI   TRACLN-TRACID(R1),TRACLEN  LENGTH                                
         MVC   TRACCSQ-TRACID(4,R1),SPTCMLSQ                                    
         MVC   TRACREF-TRACID(,R1),SPTPREF       SAVE PATERN REF #              
*                                                                               
         MVI   PCMTS-1,C'*'                                                     
*                                                                               
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,AIO1,ELEM,(C'R',(R4))                                
         CLI   8(R1),C'R'          TEST ELEMENT FIT IN RECORD                   
         BNE   UPD76               NO - TELL THEM TO SPLIT                      
*                                                                               
         OI    LOG,LOGUPDSW                    SET ON UPDATE NEEDED             
         MVC   PCMTS(8),=C'ASSIGNED'                                            
         TM    SPTFLAG,SPTPRVA     IS PREVIOUS ASSIGN FLAG ON                   
         B     UPD80                TEMP FOR FXING OUT OF PLACE ELEMS           
         BZ    UPD80                                                            
         DC    H'0'                                                             
*                                                                               
UPD76    MVC   PCMTS(14),=C'TOO BIG, SPLIT'                                     
*                                                                               
UPD80    TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    UPD84                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
UPD84    MVC   P,SPACES                                                         
*                                                                               
UPD86    LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             AT END OF SPOT TABLE                         
         BE    UPD90               YES                                          
         CLC   SPTDSKAD,KEY+14     SAME REC                                     
         BE    UPD30               YES                                          
*                                                                               
UPD90    TM    LOG,LOGUPDSW                    IS UPDATE NEEDED                 
         BZ    UPD94                                                            
         TM    OPTFLAG,OPTTEST                                                  
         BO    UPD94                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   TWAWRITE,C'Y'                                                    
         BNE   UPD92                                                            
                                                                                
* DO GETREC WITH UPDATE FOR PUTREC                                              
                                                                                
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,KEY+14,(R6),     C        
               DMWORK                                                           
         GOTO1 DATAMGR,DMCB,=C'PUTREC',SYSFIL,KEY+14,AIO1,DMWORK                
*                                                                               
UPD92    AP    BUYUPCT,=P'1'                                                    
*                                                                               
UPD94    CLI   0(R3),0             AT END OF SPOT TABLE                         
         BNE   UPD20                NO, UPDATE NEXT BUY                         
*                                                                               
         TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    NEXTSTA                                                          
*                                                                               
         EDIT  BUYLNCT,(5,P+8),ZERO=NOBLANK                                     
         MVC   P+14(13),=C'BUYLINES WITH'                                       
*                                                                               
         EDIT  SPTCT,(5,P+29),ZERO=NOBLANK                                      
         MVC   P+35(10),SPOTSRD                                                 
*                                                                               
         EDIT  SPTASCT,(5,P+49),ZERO=NOBLANK                                    
         MVC   P+55(19),=C'SPOTS PREV ASSIGNED'                                 
*                                                                               
         CP    BUYUPCT,=P'0'       ANY BUY UPDATED?                             
         BE    UPD96                NO                                          
*                                                                               
         EDIT  BUYUPCT,(5,P+79),ZERO=NOBLANK                                    
         MVC   P+85(13),=C'BUYLINES WITH'                                       
*                                                                               
         EDIT  SPTUPCT,(5,P+100),ZERO=NOBLANK                                   
         MVC   P+106(13),=C'SPOTS UPDATED'                                      
*                                                                               
UPD96    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
NEXTSTA  AP    TBUYLNCT,BUYLNCT                                                 
         AP    TBUYUPCT,BUYUPCT                                                 
         AP    TSPTCT,SPTCT                                                     
         AP    TSPTASCT,SPTASCT                                                 
         AP    TSPTUPCT,SPTUPCT                                                 
         ZAP   BUYLNCT,=P'0'                                                    
         ZAP   BUYUPCT,=P'0'                                                    
         ZAP   SPTCT,=P'0'                                                      
         ZAP   SPTASCT,=P'0'                                                    
         ZAP   SPTUPCT,=P'0'                                                    
         XC    SVBUYLIN,SVBUYLIN                                                
*                                                                               
         OC    OPTMKTST,OPTMKTST   WAS STATION ENTERED                          
         BNZ   CKSPOTS                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(4),BAGYMD                                                    
         CLC   SVKEY(4),KEY                                                     
         BNE   CKSPOTS                                                          
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         OC    BMKT,BMKT           1 MARKET REQUEST                             
         BZ    LRR00                                                            
         CLC   BMKT,SVKEY+4        1 MARKET REQUEST                             
         BE    LRR00                                                            
*                                                                               
CKSPOTS  TM    LOG,LOGSPOTS        WERE ANY SPOTS FOUND                         
         BNZ   *+16                                                             
         TM    SEEDTYPE,ESTALL+PRDALL IF SEED FOR ALL PRDS OR EST               
         BZ    NOSPTER             NO, ERROR                                    
         B     CKSPTS20                                                         
         TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    PRTUPDMS                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+2(5),=C'TOTAL'                                                 
         EDIT  TBUYLNCT,(5,P+8),ZERO=NOBLANK                                    
         MVC   P+14(13),=C'BUYLINES WITH'                                       
*                                                                               
         EDIT  TSPTCT,(5,P+29),ZERO=NOBLANK                                     
         MVC   P+35(10),=C'SPOTS READ'                                          
*                                                                               
         EDIT  TSPTASCT,(5,P+49),ZERO=NOBLANK                                   
         MVC   P+55(19),=C'SPOTS PREV ASSIGNED'                                 
*                                                                               
         CP    BUYUPCT,=P'0'       ANY BUYS UPDATED?                            
         BE    CKSPTS10             NO                                          
*                                                                               
         EDIT  TBUYUPCT,(5,P+79),ZERO=NOBLANK                                   
         MVC   P+85(13),=C'BUYLINES WITH'                                       
*                                                                               
         EDIT  TSPTUPCT,(5,P+100),ZERO=NOBLANK                                  
         MVC   P+106(13),=C'SPOTS UPDATED'                                      
*                                                                               
CKSPTS10 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    SEEDTYPE,ESTALL+PRDALL IF SEED FOR ALL PRDS OR EST               
         BNZ   *+12                                                             
         BAS   RE,RELSTR           RELEASE STORAGE                              
         B     EXIT                                                             
*                                                                               
CKSPTS20 TM    SEEDTYPE,DONEALL    DONE ALL SEED                                
         BZ    *+16                                                             
         TM    SEEDTYPE,FOUNDONE   AT LEAST ONE DONE                            
         BZ    NOSPTER             NO, ERROR                                    
         BAS   RE,RELSTR           RELEASE STORAGE                              
         B     EXIT                                                             
         BAS   RE,VK34D                                                         
         TM    SEEDTYPE,DONEALL                                                 
         BZ    LRR                                                              
         TM    SEEDTYPE,FOUNDONE   AT LEAST ONE DONE                            
         BZ    NOSPTER             NO, ERROR                                    
         BAS   RE,RELSTR           RELEASE STORAGE                              
         B     EXIT                                                             
         EJECT                                                                  
FCMLCD   L     RF,ACMLTAB                                                       
*                                                                               
FCMLCD10 CLC   0(2,R1),0(RF)                                                    
         BER   RE                                                               
         LA    RF,L'CMLTDATA(RF)                                                
         OC    0(2,RF),0(RF)                                                    
         JNZ   FCMLCD10                                                         
         MVC   WORK(2),0(R1)       MOVE THIS CODE                               
         MVC   WORK+2(12),=12C'?'                                               
         LA    RF,WORK                                                          
         BR    RE                                                               
                                                                                
* FIND 3 CHAR PROD CODE FROM 1 BINARY CODE                                      
                                                                                
FPROD    L     RF,ASVCLIST                                                      
*                                                                               
FPROD10  CLC   3(1,R1),3(RF)                                                    
         BE    FPROD20                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    FPROD10                                                          
         DC    H'0'                                                             
*                                                                               
FPROD20  MVC   0(3,R1),0(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*RELEASE STORAGE IF OFFLINE                                                     
RELSTR   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         JNE   EXIT                                                             
         LAY   R1,SPTTSIZE                                                      
         ICM   R0,15,0(R1)                                                      
         L     R1,ASPTABLE                                                      
         LTR   R1,R1                                                            
         JZ    EXIT                                                             
         SHI   R1,8                                                             
         STORAGE RELEASE,LENGTH=(R0),ADDR=(R1),COND=YES                         
         LTR   RF,RF               ERROR?                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    ASPTABLE,ASPTABLE                                                
         J     EXIT                                                             
*                                                                               
*-------------------------------------------------------                        
* CHECK IF PRODUCT IS THEATRICAL FOR PRD=ALL REQUEST                            
*-------------------------------------------------------                        
CTHTR    NTR1                                                                   
         XC    FLDH,FLDH                                                        
         MVI   FLDH+5,3            SAVE LENGTH                                  
         MVC   FLD(3),QPRD         3 CHAR PROD                                  
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         GIVE ME CONTROL FOR ERROR HANDLING           
         NI    SECFLAG,X'FF'-PRDTHTR INIT THEATRICAL PRODUCT                    
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT, SETS CC                  
         XIT1                                                                   
         EJECT                                                                  
*=======================================================                        
* HEADHOOK ROUTINE                                                              
*=======================================================                        
                                                                                
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
*                                                                               
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,PERSTP),(5,H4+48)                                 
         MVI   H4+58,C'-'                                                       
         GOTO1 (RF),(R1),(2,PERENDP),(5,H4+61)                                  
*                                                                               
         CLI   BEST,0                                                           
         BNE   HDHK10                                                           
         MVC   H6+10(4),=C'NONE'                                                
         B     HDHK20                                                           
*                                                                               
HDHK10   LLC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H6+10(3),DUB                                                     
*                                                                               
HDHK20   TM    OPTFLAG,OPTTEST     THIS A TEST RUN                              
         BZ    HDHKX                                                            
         MVC   H3+52(14),=C'** TEST RUN **'                                     
HDHKX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BUYEL    CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
NOESTER  MVI   ERROR,NOESTS                                                     
         B     TRAPERR                                                          
*                                                                               
BDESTPR  MVI   ERROR,BADESTS                                                    
         B     TRAPERR                                                          
*                                                                               
MISSCLT  LA    R2,TRACLTH                                                       
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
*                                                                               
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     TRAPERR                                                          
*                                                                               
NOTNOWER LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
NUMERR   MVI   ERROR,NOTNUM                                                     
*                                                                               
TRAPERR  BAS   RE,RELSTR                                                        
         GOTO1 ERREX                                                            
*                                                                               
THTRERR  BAS   RE,RELSTR                                                        
         MVC   GERROR,=Y(THTRFMS)  THEATRICAL PRODUCT FOUND ERROR               
         GOTO1 VTRAERR                                                          
*                                                                               
INVSLNER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'* ERROR * INVALID PRODUCT LENGTH'                 
         LA    R2,TRAPRDH                                                       
         B     ERREXIT                                                          
*                                                                               
PRTUPDMS XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=C'* NOTE *'                                          
         LA    R2,CONHEAD+8                                                     
         EDIT  TBUYLNCT,(4,(R2)),ZERO=NOBLANK                                   
         MVC   CONHEAD+13(13),=C'BUYLINES WITH'                                 
         LA    R2,CONHEAD+27                                                    
         EDIT  TSPTUPCT,(4,(R2)),ZERO=NOBLANK                                   
         MVC   CONHEAD+32(13),=C'SPOTS UPDATED'                                 
         LA    R2,TRAMEDH                                                       
         B     ERREXIT                                                          
*                                                                               
DLRTAGER LAY   R1,DLRTAGMS                                                      
         LA    R2,TRACLTH          CLIENT                                       
         B     COMERR              NOTHING TO SAVE                              
*                                                                               
SPTSIZER LAY   R1,SPTSIZMS                                                      
         LA    R2,TRAMEDH                                                       
         B     COMERR              NOTHING TO SAVE                              
*                                                                               
NOPRVCML LAY   R1,NOPRVMS                                                       
         LA    R2,TRAMEDH                                                       
         B     COMERR                                                           
*                                                                               
NOPBERR  LAY   R1,NOPBMS                                                        
         LA    R2,TRAMEDH                                                       
         B     COMERR                                                           
*                                                                               
CMLSLNER LAY   R1,CMLSLNMS                                                      
         LA    R2,TRAMEDH                                                       
         B     COMERR                                                           
*                                                                               
NOSPTER  LAY   R1,NOSPTMS                                                       
         LA    R2,TRAMEDH                                                       
         B     COMERR                                                           
*                                                                               
PERERR   LAY   R1,PERERMSG                                                      
         LA    R2,TRAPERH                                                       
         B     COMERR                                                           
*                                                                               
OVERR    LAY   R1,OVERMSG                                                       
         LA    R2,CONWHENH                                                      
         B     COMERR                                                           
*                                                                               
PRDLNERR LAY   R1,PLNERMSG                                                      
         LA    R2,TRAPRDH                                                       
         B     COMERR                                                           
*                                                                               
ESTCPYER LAY   R1,ESTCPYMS                                                      
         LA    R2,TRAESTH                                                       
         B     COMERR                                                           
*                                                                               
MISESTER LAY   R1,MISESTMS                                                      
         LA    R2,TRAESTH                                                       
         B     COMERR                                                           
*                                                                               
NOUPATER LAY   R1,NOUPATMS                                                      
         LA    R2,TRAPERH                                                       
         B     COMERR                                                           
*                                                                               
COMERR   LR    R3,R1                                                            
         BAS   RE,RELSTR                                                        
         LR    R1,R3                                                            
         XC    CONHEAD,CONHEAD                                                  
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,COMERMVC                                                      
         B     *+8                                                              
ERREXIT  BAS   RE,RELSTR                                                        
         GOTO1 ERREX2                                                           
COMERMVC MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
SPOTS    DS   0CL5                                                              
SPOTSRD  DC    C'SPOTS READ'                                                    
         LTORG                                                                  
         EJECT                                                                  
HEADINGS SSPEC H1,2,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'S P O T   C O M M E R C I A L   S E E D'                 
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,2,C'MEDIA'                                                    
         SSPEC H2,35,C'---------------------------------------'                 
         SSPEC H2,80,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H6,2,C'ESTIMATE'                                                 
         SSPEC H4,80,RUN                                                        
         SSPEC H5,80,PAGE                                                       
         SSPEC H5,90,REQUESTOR                                                  
         SSPEC H4,40,C'PERIOD'                                                  
         SSPEC H8,1,C'MARKET STATION  DATE  LIN DAY'                            
         SSPEC H9,1,C'------ -------  ----- --- -------'                        
         SSPEC H8,36,C'TIME        PROGRAM    D PRD/SLN'                        
         SSPEC H9,36,C'----------- ---------- - -------'                        
         SSPEC H8,69,C'COMMERCIAL   PTR/SLN COMMERCIAL   PREF'                  
         SSPEC H9,69,C'------------ ------- ------------ ----'                  
         DC    X'00'                                                            
         EJECT                                                                  
ALPHATAB DC    C'ABCDEFGHIJKL'                                                  
CMLOPTMS DC    C'TOTAL/SAVE/RECML - ONLY VALID'                                 
         DC    AL1(L'NOPBMS-1)                                                  
NOPBMS   DC    C'* ERROR * NO PIGGYBACK PRODUCT, NO COMMERCIAL NEEDED *C        
               '                                                                
         DC    AL1(L'NOPRVMS-1)                                                 
NOPRVMS  DC    C'* ERROR * MUST HAVE PREVIOUS COMML TO COPY FROM *'             
         DC    AL1(L'CMLSLNMS-1)                                                
CMLSLNMS DC    C'* ERROR * SPOTS LEN AND COMML LEN DIFFERENT *'                 
         DC    AL1(L'DLRTAGMS-1)                                                
DLRTAGMS DC    C'* ERROR * SPOTS HAVE BEEN DEALER TAGGED *'                     
         DC    AL1(L'BDCMLDTM-1)                                                
BDCMLDTM DC    C'* ERROR * SPOT OUTSIDE COMML (MMMDD/YY-MMMDD/YY) *'            
         DC    AL1(L'ESTCPYMS-1)                                                
ESTCPYMS DC    C'* ERROR * EST HAS COPY CODE *'                                 
         DC    AL1(L'MISESTMS-1)                                                
MISESTMS DC    C'* ERROR * EST NEEDED FOR COPY CDE=EST *'                       
         DC    AL1(L'SPTSIZMS-1)                                                
SPTSIZMS DC    C'* ERROR * TOO MANY SPOTS, REDO WITH SMALLER PERIOD *'          
         DC    AL1(L'NOSPTMS-1)                                                 
NOSPTMS  DC    C'* ERROR * NO SPOTS FOUND *'                                    
         DC    AL1(L'NOPSPTMS-1)                                                
NOPSPTMS DC    C'* ERROR * NO SPOTS (TRUE-POL) FOUND *'                         
         DC    AL1(L'SAVEDMSG-1)                                                
SAVEDMSG DC    C'CML ASSIGNS SAVED, ENTER NEXT REQUEST'                         
         DC    AL1(L'PERERMSG-1)                                                
PERERMSG DC    C'* ERROR * MUST ENTER PERIOD TO PROCESS ALL ESTIMATE'           
         DC    AL1(L'OVERMSG-1)                                                 
OVERMSG  DC    C'* ERROR * MUST RUN OVERNIGHT'                                  
         DC    AL1(L'PLNERMSG-1)                                                
PLNERMSG DC    C'* ERROR * MUST ENTER SPOT LENGTH TO PROCESS ALL PRDS'          
         DC    AL1(L'NOUPATMS-1)                                                
NOUPATMS DC    C'* ERROR * NO PATTERNS APPLY TO STATION IN PERIOD *'            
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
*=============================================================                  
* GET PROFILE REC(S)                                                            
*=============================================================                  
                                                                                
RDPROF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* READ TZ PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'Z'                                                      
         GOTO1 (RF),(R1),,SVTZPROF                                              
*                                                                               
* READ TB PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'B'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVTBPR04,ELEM+3                                                  
*MNMB                                                                           
* READ T3 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PR06,ELEM+5                                                  
*MNMB                                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* SUBROUTINE TO PROCESS PATTERN LIST DATA AND BUILD A TABLE *                   
* OF PATTERNS THAT APPLY TO EACH DAY IN THE TELECAST PERIOD *                   
* WORK    = START OF PERIOD, WORK+6  = END OF PERIOD DATE   *                   
* WORK+12 = PATTERN START,   WORK+18 = PATTERN END DATE     *                   
* WORK+24 = EFFECTIVE PATTN STR, WORK+30 = EFF PAT END      *                   
* AFTER EACH PATTERN IS SET FOR USE, IT IS SEEDED TO SPOTS  *                   
*************************************************************                   
*                                                                               
BLDDATE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,APTNTABL                                                      
         USING PTNLISTD,R5                                                      
         XC    DPTLIST,DPTLIST                                                  
         MVI   DPTINDEX,0                                                       
*                                                                               
         CLI   0(R5),0             TEST FOR NO PATTERN                          
         BNE   BLDDT02                                                          
                                                                                
* FLAG ALL SPOTS WITH NO PATTERN                                                
                                                                                
         L     R3,ACURRSPT                                                      
         MVC   SPTWORK(8),SPTPROD  SAVE PRD/SLN, PRD2/SLN2                      
*                                                                               
BLDDT00  CLI   0(R3),0             END OF SPOT TABLE                            
         BE    BLDDTX                                                           
         OI    SPTFLAG,SPTNOPT     SET ON NO PATTERN                            
*                                                                               
         LA    R3,SPTNEXT                                                       
         CLC   SPTWORK(8),SPTPROD  SAME PRD/SLN, PRD2/SLN2                      
         BE    BLDDT00              YES                                         
         B     BLDDTX                                                           
*                                                                               
BLDDT02  MVI   BYTE,0                                                           
         NI    LOG,X'FF'-LOGASGND  INIT SPOT ASSIGNED FLAG                      
*                                                                               
         CLI   SVPROF+10,C'E'      COPYCODE=EST                                 
         BE    BLDDT05                                                          
*                                                                               
         CLI   SVPROF+10,C'D'      COPYCODE = DAYPART ?                         
         BE    *+12                                                             
         CLI   SVPROF+10,C'A'      COPYCODE= PROG ADJ CODE?                     
         BNE   BLDDT06                                                          
*                                                                               
         L     R1,ADPCTABL         COPYCODE LIST                                
         ST    R1,ANEXTCC          SAVE ADDRESS OF NEXT COPYCODE                
         B     BLDDT04                                                          
*                                                                               
BLDDT03  L     R1,ANEXTCC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ANEXTCC                                                       
*                                                                               
BLDDT04  L     RF,ADPCTEND                                                      
         CR    RF,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             DONE ALL                                     
         BE    BLDDT100            YES                                          
         MVC   BYTE,0(R1)          SAVE COPY CODE                               
         B     BLDDT06                                                          
*                                                                               
BLDDT05  BAS   RE,BLDDPTL          BUILD DPTLIST                                
*                                                                               
BLDDT06  L     R0,ADTLIST                                                       
         L     R1,ADTLISTX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ADTLIST                                                       
         MVI   ELCODE,14                                                        
         CLI   BEST,0              ANY COPY EXPECTED                            
         BE    BLDDT10              NO                                          
         MVI   ELCODE,28                                                        
*                                                                               
BLDDT10  CLI   PTNLTYPE,0          TEST FOR EOL                                 
         BE    BLDDT30                                                          
*                                                                               
         CLC   PTNLTYPE,ELCODE     ELSE MATCH CODES                             
         BNE   BLDDT16                                                          
*                                                                               
         CLI   BYTE,0              ANY COPY CODE?                               
         BE    *+14                                                             
         CLC   PTNLCODE,BYTE                                                    
         BNE   BLDDT16                                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS BY EST ACTIVE                      
         BE    BLDDT12             NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLC   PTNLDPT,0(RE)       MATCH DPT                                    
         BNE   BLDDT16                                                          
                                                                                
* DETERMINE LIMITS OF PATTERN DATES VS TLCST DATES *                            
                                                                                
BLDDT12  LA    R0,PTNLSTR          POINT TO PATTERN START                       
         CLC   PERSTP,PTNLSTR      PERIOD START TO PATTERN START                
         BL    *+8                  LOW - USE PATTERN START DATE                
         LA    R0,PERSTP           ELSE POINT TO FIRST TLCST DATE               
         GOTO1 DATCON,DMCB,(2,(R0)),WORK+12                                     
*                                                                               
         LA    R0,PTNLEND                                                       
         CLC   PERENDP,PTNLEND     PERIOD END TO PATTERN END                    
         BH    *+8                                                              
         LA    R0,PERENDP                                                       
         GOTO1 (RF),(R1),(2,(R0)),WORK+18                                       
                                                                                
* GET DISPLACEMENT TO FIRST DATE IN DATELIST (FROM SVPERST) *                   
                                                                                
         GOTO1 PERVERT,(R1),PERSTDT,WORK+12                                     
         LH    R0,8(R1)            GIVES INCLUSIVE DAYS                         
         BCTR  R0,0                                                             
         SLL   R0,3                X 8                                          
         L     R6,ADTLIST                                                       
         AR    R6,R0               POINT TO FIRST DATE                          
         LLC   R0,DPTINDEX                                                      
         AR    R6,R0               POINT TO CORRECT SLOT                        
*                                                                               
BLDDT14  MVC   0(1,R6),PTNLSEQ     MOVE PATTERN SEQUENCE                        
         LA    R6,L'DPTLIST(R6)                                                 
         OI    LOG,LOGASGND        SPOT ASSIGNED                                
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+12,WORK+12,1                                     
         CLC   WORK+12(6),WORK+18  TEST REACHED END LIMIT                       
         BNH   BLDDT14                                                          
*                                                                               
BLDDT16  LA    R5,L'PTNLIST(,R5)   NEXT PATTERN LIST ENTRY                      
         B     BLDDT10                                                          
         EJECT                                                                  
*===============================================================                
* IF DAYPARTS ACTIVE, UPDATE DPTINDX AND PROCESS AGAIN                          
* WHEN THAT'S COMPLETE,                                                         
* DECREMENT VALUE IN ELCODE AND RE-PROCESS PATTERN LIST                         
*===============================================================                
                                                                                
BLDDT30  CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    BLDDT30X            NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  MAX 8 DPTS                                   
         BE    BLDDT30X                                                         
         LA    RE,DPTLIST(RE)      POINT TO NEXT SLOT IN LIST                   
         CLI   0(RE),0                                                          
         BE    BLDDT30X            NO MORE DPTS                                 
         L     R5,APTNTABL         POINT TO FIRST PATTERN                       
         B     BLDDT10                                                          
*                                                                               
BLDDT30X MVI   DPTINDEX,0          RESET POINTERS                               
         L     R5,APTNTABL                                                      
         L     R6,ADTLIST                                                       
         LLC   R0,ELCODE                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,ELCODE                                                        
         LTR   R0,R0                                                            
         BP    BLDDT10                                                          
                                                                                
* ALL LIST ELEMENTS PROCESSED - MAY NOT HAVE *                                  
* PATTERN FOR EVERY DAY IN TELECAST PERIOD   *                                  
                                                                                
         GOTO1 PERVERT,DMCB,PERSTDT,PEREDDT                                     
         LH    R0,8(R1)            NUMBER OF DAYS IN TELECAST PERIOD            
         CHI   R0,371                                                           
         BH    DATSPRER            DATE SPREAD ERROR                            
*                                                                               
         L     RE,ADTLIST                                                       
         MHI   R0,L'DPTLIST                                                     
         AR    RE,R0                                                            
         MVI   0(RE),X'FF'               SET EOL FLAGS FOR EACH DPT             
         MVC   1(L'DPTLIST-1,RE),0(RE)                                          
*                                                                               
         L     R6,ADTLIST          START OF LIST                                
         LH    R0,8(R1)            DAYS IN TLCST PERIOD                         
*                                                                               
BLDDT34  OC    0(L'DPTLIST,R6),0(R6)   TEST NON-ZERO ENTRY                      
         BNZ   BLDDT40                                                          
*                                                                               
BLDDT34X LA    R6,L'DPTLIST(R6)                                                 
         CLI   0(R6),X'FF'         TEST EOL                                     
         BE    BLDDT36                                                          
         BCT   R0,BLDDT34                                                       
*                                                                               
BLDDT36  CLI   BYTE,0              DOING COPY CODES?                            
         BNE   BLDDT03             YES- PROCESS NEXT                            
         J     NOUPATER                                                         
         EJECT                                                                  
*==============================================================                 
* NOW BUILD DATES FOR THIS PATTERN AND SEED COMMLS INTO SPOTS *                 
*==============================================================                 
                                                                                
BLDDT40  ST    R6,AFPD             ADDRESS OF FIRST PATTERN DATE                
         STH   R0,AFPDCNT          SAVE REMAINING DAY COUNT                     
*                                                                               
BLDDT42  LLC   RE,DPTINDEX                                                      
         AR    RE,R6                                                            
         CLC   0(1,RE),L'DPTLIST(RE)  SAME PATTERN FOR NEXT DAY                 
         BNE   BLDDT44                                                          
         LA    R6,L'DPTLIST(R6)                                                 
         BCT   R0,BLDDT42                                                       
         DC    H'0'                                                             
*                                                                               
BLDDT44  STH   R0,ALPDCNT          SAVE REMAINING COUNT                         
         ST    R6,ALPD             ADDRESS OF LAST PATTERN DATE                 
*                                                                               
         CLI   0(R6),0             THIS A TBA                                   
         BE    BLDDT66              YES, BYPASS                                 
                                                                                
* POINT TO PROPER PATTERN *                                                     
                                                                                
         LLC   R5,0(R6)                                                         
         BCTR  R5,0                                                             
         MHI   R5,PTNLNEXT-PTNLIST                                              
         A     R5,APTNTABL                                                      
                                                                                
* SET PATTERN DATES TO THOSE USED HERE *                                        
                                                                                
         L     RF,AFPD                                                          
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0               GIVES NUMBER OF DAYS IN R1                   
         LR    R0,RF                                                            
         GOTO1 ADDAY,DMCB,PERSTDT,WORK+24,(R0)                                  
*                                                                               
         L     RF,ALPD                                                          
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0                                                            
         LR    R0,RF                                                            
         GOTO1 ADDAY,(R1),,WORK+30,(R0)                                         
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK+24),(2,PTNLSTR)                              
         GOTO1 (RF),(R1),(0,WORK+30),(2,PTNLEND)                                
*                                                                               
         MVC   KEY+14(4),PTNLDSKA                                               
         MVI   RDUPDATE,C'N'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        COMMERCIAL LIST ELEMENT                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GCSQ             GET CML SEQ #S FOR EACH CML                  
*                                                                               
         MVI   ELCODE,X'32'        ROTATION LIST ELEMENT                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R6)                                                         
         LA    R1,2(R6)                                                         
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         STC   R0,ELEM                                                          
         LA    RE,ELEM+1                                                        
*                                                                               
BLDDT54  ICM   RF,1,0(R1)                                                       
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         CLI   0(R1),C'J'                                                       
         BL    *+8                                                              
         LA    RF,9(RF)           J=10, K=11                                    
         BCTR  RF,0                                                             
         STC   RF,0(RE)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLDDT54                                                       
*                                                                               
         L     R3,ACURRSPT                                                      
         MVC   SPTWORK,SPTPROD     SAVE CURR ENTRY                              
         USING SPTABLED,R3                                                      
*                                                                               
BLDDT60  LLC   R0,ELEM             ROTATION LENGTH                              
         LA    R1,ELEM+1                                                        
*                                                                               
BLDDT62  CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   BLDDT64                                                          
*                                                                               
         OC    SPTCMLSQ,SPTCMLSQ   WAS THIS ASSIGNED BEFORE                     
         BZ    BLDDT64              LEAVE IT ALONE                              
*                                                                               
         LA    R3,SPTNEXT                                                       
         CLC   SPTWORK(8),SPTPROD  SAME PRD/SLN, PRD2/SLN2                      
         BNE   BLDDT76              NO                                          
         CLI   0(R3),0             END OF SPOT TABLE                            
         BE    BLDDT76              YES                                         
         B     BLDDT62                                                          
*                                                                               
BLDDT64  DS    0H                                                               
         CLC   SPTFTD,PTNLEND      FIRST TELECAST DATE TO PAT END               
         BH    BLDDT70              NO MATCH                                    
         CLC   SPTLTD,PTNLSTR      LAST TELECAST DATE TO PAT START              
         BL    BLDDT70              NO                                          
*                                                                               
BLDDT66  CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BNE   *+14                                                             
         CLC   PTNLCODE,SPTDPT                                                  
         BNE   BLDDT70              NO                                          
*                                                                               
         CLI   SVPROF+10,C'A'      PROG ADJ CODE?                               
         BNE   *+14                                                             
         CLC   PTNLCODE,SPTPROGT                                                
         BNE   BLDDT70              NO                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS FOR COPYCODE=EST                   
         BE    BLDDT68             NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),X'FF'         TEST ALL DPT ENTRY                           
         BE    BLDDT68                                                          
         CLC   SPTDPT,0(RE)        ELSE MATCH DAYPART                           
         BNE   BLDDT70                                                          
*                                                                               
BLDDT68  LLC   RE,0(R1)            ROT POINTER                                  
         SLL   RE,2                TIMES 4                                      
         LA    RF,WORK(RE)         THIS COMML                                   
*                                                                               
         OC    0(4,RF),0(RF)       WERE THESE CMLS DELETED                      
         BZ    BLDDT74              YES                                         
*                                                                               
         MVC   SPTCMLSQ(4),0(RF)   MOVE IN COMML SEQ #'S                        
         SR    RE,RE                                                            
         ICM   RE,7,PTNLREF                                                     
         SRL   RE,10               DROP SUBLINE                                 
         X     RE,=XL4'00003FFF'    CONVERT TO POSITIVE NUMBERS                 
         STCM  RE,3,SPTPREF                                                     
*                                                                               
         AP    SPTUPCT,=P'1'                                                    
*                                                                               
BLDDT70  LA    R3,SPTNEXT                                                       
         CLC   SPTWORK(8),SPTPROD  SAME PRD/SLN, PRD2/SLN2                      
         BNE   BLDDT76              NO                                          
         CLI   0(R3),0             END OF SPOT TABLE                            
         BE    BLDDT76              YES                                         
*                                                                               
BLDDT74  LA    R1,1(R1)                                                         
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNZ   BLDDT62             USE NEXT ROTATION SLOT                       
         B     BLDDT60             RESET TO FIRST ROTATION SLOT                 
*                                                                               
BLDDT76  CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BE    BLDDT03              YES, GO PROCESS NEXT DPT                    
         CLI   SVPROF+10,C'A'      PROG ADJ CODE?                               
         BE    BLDDT03              YES GO PROCESS NEXT ADJ COD                 
*                                                                               
         CLI   DPTLIST,0           DPTS FOR COPYCODE=EST                        
         BE    BLDDT80             NO                                           
*                                                                               
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  TEST REACHED MAX                             
         BE    BLDDT80                                                          
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),0             TEST ANY MORE IN LIST                        
         BE    BLDDT80                                                          
*                                                                               
         L     R6,AFPD                                                          
         LH    R0,AFPDCNT          RESTORE POINTERS                             
         B     BLDDT42                                                          
*                                                                               
BLDDT80  L     R6,ALPD                                                          
         LH    R0,ALPDCNT                                                       
*                                                                               
         LA    R6,L'DPTLIST(R6)                                                 
         CLI   0(R6),X'FF'         TEST END OF DATE TABLE                       
         BE    BLDDT100            YES                                          
         ST    R6,AFPD             SET FIRST PATTERN DATE ADDRESS               
         BCTR  R0,0                DECREMENT COUNT                              
         MVI   DPTINDEX,0          RESET INDEX, STUPIDO!                        
         B     BLDDT42                                                          
*                                                                               
BLDDT100 DS    0H                                                               
         CLI   BYTE,0              DOING COPY CODES?                            
         BE    *+16                 NO                                          
         TM    LOG,LOGASGND        AT LEAST ONE ASSIGNED                        
         BO    *+8                                                              
         OI    SPTFLAG,SPTNOPT     SET ON NO PATTERN                            
*                                                                               
BLDDTX   ST    R3,ACURRSPT                                                      
         XIT1                                                                   
*                                                                               
DATSPRER LA    R1,DATSPRMS                                                      
         XC    CONHEAD,CONHEAD                                                  
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD(0),1(R1)                                                 
         J     ERREXIT                                                          
*                                                                               
         DC    AL1(L'DATSPRMS-1)                                                
DATSPRMS DC    C'* ERROR * MAX 371 DAYS IN PERIOD COVERED *'                    
*                                                                               
         DROP  R3                                                               
*=================================================================              
* BUILD A LIST OF ALL THE DAYPARTS WITH PATTERNS                                
*=================================================================              
                                                                                
BLDDPTL  NTR1                                                                   
*                                                                               
         L     R5,APTNTABL                                                      
         USING PTNLISTD,R5                                                      
*                                                                               
BLDDPT1  LA    RE,DPTLIST                                                       
         LA    RF,L'DPTLIST        FOUND IN PATTERNS                            
*                                                                               
BLDDPT2  CLI   PTNLDPT,X'FF'       TEST THIS IS ALL DPT ENTRY                   
         BE    BLDDPT6             YES - MAKE SURE IT'S FIRST IN LIST           
         CLC   0(1,RE),PTNLDPT     MATCH DPT                                    
         BE    BLDDPT4                                                          
         CLI   0(RE),0             TEST EOL                                     
         BE    BLDDPT4                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,BLDDPT2                                                       
         DC    H'0'                TOO MANY DIFFERENT DPTS                      
*                                                                               
BLDDPT4  MVC   0(1,RE),PTNLDPT                                                  
         B     BLDDPT8                                                          
*                                                                               
BLDDPT6  CLI   DPTLIST,X'FF'       TEST DPT=0 ENTRY YET                         
         BE    BLDDPT8             YES - HAVE ONE                               
                                                                                
* MOVE ENTRIES DOWN 1 SO ALL DPT ENTRY IS FIRST                                 
                                                                                
         LM    R0,R1,DPTLIST                                                    
         MVI   DPTLIST,X'FF'                                                    
         STCM  R0,15,DPTLIST+1                                                  
         STCM  R1,14,DPTLIST+5                                                  
*                                                                               
BLDDPT8  LA    R5,L'PTNLIST(R5)                                                 
         CLI   0(R5),0                                                          
         BNE   BLDDPT1                                                          
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* BUILD ESTIMATE TABLE                                                          
*===========================================================                    
                                                                                
BLDEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AESTTABL         CLEAR ESTIMATE TABLE AREA                    
         L     R1,AESTTABX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVESTNXT,SVESTNXT   CLEAR NEXT ESTIMATE ADDRESS                  
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
* READ ESTIMATE RECORD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
*                                                                               
         TM    SEEDTYPE,PRDALL     PRD=ALL ?                                    
         BO    *+10                 YES                                         
         MVC   KEY+4(3),QPRD                                                    
*                                                                               
BLDEST01 MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
BLDEST02 TM    SEEDTYPE,PRDALL     PRD=ALL                                      
         BZ    BLDEST05             NO, BRAND SPECIFIC                          
         CLC   KEY(4),KEYSAVE      SAME A/M/CLT?                                
         BNE   BLDESTX             DONE                                         
         B     BLDEST06                                                         
*                                                                               
BLDEST05 CLC   KEY(7),KEYSAVE      SAME A/M/CLT/PRD?                            
         BNE   BLDESTX              NO                                          
*                                                                               
BLDEST06 DS    0H                                                               
         CLI   KEY+7,0             PRD RECORD?                                  
         BNE   BLDEST08                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     BLDEST02                                                         
*                                                                               
BLDEST08 OC    KEY+8(5),KEY+8      BILL?                                        
         BZ    BLDEST10            NO, ESTIMATE                                 
*                                                                               
         MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         B     BLDEST01                                                         
*                                                                               
BLDEST10 DS    0H                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   ESTERR                                                           
*                                                                               
BLDEST22 CLC   ESTART,SVQEND       ESTIMATE START>PERIOD END                    
         BH    BLDEST50             YES, BYPASS                                 
         CLC   EEND,SVQSTART       ESTIMATE END>=PERIOD START                   
         BL    BLDEST50             YES, BYPASS                                 
                                                                                
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
                                                                                
BLDEST26 GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
*                                                                               
         GOTO1 (RF),(R1),(3,SVPERST),PERSTDT                                    
         GOTO1 (RF),(R1),(3,SVPEREND),PEREDDT                                   
*                                                                               
         MVC   WORK+47(4),KEY+4    PRODUCT/ESTIMATE                             
         MVC   WORK+51(12),ESTART  MOVE ESTIMATE START/END DATE                 
*                                                                               
         L     R5,AESTTABL         START OF ESTIMATE TABLE                      
         USING ESTTBLED,R5                                                      
*                                                                               
BLDEST30 DS    0H                                                               
         OC    ESTDATA,ESTDATA     ANY ENTRY?                                   
         BZ    BLDEST40             NO                                          
         CLC   ESTDATA,WORK+50     ALREADY IN THE TABLE?                        
         BE    BLDEST50             YES                                         
         LA    R5,ESTNEXT                                                       
         C     R5,AESTTABX         END OF TABLE?                                
         BL    BLDEST30                                                         
         DC    H'0'                MAKE ESTIMATE TABLE BIGGER                   
*                                                                               
BLDEST40 DS    0H                                                               
         MVC   ESTPROD,KEY+4       SAVE PRODUCT                                 
         MVC   ESTNUM,KEY+7        SAVE EST NUMBER                              
         GOTO1 DATCON,DMCB,(0,WORK+51),(3,ESTSTART) SAVE START                  
         GOTO1 (RF),(R1),(0,WORK+57),(3,ESTEND)     AND END DATES               
*                                                                               
BLDEST50 MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 GET NEXT ESTIMATE RECORD                     
         B     BLDEST02                                                         
*                                                                               
BLDESTX  GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         XIT1                                                                   
*                                                                               
ESTERR   LAY   R1,ESTCPYMS                                                      
         XC    CONHEAD,CONHEAD                                                  
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD(0),1(R1)                                                 
         J     ERREXIT                                                          
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ ALL COMMLS IN A PATTERN, AND SAVE THEIR SEQ #'S IN WORK                  
*===================================================================            
                                                                                
GCSQ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R2,2(R6)                                                         
         LLC   R3,1(R6)                                                         
         SRL   R3,4           DIVIDE BY 16-AND DROPS 2 FOR ELEM ID/LEN          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
*                                                                               
GCSQ10   CLC   =XL2'5C00',0(R2)   DELETED COMML?                                
         BE    GCSQ20                                                           
         CLC   =XL2'5C00',8(R2)   DELETED COMML?                                
         BE    GCSQ20                                                           
*                                                                               
         MVC   CMLKID,=X'0AC1'                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+10                                                             
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R2)                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BO    GCSQ20                                                           
*                                                                               
         MVC   0(2,R5),CMLSEQ+1                                                 
         MVC   MYCMLSEQ,CMLSEQ+1                                                
         L     RE,AIO                                                           
         MVC   MYCMLCML,CMLKCML-CMLKEY(RE)  MOVE CMML FROM REC                  
         MVC   MYCMLCML+8(4),SPACES                                             
*                                                                               
         MVI   ELCODE,X'A0'        LOOK FOR ADID EL                             
         BRAS  RE,NEXTEL                                                        
         BNE   GCSQ12                                                           
         USING CMLADIEL,R6                                                      
         MVC   MYCMLCML(12),CMLADID                                             
         DROP  R6                                                               
*                                                                               
GCSQ12   BAS   RE,SCML             SAVE CML SEQ/CODE                            
*                                                                               
         OC    8(8,R2),8(R2)       IS THERE A P/B COMML                         
         BZ    GCSQ30               NO                                          
         MVC   CMLKCML,8(R2)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BO    GCSQ20                                                           
*                                                                               
         MVC   2(2,R5),CMLSEQ+1                                                 
         MVC   MYCMLSEQ,CMLSEQ+1                                                
         L     RE,AIO                                                           
         MVC   MYCMLCML,CMLKCML-CMLKEY(RE)                                      
         MVC   MYCMLCML+8(4),SPACES                                             
*                                                                               
         MVI   ELCODE,X'A0'        LOOK FOR ADID EL                             
         BRAS  RE,NEXTEL                                                        
         BNE   GCSQ15                                                           
         USING CMLADIEL,R6                                                      
         MVC   MYCMLCML(12),CMLADID                                             
         DROP  R6                                                               
*                                                                               
GCSQ15   BAS   RE,SCML             SAVE CML SEQ/CODE                            
         B     GCSQ30                                                           
*                                                                               
GCSQ20   XC    0(4,R5),0(R5)                                                    
*                                                                               
GCSQ30   LA    R2,16(R2)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,GCSQ10                                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    WORK,WORK           WAS ANYTHING FOUND                           
         BNZ   GCSQX                                                            
*                                                                               
         L     R4,AIO1                                                          
         USING PATRECD,R4                                                       
*                                                                               
         MVC   CONHEAD(L'NOCMLER),NOCMLER                                       
         MVC   CONHEAD+14(38),SPACES                                            
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
GCSQ40   DS    0H                                                               
         CLC   PATKPRD,3(R1)                                                    
         BE    GCSQ44                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '          END OF TABLE                                 
         BNH   *+8                                                              
         BCT   R0,GCSQ40                                                        
         DC    H'0'                                                             
*                                                                               
GCSQ44   DS    0H                                                               
         MVC   CONHEAD+14(3),0(R1)                                              
         MVI   CONHEAD+17,C'/'                                                  
         LLC   R0,PATKSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+18(3),DUB+6(2)                                           
*                                                                               
         LA    R5,CONHEAD+22                                                    
         CLI   PATKPRD2,0                                                       
         BE    GCSQ60                                                           
*                                                                               
         LA    R0,220                                                           
         L     R1,ASVCLIST                                                      
GCSQ50   DS    0H                                                               
         CLC   PATKPRD2,3(R1)                                                   
         BE    GCSQ54                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '          END OF TABLE                                 
         BNH   *+8                                                              
         BCT   R0,GCSQ50                                                        
         DC    H'0'                                                             
*                                                                               
GCSQ54   DS    0H                                                               
         MVI   CONHEAD+21,C'/'                                                  
         MVC   CONHEAD+22(3),0(R1)                                              
         MVI   CONHEAD+25,C'/'                                                  
         LLC   R0,PATKSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+26(3),DUB+6(2)                                           
         LA    R5,CONHEAD+30                                                    
*                                                                               
GCSQ60   DS    0H                                                               
         MVC   0(2,R5),=C'R='                                                   
         SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         SRL   R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(4,R5),DUB                                                      
         LA    R5,7(,R5)                                                        
*                                                                               
         CLI   PATKCODE,0                                                       
         BE    GCSQ64                                                           
*                                                                               
         TM    PATSTAT,X'10'       THIS AN ESTIMATE                             
         BO    GCSQ64                                                           
*                                                                               
         MVC   0(3,R5),=C'CD='                                                  
         MVC   3(1,R5),PATKCODE                                                 
         LA    R5,5(,R5)                                                        
         B     GCSQ66                                                           
GCSQ64   DS    0H                                                               
         MVC   0(3,R5),=C'ES='                                                  
         LLC   R0,PATKCODE                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(3,R5),DUB                                                      
         LA    R5,7(,R5)                                                        
GCSQ66   DS    0H                                                               
         MVC   0(2,R5),=C'C='                                                   
         MVC   2(8,R5),KEY+5       MOVE IN CMML CODE                            
         LA    R2,TRAPRDH                                                       
         J     ERREXIT                                                          
GCSQX    XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* ON ENTRY MYCMLSEQ HAS SEQNUM                                                  
*          MYCMLCML HAS 12 BYTE CMML                                            
*=============================================================                  
                                                                                
SCML     NTR1                                                                   
*                                                                               
         L     R4,ACMLTAB                                                       
         L     R5,ACMLTABX         END                                          
         USING CMLTABD,R4                                                       
*                                                                               
SCML10   OC    CMLTSEQ,CMLTSEQ     EMPTY ENTRY                                  
         BZ    SCML20                                                           
         CLC   CMLTSEQ,MYCMLSEQ                                                 
         BE    SCML20                                                           
         LA    R4,L'CMLTDATA(R4)                                                
         CR    R4,R5                                                            
         BL    SCML10                                                           
         DC    H'0'                                                             
*                                                                               
SCML20   MVC   CMLTSEQ,MYCMLSEQ                                                 
         MVC   CMLTCML,MYCMLCML      MOVE EBCDIC CMML                           
*                                                                               
SCMLX    J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
NOCMLER  DC    C'* ERROR * PAT=PRD/SLN/PTR/SLN REF=0000 CML=XXXXXXXX *'         
         EJECT                                                                  
*=================================================================              
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD                  
* ON ENTRY R5 POINTS TO NEXT PATTERN LIST SLOT AND PATSEQ                       
*                                  HAS NEXT SEQUENCE NUMBER                     
*=================================================================              
                                                                                
BLDLIST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(L'PTNLIST+1,R5),0(R5)    CLEAR 1 ENTRY + 1 BYTE                
         USING PTNLISTD,R5                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         CLC   SVPEREND,PATSTART   LAST TLCST BEFORE PATTERN START              
         BL    BLDLX                                                            
         CLC   SVPERST,PATEND      FIRST TLCST AFTER PATTERN END                
         BH    BLDLX                                                            
         MVI   ELEM+6,0                                                         
         CLI   1(R6),38            TEST EXTENDED ELEMENT                        
         BNH   BLDL02               NO                                          
         TM    PATSTAT,X'80'       TEST STATUS = DELETED                        
         BO    BLDLX                YES - IGNORE                                
*                                                                               
BLDL02   XC    ELEM,ELEM                                                        
         MVC   ELEM(6),PATSTART    SAVE DATES                                   
         MVC   ELEM+8(1),PATDPT                                                 
         MVC   ELEM+10(1),PATSTAT                                               
         MVC   ELEM+12(4),PATSTIM                                               
*                                                                               
* TEST X'30' ELEMENT FOR HIATUS *                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'HIATUS',2(R6)    IF HIATUS, BYPASS                            
         BE    BLDLX                                                            
*                                                                               
* TEST PATTERN APPLIES TO THIS STATION *                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            FIND LIST ELEMENT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   BLDL03                                                           
         OC    PATLST,PATLST       TEST ALL MARKET PATTERN                      
         BNZ   BLDL03                                                           
         MVI   ELCODE,14           SET ALL MKT IND                              
         B     BLDL60                                                           
*                                                                               
BLDL03   LLC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1               SET FOR BCT                                  
         LA    R6,2(R6)                                                         
*                                                                               
         CLI   0(R6),C'G'          TEST MKT GROUP PATTERN                       
         BNE   BLDL10                                                           
         MVC   WORK(18),KEY                                                     
*                                                                               
BLDL04   DS    0H                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(3),1(R6)                                                   
         MVC   KEY+11(2),SVMKT    THIS BUY MARKET                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL05                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL05                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
*                                                                               
BLDL05   MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+3(5),KEY+3      TRY NON-CLIENT SPECIFIC                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL06                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL06                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
BLDL06   LA    R6,5(,R6)                                                        
         BCT   R0,BLDL04                                                        
*                                                                               
         MVC   KEY(18),WORK                                                     
         B     BLDLX                                                            
*                                                                               
BLDL08   MVI   ELCODE,12           SET MARKET GROUP ENTRY IND                   
         MVC   KEY(18),WORK                                                     
         B     BLDL60                                                           
*                                                                               
BLDL10   CLI   0(R6),C'M'          TEST MKT PATTERN                             
         BNE   BLDL20                                                           
                                                                                
* PROCESS MARKET LIST *                                                         
                                                                                
BLDL12   CLC   SVMKT,4(R6)         MATCH MARKET CODES                           
         BE    BLDL14                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL12                                                        
         B     BLDLX                                                            
BLDL14   MVI   ELCODE,10           SET MARKET ENTRY IND                         
         B     BLDL60                                                           
                                                                                
* PROCESS AFFILIATE LIST *                                                      
                                                                                
BLDL20   CLI   0(R6),C'A'          TEST AFFILIATE LIST                          
         BNE   BLDL30                                                           
BLDL22   CLC   SVAFFIL,1(R6)                                                    
         BE    BLDL24                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL22                                                        
         B     BLDLX                                                            
BLDL24   MVI   ELCODE,8            SET AFFILIATE ENTRY IND                      
         B     BLDL60                                                           
                                                                                
* PROCESS COMBINED MARKET/AFFILIATE LIST *                                      
                                                                                
BLDL30   CLI   0(R6),C'C'          TEST COMBINED                                
         BNE   BLDL40                                                           
         LR    RE,R0                                                            
         LR    RF,R6                                                            
BLDL32   CLI   1(R6),0             THIS AN AFFILIATE                            
         BE    BLDL33               NO                                          
         CLC   SVAFFIL,1(R6)                                                    
         BE    BLDL34                                                           
BLDL33   LA    R6,5(R6)                                                         
         BCT   R0,BLDL32                                                        
         B     BLDLX                                                            
BLDL34   CLC   SVMKT,4(RF)         MATCH MARKET CODES                           
         BE    BLDL36                                                           
         LA    RF,5(,RF)                                                        
         BCT   RE,BLDL34                                                        
         B     BLDLX                                                            
BLDL36   MVI   ELCODE,6            SET COMBINED                                 
         B     BLDL60                                                           
                                                                                
* PROCESS STATION TYPE *                                                        
                                                                                
BLDL40   CLI   0(R6),C'T'          TEST STATION TYPE LIMIT                      
         BNE   BLDL50                                                           
BLDL42   CLC   SVTYPE,1(R6)                                                     
         BE    BLDL44                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL42                                                        
         B     BLDLX                                                            
BLDL44   MVI   ELCODE,4            SET STATION TYPE ENTRY IND                   
         B     BLDL60                                                           
*                                                                               
BLDL50   CLI   0(R6),C'S'          TEST STATION LIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
BLDL52   OC    1(2,R6),1(R6)       THIS A CABLE HEAD STA                        
         BNZ   BLDL53                                                           
         CLC   SVSTA,3(R6)         THIS FOR THIS STATION                        
         BNE   BLDL53C                                                          
         B     BLDL54                                                           
*                                                                               
BLDL53   CLC   QSTA,1(R6)                                                       
         BE    BLDL54                                                           
BLDL53C  LA    R6,5(,R6)                                                        
         BCT   R0,BLDL52                                                        
         B     BLDLX                                                            
BLDL54   MVI   ELCODE,2            SET STATION LIST ENTRY IND                   
                                                                                
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
                                                                                
BLDL60   CLI   OFFLINE,C'Y'                                                     
         BE    BLDL76                                                           
*                                                                               
         L     R0,APTNTABX                                                      
         AHI   R0,-(L'PTNLIST+1)                                                
         CR    R5,R0                     TEST ROOM IN LIST                      
         BH    NOPTNRM                                                          
         XC    0(L'PTNLIST+1,R5),0(R5)   CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
BLDL76   LLC   RE,PATSEQ           BUMP SEQUENCE NUMBER                         
         LA    RE,1(,RE)                                                        
         STC   RE,PATSEQ           AND SAVE                                     
         CLI   PATSEQ,X'FF'                                                     
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
*                                                                               
         MVC   PTNLSEQ,PATSEQ          MOVE SEQUENCE NUMBER                     
         MVC   PTNLTYPE,ELCODE         SET ENTRY TYPE                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,ELEM),(2,PTNLSTR)                                 
*                                                                               
         CLC   =X'FFFFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+12                                                             
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
         B     BLDL78                                                           
*                                                                               
         GOTO1 (RF),(R1),(3,ELEM+3),(2,PTNLEND)                                 
*                                                                               
BLDL78   MVC   PTNLREF,KEY+10          REF/SUBLINE                              
         MVC   PTNLDSKA,KEY+14         AND DISK ADDRESS                         
         MVC   PTNLCODE,KEY+9              COPY CODE                            
         MVC   PTNLSTIM(4),ELEM+12     MOVE START/END TIMES                     
         MVC   PTNLDPT,ELEM+8          MOVE DPT W/IN EST                        
         CLI   PTNLDPT,0                                                        
         BNE   *+8                                                              
         MVI   PTNLDPT,X'FF'                                                    
*                                                                               
         CLC   =X'FFFFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+8                                                              
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
*                                                                               
BLDL80   LA    R5,PTNLNEXT           NEXT PATTERN LIST ENTRY                    
*                                                                               
BLDLX    XIT1  REGS=(R5)                                                        
         DROP  R5                                                               
*                                                                               
NOPTNRM  DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD                               
*=============================================================                  
                                                                                
VPER     NTR1  BASE=*,LABEL=*                                                   
         XC    SVPERDTS,SVPERDTS                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYP)                                     
*                                                                               
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,SVPERST)                                    
*                                                                               
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VPER14   BRAS  RE,NEXTEL                                                        
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         J     ERREXIT                                                          
*                                                                               
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVGENEND),(4,CONHEAD+21)                            
         J     ERREXIT                                                          
*                                                                               
VPER30   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER32                                                           
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVGENEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVGENST),SVQSTART                                   
         GOTO1 (RF),(R1),(3,SVGENEND),SVQEND                                    
         MVC   SVPERDTS,SVGENDTS                                                
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER60                                                           
*                                                                               
VPER32   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
*                                                                               
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
*                                                                               
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
*                                                                               
         TM    SEEDTYPE,ESTALL                                                  
         BO    VPER60                                                           
*                                                                               
VPER40   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER50                                                           
         BAS   RE,FFLT                                                          
*                                                                               
         B     VPER60                                                           
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER50   TM    SEEDTYPE,ESTALL     EST=ALL                                      
         BZ    *+14                                                             
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BZ    ENDTERR             END DATE REQUIRED                            
*                                                                               
         CLC   SVPERST,SVGENEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVPERST,SVGENST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BNZ   VPER54                                                           
*                                                                               
         MVC   SVPEREND,SVGENEND   USE EST END DATE                             
         B     VPER56                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
*                                                                               
VPER54   CLC   SVPEREND,SVGENEND   LAST TLCST MUST BE EST END                   
         BH    ESTDTERR                                                         
*                                                                               
VPER56   GOTO1 DATCON,DMCB,(3,SVPERST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
*                                                                               
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
*                                                                               
VPER60   GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
*                                                                               
         GOTO1 (RF),(R1),(3,SVPERST),PERSTDT                                    
         GOTO1 (RF),(R1),(3,SVPEREND),PEREDDT                                   
VPERX    XIT1                                                                   
*                                                                               
ENDTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* MUST ENTER AN END DATE *'                    
         J     ERREXIT                                                          
*                                                                               
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         J     ERREXIT                                                          
*                                                                               
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         J     TRAPERR                                                          
*                                                                               
FLTELER  MVI   ERROR,NOFLTEL                                                    
         J     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
*                                                                               
FLTRECER MVI   ERROR,NOFLTREC                                                   
         J     TRAPERR                                                          
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
*                                                                               
         DS    0H                                                               
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF4                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKF6    BRAS  RE,NEXTEL                                                        
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         OC    SVPEREND,SVPEREND   TEST END DATE GIVEN                          
         BZ    CHKF10                                                           
*                                                                               
         CLC   SVPERST,FLTEND      FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   SVPEREND,FLTSTART    LAST TLCST BEFORE FLIGHT START              
         BL    CHKF6                                                            
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVPEREND,FLTEND      LAST TLCST DATE TO FLT END                  
         BH    FLTOVLER                                                         
         CLC   SVPERST,FLTSTART                                                 
         BL    FLTOVLER                                                         
         MVC   SVGENDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVPERDTS,FLTSTART   TEST FLIGHT = TLCST DATES                    
         BE    CHKF20                                                           
         MVC   SVGENDTS,SVPERDTS   FORCE FLIGHT DATES = TELECAST                
         B     CHKF20                                                           
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF10   CLC   SVPERST,FLTSTART                                                 
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVPEREND,SVGENEND                FORCE END DATE                  
         GOTO1 DATCON,DMCB,(3,SVPEREND),SVQEND  AND REQ END DATE                
*                                                                               
CHKF20   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VOPT     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         LA    R2,TRAOPTH          OPTIONS                                      
*                                                                               
         XC    OPTIONS,OPTIONS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPTX                NO                                          
*                                                                               
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
*                                                                               
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,BLOCK+64)                                
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA            NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VOPT12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VOPT14              NO, NETHER                                   
VOPT12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
*                                                                               
VOPT14   LA    RF,OPTTABLE                                                      
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    RF,L'OPTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VOPTHLP                                                          
*                                                                               
VOPTCLC  CLC   12(0,R4),0(RF)                                                   
*                                                                               
VOPTGO   L     RE,10(RF)                                                        
         A     RE,SPTRRR                                                        
         BR    RE                                                               
         EJECT                                                                  
VOPTTEST OI    OPTFLAG,OPTTEST     TEST                                         
         B     VOPT90                                                           
*                                                                               
VOPTNPRT OI    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         B     VOPT90                                                           
*                                                                               
* CONSTRUCT FLDHDR FOR VALISTA *                                                
*                                                                               
VOPTSTA  MVC   OPTMKTST,BMKTSTA   SAVE MARKET                                   
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
*                                                                               
         GOTO1 VALISTA                                                          
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          POINT TO REAL FLDHDR FOR ERROR RTN           
         CLI   ERROR,0                                                          
         BNE   TRAPERRA                                                         
*                                                                               
         OC    OPTMKTST(2),OPTMKTST   WAS MARKET ENTERED                        
         BZ    VOPT66                  NO                                       
         CLC   BMKT,OPTMKTST       IS STA IN MARKET                             
         BNE   MKTSTAER                                                         
*                                                                               
VOPT66   MVC   OPTMKTST,BMKTSTA                                                 
         XC    BMKTSTA,BMKTSTA                                                  
*                                                                               
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPT96                                                           
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPT96                                                           
*                                                                               
         TM    OPTFLAG,OPTTEST     TEST                                         
         BO    VOPT96                                                           
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VOPT96   DS    0H                                                               
         TM    OPTFLAG,OPTTEST     TEST                                         
         BZ    VOPTX                                                            
         TM    OPTFLAG,OPTNOPRT    BYPASS REPORT PRINT                          
         BO    TSTPRTER                                                         
*                                                                               
VOPTX    XIT1                                                                   
         EJECT                                                                  
TSTPRTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TSTPRTMS),TSTPRTMS                                     
         J     ERREXIT                                                          
*                                                                               
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHLPMS),OPTHLPMS                                     
         J     ERREXIT                                                          
*                                                                               
MKTSTAER BRAS  RE,RELSTR                                                        
         MVC   GERROR,=Y(WRONGMKT)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R1),5             L'SUBST TEXT + 1                             
         MVC   1(4,R1),QMKT                                                     
         LA    R1,5(R1)                                                         
         MVI   0(R1),5             L'SUBST TEXT + 1                             
         SR    R0,R0                                                            
         ICM   R0,3,OPTMKTST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(4,R1),DUB                                                      
VOPTTRAP GOTO1 VTRAERR                                                          
*                                                                               
MISSERRA MVI   ERROR,MISSING                                                    
TRAPERRA BRAS  RE,RELSTR                                                        
         GOTO1 ERREX                                                            
*                                                                               
OPTHLPMS DC    C'* ERROR * OPTIONS-TEST/STATION/NOPRT = *'                      
TSTPRTMS DC    C'* ERROR * CAN''T RUN TEST AND NOPRT TOGETHER *'                
*                                                                               
OPTTABLE DS    0CL14                                                            
         DC    CL10'TEST      ',AL4(VOPTTEST)                                   
         DC    CL10'NOPRT     ',AL4(VOPTNPRT)                                   
         DC    CL10'STATION   ',AL4(VOPTSTA)                                    
         DC    CL10'HELP      ',AL4(VOPTHLP)                                    
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* VALIDATE PATTERN REFS TO CLEAR                                                
*=============================================================                  
                                                                                
VREF     NTR1  BASE=*,LABEL=* 0,**VREF**                                        
*                                                                               
         LA    R2,TRAREFH          PATTERN REFS                                 
*                                                                               
         XC    CREFTABL,CREFTABL                                                
         LA    R5,CREFTABL                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VREFX                NO                                          
*                                                                               
         GOTO1 SCANNER,DMCB,TRAREFH,(15,BLOCK)                                  
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRB            NO                                           
*                                                                               
         CHI   R3,15                                                            
         BH    MAXREF                                                           
*                                                                               
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VREF10   CLI   0(R4),4             MUST BE 1-4 DIGITS                           
         BH    BADREF                                                           
         TM    2(R4),X'80'         MUST BE NUMERIC                              
         BZ    NUMERRB                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(2),BPRD       PRD/SLN                                      
*                                                                               
         CLI   BPRD2,255           PTR NONE                                     
         BE    *+10                                                             
         MVC   KEY+7(2),BPRD2      PRD2/SLN2                                    
*                                                                               
         MVC   KEY+9(1),BEST       ESTIMATE                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VREF20   CLC   KEY(7),KEYSAVE                                                   
         BNE   NOPATER                                                          
*                                                                               
         CLI   BPRD2,0             ANY SPECIFIC REQUEST                         
         BE    VREF30               NO                                          
*                                                                               
         CLI   BPRD2,255           PTR = NONE                                   
         BE    VREF26                                                           
*                                                                               
         CLC   KEY+7(2),BPRD2                                                   
         BNE   VREF50                                                           
         B     VREF30                                                           
*                                                                               
VREF26   OC    KEY+7(2),KEY+7      ONLY 1 PRODUCT                               
         BNZ   VREF50                                                           
*                                                                               
VREF30   CLI   BEST,0                                                           
         BE    VREF40                                                           
*                                                                               
         CLC   BEST,KEY+9                                                       
         BNE   VREF50                                                           
*                                                                               
VREF40   SR    RE,RE                                                            
         ICM   RE,7,KEY+10                                                      
         SRL   RE,10               DROP SUBLINE                                 
         X     RE,=XL4'00003FFF'    CONVERT TO POSITIVE NUMBERS                 
         CLM   RE,3,6(R4)                                                       
         BNE   VREF50                                                           
*                                                                               
         L     R6,AIO1             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         AH    R6,DATADISP                                                      
         CLI   0(R6),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         CLI   BEST,0              THIS BY ESTIMATE                             
         BNE   VREF44                                                           
*                                                                               
         TM    PATSTAT,X'10'       THIS PATTERN BY ESTIMATE                     
         BO    VREF50                                                           
         B     VREF46                                                           
*                                                                               
VREF44   TM    PATSTAT,X'10'       THIS PATTERN BY ESTIMATE                     
         BZ    VREF50                                                           
*                                                                               
VREF46   CLC   SVPEREND,PATSTART   LAST TLCST BEFORE PATTERN START              
         BL    VREF50                                                           
         CLC   SVPERST,PATEND      FIRST TLCST AFTER PATTERN END                
         BNH   VREF60                                                           
         DROP  R6                                                               
*                                                                               
VREF50   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     VREF20                                                           
*                                                                               
VREF60   MVC   0(2,R5),6(R4)                                                    
         LA    R4,32(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,VREF10                                                        
*                                                                               
VREFX    XIT1                                                                   
*                                                                               
NUMERRB  MVI   ERROR,NOTNUM                                                     
         B     VREFERX                                                          
*                                                                               
MISSERRB MVI   ERROR,MISSING                                                    
*                                                                               
VREFERX  J     TRAPERR                                                          
*                                                                               
BADREF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADREFMS),BADREFMS                                     
         B     VREFERX2                                                         
*                                                                               
MAXREF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAXREFMS),MAXREFMS                                     
         B     VREFERX2                                                         
*                                                                               
NOPATER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPATMS),NOPATMS                                       
*                                                                               
VREFERX2 J     ERREXIT                                                          
*                                                                               
BADREFMS DC    C'* ERROR * ENTER PATTERN REF - EACH 1-4 DIGITS *'               
MAXREFMS DC    C'* ERROR * MAX OF 15 PATTERN REFERENCE NUMBERS *'               
NOPATMS  DC    C'* ERROR * PATTERN NOT FOUND *'                                 
         EJECT                                                                  
*====================================================================           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON REQUESTED DATES             
*====================================================================           
                                                                                
BLEST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVESTAB,SVESTAB                                                  
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
         CLI   KEY+7,0                                                          
         BNE   *+8                                                              
         MVI   KEY+7,1                                                          
*                                                                               
BLEST10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLEST30                                                          
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLEST20                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,SVQEND       EST START AFTER REQ END                      
         BH    BLEST20                                                          
         CLC   EEND,SVQSTART       EST END BEFORE REQ START                     
         BL    BLEST20                                                          
*                                                                               
         LLC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),X'FF'                                                      
*                                                                               
BLEST20  MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         CLI   BEST,0              TEST NO ESTIMATE REQUEST                     
         BE    BLEST10              YES - CONTINUE                              
         CLC   KEY+7(1),BEST       TEST PAST END OF SERIES OR ONE               
         BL    BLEST10              NO - CONTINUE                               
*                                                                               
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
*                                                                               
BLEST30  GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         OC    SVESTAB,SVESTAB     TEST ANY DATA FOUND                          
         BZ    NEQXIT              NO - RETURN WITH CC SET                      
*                                                                               
         CLI   BEST,0              TEST EST=NO REQUEST                          
         BNE   EQXIT                                                            
*                                                                               
         LA    RE,SVESTAB                                                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         LA    R1,SVESTAB                                                       
         SR    RE,R1                                                            
         STC   RE,BEST                                                          
*                                                                               
         LA    RE,SVESTAB+255                                                   
         LA    R0,255                                                           
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,BESTEND                                                       
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     BLESTX                                                           
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
BLESTX   XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================                
* READ COMML CODES FOR ALL SPOTS                                                
*===============================================================                
                                                                                
FNDCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ASPTABLE                                                      
         LH    R4,SPOTCT                                                        
         USING SPTABLED,R3                                                      
*                                                                               
FNDCML10 DS    0H                                                               
         LA    R1,SPTCMLSQ               READ FOR COMML 1                       
         BAS   RE,CHKTAB                                                        
*                                                                               
         CLI   CMLFND,C'Y'               WAS COMML 1 FOUND?                     
         BE    FNDCML30                                                         
         XC    SPTCMLSQ,SPTCMLSQ         IF NOT CLEAR COMML 1 FIELD             
         OC    SPTPREF,SPTPREF           IS PAT REF# PRESENT?                   
         BZ    FNDCML30                  IF NO GO TO READ FOR COMML 2           
         NI    SPTFLAG,X'FF'-SPTPRVA-SPTPRVPT-SPTPRVPR    CLEAR FLAGS           
         XC    SPTPREF,SPTPREF           CLEAR REF                              
         XC    SPTCMLS2,SPTCMLS2         CLEAR COMML 2                          
         B     FNDCML50                                                         
*                                                                               
FNDCML30 DS    0H                                                               
         LA    R1,SPTCMLS2               READ FOR COMML 2                       
         BAS   RE,CHKTAB                                                        
*                                                                               
         CLI   CMLFND,C'Y'               WAS COMML 2 FOUND?                     
         BE    FNDCML50                                                         
         XC    SPTCMLS2,SPTCMLS2         IF NOT CLEAR COMML 2 FIELD             
*                                                                               
FNDCML50 LA    R3,SPTNEXT                                                       
         BCT   R4,FNDCML10                                                      
*                                                                               
FNDCMLX  XIT1                                                                   
*                                                                               
CHKTAB   NTR1                                                                   
         MVI   CMLFND,C'Y'                                                      
         OC    0(2,R1),0(R1)       IF NO SEQNUM, RETURN                         
         JZ    EXIT                                                             
*                                                                               
         L     R5,ACMLTAB                                                       
         USING CMLTABD,R5                                                       
*                                                                               
CHKTAB2  OC    0(2,R5),0(R5)       TEST FREE SLOT                               
         BZ    CHKTAB4                                                          
         CLC   0(2,R1),0(R5)                                                    
         JE    EXIT                                                             
         LA    R5,L'CMLTDATA(R5)                                                
         C     R5,ACMLTABX                                                      
         BL    CHKTAB2                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   TABSIZER                                                         
         DC    H'0'                                                             
*                                                                               
CHKTAB4  MVC   CMLTSEQ,0(R1)       ADD SEQNUM TO TABLE                          
                                                                                
         XC    KEY,KEY             NOW READ CMML RECORD                         
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM(3),BAGYMD   A-M/CLT                                       
         MVC   CMLPSEQ+1(2),CMLTSEQ                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
*MN      BE    *+6                                                              
*MN      DC    H'0'                                                             
         BE    CHKTAB8                                                          
         MVI   CMLFND,C'N'                                                      
         J     EXIT                                                             
*                                                                               
CHKTAB8  L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   CMLTCML,5(R4)      MOVE ISCI OR PACKED ADID                      
         MVC   CMLTCML+8(4),SPACES                                              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'        FIND ADID ELEMENT                            
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   CMLTCML,CMLADID-CMLADIEL(R6)                                     
*                                                                               
CHKTABX  J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
TABSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TABSIZMS),TABSIZMS                                     
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
         DC    AL1(L'TABSIZMS-1)                                                
TABSIZMS DC    C'* ERROR * TOO MANY CMMLS *'                                    
         LTORG                                                                  
         DROP  R3,R4                                                            
         EJECT                                                                  
*=============================================================                  
* GET STORAGE FOR OFFLINE                                                       
*=============================================================                  
*                                                                               
GETSTROF NTR1  BASE=*,LABEL=*                                                   
         ICM   R1,15,ASPTABLE      ANY BUFFER ACQUIRED YET                      
         JNZ   EXIT                                                             
*                                                                               
         L     R0,SPTTSIZE         WAS 4000X36                                  
         STORAGE OBTAIN,LENGTH=(R0),LOC=24,COND=YES                             
         LTR   RF,RF               ERROR?                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R0,R1               R0=LEN OF STORAGE                            
         ST    R0,ASPTEND                                                       
         MVC   0(8,R1),=C'*SPTABLE'  R1=A(STORAGE)                              
         LA    R1,8(R1)                                                         
         ST    R1,ASPTABLE                                                      
         J     EXIT                                                             
SPTTSIZE DC    F'500000'                                                        
*                                                                               
*=============================================================                  
* ALLOCATE STORAGE - OFFLINE IN VADUMMY                                         
*                    ONLINE IN 28K T2168C                                       
*=============================================================                  
                                                                                
GETSTOR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         L     R0,LSYSD                                                         
         AR    R0,R9                                                            
         LR    R1,R9                                                            
         AHI   R1,ENDSYSD-SYSD                                                  
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
*                                                                               
         LA    RE,DPCTABLE        THIS TABLE IN WORKING STORAGE                 
         ST    RE,ADPCTABL                                                      
         LA    RE,100(RE)                                                       
         ST    RE,ADPCTEND                                                      
*                                                                               
         LA    RE,DATELIST         THIS ONE TOO                                 
         ST    RE,ADTLIST                                                       
         LA    RE,3200(RE)                                                      
         ST    RE,ADTLISTX                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+16                                                             
         BRAS  RE,GETSTROF         GET STORAGE OFFLINE FOR STA TABLE            
         L     RE,VADUMMY                                                       
         B     GETSTOR2                                                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'8C'          GET ONLINE STORAGE PHASE                     
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)            GET PHASE ADDRESS                            
         LA    RE,16(RE)           CAN USE FROM +16                             
*                                                                               
GETSTOR2 CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    GETSTOR3            STORAGE IS SET FOR SPTABLE                   
         MVC   0(8,RE),=C'SPTABLE'                                              
         LA    RE,8(RE)                                                         
         ST    RE,ASPTABLE                                                      
         LA    R0,400                                                           
         MHI   R0,SPTNEXT-SPTABLED X 33 BYTES/SPOT                              
         AR    RE,R0                                                            
         ST    RE,ASPTEND                                                       
*                                                                               
GETSTOR3 MVC   0(8,RE),=C'CMLTABLE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ACMLTAB                                                       
         LHI   R0,20000                                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R0,2000                                                          
         AR    R0,R0               X 2                                          
         AR    RE,R0                                                            
         ST    RE,ACMLTABX                                                      
*                                                                               
         MVC   0(8,RE),=C'PTNTABLE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,APTNTABL                                                      
         LHI   R0,10000                                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R0,2000                                                          
         AR    RE,R0                                                            
         ST    RE,APTNTABX                                                      
*                                                                               
         MVC   0(8,RE),=C'ESTTABLE'                                             
         LA    RE,8(RE)                                                         
         ST    RE,AESTTABL         ESTIMATE TABLE (FOR EST=ALL)                 
         LA    R0,3500                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R0,990                                                           
         AR    RE,R0                                                            
         ST    RE,AESTTABX                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AQSORT,DMCB                                                      
         J     EXIT                                                             
         EJECT                                                                  
* INCLUDE SPGENCLT                                                              
* INCLUDE SPGENPRD                                                              
* INCLUDE SPGENEST                                                              
* INCLUDE SPGENSTA                                                              
* INCLUDE SPGENBUY                                                              
* INCLUDE SPTRCMML                                                              
* INCLUDE SPTRPAT                                                               
* INCLUDE SPTRFLT                                                               
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA78D                                                       
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
         DS    0D                                                               
AFPD     DS    A                   A(FIRST DATE THIS PTTN)                      
ALPD     DS    A                   A(LAST DATE THIS PTTN)                       
ANEXTCC  DS    A                   ADDRESS OF NEXT COPY CODE                    
ADTLIST  DS    A                                                                
ADTLISTX DS    A                                                                
VTRPACK  DS    A                                                                
DPTLIST  DS    CL8                                                              
DPTINDEX DS    X                                                                
         DS    X                                                                
AFPDCNT  DS    H                                                                
ALPDCNT  DS    H                                                                
*                                                                               
DATECTR  DS    H                                                                
*                                                                               
* FROM BUY ELEMENT                                                              
*                                                                               
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
ROTDAYS  DS    H                                                                
PATSEQ   DS    X'00'                                                            
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SVBSTA   DS    XL3                                                              
STAAFFL  DS    CL1                                                              
STATYPE  DS    CL1                                                              
SPOTNUMB DS    XL1                                                              
SVBUYLIN DS    XL2                                                              
SPTWORK  DS    XL(L'SPTDATA)                                                    
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
SVESTAB  DS    XL256                                                            
SPTRRR   DS    A                                                                
AQSORT   DS    A                                                                
ASPTABLE DS    A                                                                
ASPTEND  DS    A                                                                
ACURRSPT DS    A                                                                
APTNTABL DS    A                                                                
APTNTABX DS    A                                                                
ACMLTAB DS     A                                                                
ACMLTABX DS    A                                                                
ADPCTABL DS    A                                                                
ADPCTEND DS    A                                                                
AESTTABL DS    A                                                                
AESTTABX DS    A                                                                
SVESTNXT DS    A                   NEXT ESTIMATE ADDRESS                        
SVTZPROF DS   0CL16                                                             
SVTZPR01 DS    C                   FLEXIBLE DATES - SPOT GEN                    
SVTZPR02 DS    C                   KEEP PREVIOUSLY ASSIGNED CMLS                
         DS    CL14                                                             
SVTBPR04 DS    C                   TB PROFILE 4-ESTIMATES NOT REQUIRED          
*MNMB                                                                           
SVT3PR06 DS    C                                                                
*MNMB                                                                           
SVMKTSTA DS    0XL5                                                             
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
*                                                                               
SVAFFIL  DS    CL3                 AFFILIATE FROM STATION MASTER                
SVTYPE   DS    CL1                 TYPE FROM STATION MASTER                     
*                                                                               
SVPROGN  DS    CL17                                                             
SVDAYPT  DS    CL1                                                              
*                                                                               
BUYLNCT  DS    PL3                 RECORDS READ                                 
BUYUPCT  DS    PL3                 RECORDS UPDATED                              
SPTCT    DS    PL3                 SPOTS FOUND                                  
SPTASCT  DS    PL3                 SPOTS PREV ASSIGNED                          
SPTUPCT  DS    PL3                 SPOTS UPDATED                                
TBUYLNCT DS    PL3                 RECORDS UPDATES                              
TBUYUPCT DS    PL3                 RECORDS UPDATES                              
TSPTCT   DS    PL3                 SPOTS FOUND                                  
TSPTASCT DS    PL3                 SPOTS PREV ASSIGNED                          
TSPTUPCT DS    PL3                 SPOTS UPDATED                                
*                                                                               
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
*                                                                               
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
*                                                                               
* FROM FLIGHT OR ESTIMATE RECORD OR ENTERED TELECAST DATES                      
*                                                                               
PERSTP   DS    XL2                 FLIGHT/TELECAST START DATE                   
PERENDP  DS    XL2                 FLIGHT/TELECAST END DATE                     
*                                                                               
PERSTDT  DS    CL6                                                              
PEREDDT  DS    CL6                                                              
*                                                                               
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 INSTRUCTION START DATE                       
SVGENEND DS    XL3                 INSTRUCTION END DATE                         
TODAYP   DS    XL2                 TODAY PACKED - USED IN UPD RTN               
*                                                                               
MYCMLSEQ DS    XL2                                                              
MYCMLCML DS    CL12                                                             
CMLFND   DS    CL1                                                              
*                                                                               
* KEEP ALL SVPROD TO SVBSLN2 TOGETHER AND IN ORDER *                            
*                                                                               
SVPROD   DS    CL3                                                              
SVBPRD   DS    XL1                                                              
SVBSLN   DS    XL1                                                              
SVPROD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
SVBSLN2  DS    XL1                                                              
*                                                                               
SVPPRD   DS    XL1                 SAVE PREVIOUS PROD                           
SVPEST   DS    XL1                 AND ESTIMATE                                 
*                                                                               
SPOTCT   DS    H                                                                
PRDNUM   DS    H                                                                
*                                                                               
FLDH     DS    CL8                 DUMMY FIELD HEADER                           
FLD      DS    CL3                 AND ITS FIELD                                
*                                                                               
SEEDTYPE DS    XL1                                                              
FOUNDONE EQU   X'80'               AT LEAST ONE FOUND                           
ESTALL   EQU   X'40'               SEED FOR ALL ESTIMATES                       
PRDALL   EQU   X'20'               SEED FOR ALL PRODS                           
DONEALL  EQU   X'10'               DONE SEED ALL (OR NONE TO DO)                
*OENDTE  EQU   X'08'               PERIOD WITH NO END DATE                      
*                                                                               
LOG      DS    XL1                                                              
LOGNPOL  EQU   X'80'               NON-POOL BUYS READ IN BLACT RTN              
*        EQU   X'40'                                                            
LOGSPOTS EQU   X'20'               READ POL SPOTS                               
LOGUPDSW EQU   X'10'               RECORD UPDATE NEEDED                         
LOGASGND EQU   X'01'               AT LEAST ON SPOT ASSIGNED                    
*                                                                               
OPTIONS  DS    0XL7                                                             
OPTMKTST DS    XL5                                                              
OPTFLAG  DS    XL1                                                              
OPTTEST  EQU   X'80'                                                            
OPTNOPRT EQU   X'40'                                                            
*                                                                               
HOLDSIGN DS    CL1                                                              
SVBEST   DS    CL1                 SAVE BEST                                    
*                                                                               
CREFTABL DS    XL60                15 4 BYTE ENTRIES                            
*                                  2 BYTES = PAT REF                            
*                                  2 BYTES = CT OF SPOTS CLEARED                
         DS    0D                                                               
DPCTABLE DS    XL100                                                            
*                                                                               
         DS    0D                                                               
DATELIST DS    3200C                                                            
*                                                                               
ENDSYSD  EQU   *                IF THIS ADDRESS >2F08, PAST END OF SYSD         
         EJECT                                                                  
* DSECT FOR SPOT ACTIVITY LIST ENTRIES *                                        
*                                                                               
SPTABLED DSECT                                                                  
SPTDATA  DS    0XL36                                                            
SPTSORT  DS    0XL14                                                            
SPTPROD  DS    CL3                 PROD                                         
SPTSLN   DS    XL1                 SPOT LENGTH                                  
SPTPROD2 DS    CL3                 PARTNER PROD                                 
SPTSLN2  DS    XL1                 SPOT LENGTH                                  
SPTFTD   DS    XL2                 SPOT FIRST TELECAST DATE                     
SPTTIME  DS    XL4                 START/END TIMES                              
*                                                                               
SPTBPRD  DS    XL1                                                              
SPTBPRD2 DS    XL1                                                              
*                                                                               
SPTUSORT DS    0XL5                SORT FOR UPDATE RECS                         
*                                                                               
SPTDSKAD DS    XL4                 BUY REC DISK ADDR                            
SPTSPTN  DS    XL1                 SPOT NUMBER WITHIN BUY REC                   
SPTDAY   DS    XL1                 DAYS                                         
SPTDPT   DS    XL1                 DAY PART                                     
SPTEST   DS    XL1                 ESTIMATE                                     
SPTLINE  DS    XL2                 BUY LINE                                     
SPTCMLSQ DS    XL2                 CML SEQ NUMBER                               
SPTCMLS2 DS    XL2                 CML SEQ NUMBER                               
SPTFLAG  DS    XL1                 SPOT FLAGS                                   
SPTPRVA  EQU   X'80'               PREVIOUS ASSIGN FLAG                         
SPTPRVPT EQU   X'40'               SOURCE WAS PATTERN                           
SPTPRVPR EQU   X'20'               SPOT WAS PRINTED ON INSTR                    
SPTPREV  EQU   X'10'               PRODUCT/PARTNER REVERSED                     
SPTNOPT  EQU   X'08'               NO PATTERN FOUND                             
SPTPREF  DS    XL2                 PATTERN REFERENCE (WITHOUT SUBLINE)          
SPTPROGT DS    XL1                 PROGRAM ADJ CODE                             
SPTLTD   DS    XL2                 SPOT LAST TELECAST DATE                      
SPTNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR ESTIMATE TABLE ENTRIES *                                            
*                                                                               
ESTTBLED DSECT                                                                  
ESTDATA  DS    0XL10                                                            
ESTPROD  DS    CL3                 PRODUCT                                      
ESTNUM   DS    CL1                 ESTIMATE NUMBER                              
ESTSTART DS    CL3                    "     START DATE                          
ESTEND   DS    CL3                    "     END DATE                            
ESTNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR COMML TABLE ENTRIES *                                               
*                                                                               
CMLTABD  DSECT                                                                  
CMLTDATA DS   0XL14                                                             
CMLTSEQ  DS    XL2                 COMML SEQ NO                                 
CMLTCML  DS    CL12                COMMERCIAL CODE - EBCDIC                     
CMLTNEXT EQU   *                                                                
*                                                                               
PTNLISTD DSECT                                                                  
*                                                                               
PTNLIST  DS    0XL24                                                            
PTNLTYPE DS    XL1                 10=ALL MKT  8=ONE MKT  7=MKT/AFFIL           
*                                   6=AFFIL    4=STA TYPE 2=STATION             
*                                  FF=TBA (NO PATTERN)                          
PTNLSEQ  DS    XL1                 PATTERN SEQ NUM (MIN 1)                      
PTNLSTR  DS    XL2                 PATTERN START DATE                           
PTNLEND  DS    XL2                 PATTERN END DATE                             
PTNLREF  DS    XL3                 REF/SUBLINE                                  
PTNLDSKA DS    XL4                 DISK ADDRESS                                 
PTNLCODE DS    XL1                 COPY CODE                                    
PTNLFLAG DS    XL1                                                              
PTNLFUFN EQU   X'40'               PATTERN RUNS UFN                             
PTNLDPT  DS    CL1                 DPT FOR COPYCODE=EST                         
PTNLSTIM DS    XL2                 PATTERN START TIME                           
PTNLETIM DS    XL2                 PATTERN END TIME                             
         DS    CL4                 SPARE                                        
PTNLNEXT EQU   *                                                                
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PDATE    DS    CL5                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PDAY     DS    CL7                                                              
         DS    CL3                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PPROG    DS    CL10                                                             
         DS    CL1                                                              
PDP      DS    CL1                                                              
         DS    CL1                                                              
PPRD     DS    CL7                                                              
         DS    CL1                                                              
PCML     DS    CL12                                                             
         DS    CL1                                                              
PPRD2    DS    CL7                                                              
         DS    CL1                                                              
PCML2    DS    CL12                                                             
         DS    CL1                                                              
PREF     DS    CL4                                                              
         DS    CL1                                                              
PCMTS    DS    CL14                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105SPTRA2A   06/29/20'                                      
         END                                                                    
