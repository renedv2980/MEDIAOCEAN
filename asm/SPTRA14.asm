*          DATA SET SPTRA14    AT LEVEL 050 AS OF 08/30/04                      
*PHASE T21614A                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'T21614 - DEALER (SPOT) TAGGING'                                 
***********************************************************************         
*                                                                               
* LEV 24 FIX FOR HIATUS PATTERNS                                                
*        ALSO FIXED DEALER TAG DATE CHECKING TO REQUIRE FLIGHT DATE             
*        REL/RECALL DATES OR BETTER FOR TAG LIST, BUT ONLY COVER                
*        SPOT DATE FOR INDIVIDUAL SPOT.                                         
* LEV 25 MAY09/86 ???????????                                                   
* LEV 26 NOV18/86 FIX RECUP BOMB WHEN BUY GETS TOO LARGE..                      
* LEV 27    JAN08/87 ADD OFFICE PROFILE AND DELETE TC DAYPARTS PROFILE          
* LEV 28-31 JUN11/87 READD EST                                                  
* LEV 32    NOV01/88 BYPASS AFFIDAVIT ELEMS TO LOOK FOR DLR TAG ELEMS           
* LEV 33-35 NOV10/88 FIND PATTERN 1 IN FPAT RTN                                 
* LEV 36    DEC08/88 ELIMINATE SVPROF13 & 14                                    
* LEV 37    SEP13/91 FIX READ FOR UPDATE, USE QSORT, SHORTER ENTRIES            
* LEV 38    APR09/92 SET OFF FIELD VALIDATED FOR NO SPOTS ERROR MESSAGE         
* LEV 39    DEC22/92 FIX MSUNPK                                       *         
* LEV 40    MAY04/93 ADD LOCAL CABLE, MKT GRP PAT, ALLOW UNTAGGED     *         
* LEV 41    MAY28/93 FIX BUG - DEALER TAG DATES, PERIOD DATES         *         
* LEV 42    AUG18/93 FIX BUG - SPOTS WITH NO PATTERN REF              *         
* LEV 43    OCT20/93 ADD NEW TRAFFIC SYSTEM                           *         
* LEV 44    JAN21/94 ADD 15 COMMLS PER PATTERN                        *         
* LEV 45    JUL22/94 PULLED VSWIT BEFORE/AFTER CK IF SPOT UP          *         
* LEV 46    JUL22/94 PUT VSWIT BACK                                   *         
* LEV 47    NOV10/99 USE RECUP FROM FACPAK                            *         
* LEV 48 SMUR OCT05/00 COMPARE PATTERN LENGTH TO SPOT LENGTH          *         
* LEV 49 SMUR APR10/01 USE TRAFFIC OFFICE                             *         
* LEV 50 BGRI AUG02/04 SOX                                            *         
*                                                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - COMMERCIAL RECORDS                                         
*                                                                               
*             AIO3 - PATTERN LIST BUILD AREA                                    
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - SPTABLE POINTER                                                   
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
         EJECT                                                                  
T21614   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21614**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR14RR                                                      
         ST    RC,SPTR14RC                                                      
         LA    R3,TOPASVTB         START OF SAVED AREA                          
         ST    R3,ASVSTOR                                                       
         LA    R3,SPTABLE                                                       
         L     R0,LSYSD                                                         
         AR    R0,R9                                                            
         LR    R1,R9                                                            
         AH    R1,=AL2(ENDSYSD-SYSD)                                            
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         SPACE                                                                  
* FIND DCB ADDRESS AND SEE IF READ ONLY                                         
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA                         
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DTFAD',=C'SPTDIR'                                
         SPACE                                                                  
         L     RE,12(,R1)          GET ADDR OF DCB                              
         SPACE                                                                  
         TM    ISFOPEN-ISDTF(RE),ISFORO+ISFONOP  READ ONLY OR NOP               
         BNZ   WRITERR                                                          
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO SPOT TRAFFIC                  
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
EXIT     XIT1                                                                   
         SPACE                                                                  
*     TEST FOR ANY CHANGES IN KEY FIELDS                                        
         SPACE                                                                  
VK       TM    TRAMEDH+4,X'20'     MEDIA CHANGED                                
         BZ    VK10                  YES REVALIDATE                             
         TM    TRACLTH+4,X'20'     CLIENT CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPRDH+4,X'20'     PRODUCT CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAESTH+4,X'20'     ESTIMATE CHANGED                             
         BZ    VK10                  YES REVALIDATE                             
         TM    TRASTAH+4,X'20'     MARKET/STATION CHANGED                       
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPERH+4,X'20'     PERIOD CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRATAGSH+4,X'20'    TAGS CHANGED                                 
         BZ    COPT                  YES                                        
         CLC   TRATAGS(L'TAGOPTMS),TAGOPTMS                                     
         BNE   GEN10                 DISPLAY NEXT/TOP LIST SPOTS                
         GOTO1 =A(EPRS),RR=SPTR14RR  EDIT TAG PAIR(S) HERE                      
         B     GEN10                 DISPLAY NEXT/TOP LIST SPOTS                
         EJECT                                                                  
COPT     BAS   RE,RDTWA            RESTORE SAVED SPOT TABLE                     
         LA    R2,TRATAGSH                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    COPT50              NONE                                         
         TM    TABLESW,X'80'       TABLE BUILT?                                 
         BO    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
* CHECK FOR SPECIAL REQUESTS                                                    
         SPACE                                                                  
         CLC   WORK(4),=C'SAVE'  SAVE ENTERED                                   
         BE    UPDATE                                                           
         CLC   WORK(5),=C'RECML'                                                
         BE    COPT10                                                           
         CLC   WORK(7),=C'PATTERN'                                              
         BE    COPT30                                                           
         CLC   WORK(5),=C'TOTAL'                                                
         BE    COPT40                                                           
         CLC   WORK(5),=C'UNTAG'                                                
         BNE   VK62                NO, GO VALIDATE DLR TAG PAIRS                
         SPACE                                                                  
* CLEAR ALL DEALER TAGS OR COMMERCIAL ASSIGNMENTS *                             
         SPACE                                                                  
COPT10   LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         LH    R0,SPOTCT                                                        
         CLC   WORK(5),=C'RECML'                                                
         BE    COPT20                                                           
COPT12   XC    SPTTAG,SPTTAG                                                    
         LA    R3,L'SPTDATA(,R3)                                                
         BCT   R0,COPT12                                                        
         MVC   UNTAGCT,SPOTCT                                                   
         XC    SVTAGPRS,SVTAGPRS                                                
         MVI   LOG,X'08'                                                        
         B     COPT50                                                           
COPT20   XC    SPTCMLSQ,SPTCMLSQ                                                
         MVI   SPTCMLA,0                                                        
         LA    R3,L'SPTDATA(,R3)                                                
         BCT   R0,COPT20                                                        
         LA    R3,SPTABLE                                                       
         BAS   RE,ACML                                                          
         NI    TABLESW,X'FF'-X'20'                                              
         B     COPT50                                                           
         SPACE                                                                  
COPT30   OI    LOG,X'40'           PATTERN REQUEST                              
         B     COPT50                                                           
COPT40   OI    LOG,X'20'           TOTAL REQUEST                                
COPT50   OI    TRATAGSH+4,X'20'    VALIDATED                                    
         GOTO1 =A(EPRS),RR=SPTR14RR EDIT TAG PAIR(S) HERE                       
         TM    LOG,X'08'           RETURN FROM PREV REQUEST                     
         BZ    GEN12               NO, EDIT ANY INPUT                           
         NI    LOG,X'FF'-X'08'                                                  
         TM    LOG,X'60'           ANY CURR REQ                                 
         BNZ   TOPT10              GO HONOR THIS REQUEST                        
         LA    R3,SPTABLE                                                       
         A     R3,NXTASVTB                                                      
         B     DSPLY10             NO, EDIT ANY INPUT                           
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
         SPACE                                                                  
VK10     BAS   RE,CLRCLT           RESET OTHER KEY FLDS                         
         BAS   RE,SVTWA                                                         
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK12                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK12                                                             
         GOTO1 VSOXERR                                                          
VK12     DS    0H                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK14     LA    R2,TRACLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GO GET PROFILE RECORD(S)                     
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
* EDIT ESTIMATE *                                                               
         SPACE                                                                  
VK30     LA    R2,TRAESTH          EST NUMBER                                   
         SPACE                                                                  
         XC    SVESTAB,SVESTAB                                                  
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
         CLI   5(R2),0                                                          
         BNE   VK34                                                             
VK32     CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BNE   VK40                                                             
         B     MISESTER                                                         
         SPACE                                                                  
VK34     GOTO1 ANY                                                              
         CLC   =C'NO ',WORK                                                     
         BE    MISESTER                                                         
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    *+12                 NEEDS EST                                   
         CLI   SVPROF15,C'Y'       TRAFFIC BUYS DON'T USE ESTIMATES             
         BE    VK40                   UNLESS COPY CODE = ESTIMATE               
         SPACE                                                                  
*                                  INSTRUCTIONS BY ESTIMATE                     
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   QBEST,ACTUAL        SET AS START EST                             
         MVC   QBESTEND,ACTUAL     AND AS END                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   BDESTPR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   ESTCPYER                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVGENST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVGENEND)                                  
         DROP  R6                                                               
         SPACE                                                                  
VK40     LA    R2,TRASTAH          MARKET/STATION                               
         XC    BMKTSTA,BMKTSTA                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         TM    4(R2),X'08'         IS THIS NUMERIC                              
         BO    VK44                                                             
         GOTO1 VALISTA                                                          
         SPACE                                                                  
         B     VK46                                                             
VK44     GOTO1 VALIMKT                                                          
         MVC   TRAHDG+36(7),=C'   STA '                                         
         OI    TRAHDGH+6,X'80'                                                  
VK46     OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK50     LA    R2,TRAPERH          PERIOD                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 =A(VPER),RR=SPTR14RR VALIDATE PERIOD/FLIGHT                      
         SPACE                                                                  
* READ ESTIMATE HEADERS AND BUILD LIST OF ELIGIBLE                              
         SPACE                                                                  
         GOTO1 =A(BLEST),RR=SPTR14RR                                            
         BNE   NOESTER                                                          
         SPACE                                                                  
VK60     LA    R2,TRATAGSH                                                      
VK62     XC    SVTAGPRS,SVTAGPRS                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK64                NO                                           
         BAS   RE,VTAG                                                          
         BAS   RE,VTAGL            GO VALIDATE DEALER TAGS                      
VK64     OI    4(R2),X'20'         SET ON VALIDATED                             
VK66     TM    TABLESW,X'80'       IS SPOT TABLE BUILT                          
         BZ    BUY10               NO, BUILD IT                                 
         SPACE                                                                  
         BAS   RE,ATAG             GO ASSIGN TAGS                               
         SPACE                                                                  
         B     GEN12               GO EDIT REST OF SCREEN                       
         SPACE                                                                  
CLRCLT   XC    NEXTADDR,NEXTADDR                                                
         XC    AFPD(ELCDHI+1-AFPD),AFPD                                         
         XC    TOPASVTB(PTNTABLE-TOPASVTB),TOPASVTB                             
         XC    SPTABLE(256),SPTABLE                                             
         BR    RE                                                               
         EJECT                                                                  
* READ BUYS AND BUILD TABLE OF SPOTS                                            
         SPACE                                                                  
BUY10    XC    CTRS,CTRS           ZERO SPOT/UNTAG/UNCML CTS                    
         SPACE                                                                  
         GOTO1 =A(BLACT),RR=SPTR14RR   BUILD SPOT TABLE                         
         SPACE                                                                  
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         SPACE                                                                  
* FIND END OF SPTABLE FOR START OF PATTERN TABLE                                
         SPACE                                                                  
         BAS   RE,MSPT                                                          
         SPACE                                                                  
         MVC   TRAINFO(12),=C'TOTAL SPOTS='                                     
         LH    R2,SPOTCT                                                        
         LR    R1,R2               MOVE                                         
         SH    R1,UNTAGCT          SUBT PREV TAGGED (FROM X'18' ELEM)           
         STH   R1,UNTAGCT          SAVE TOTAL UNTAGGED                          
         LA    R3,TRAINFO+12                                                    
         EDIT  (R2),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                            
         OI    TRAINFOH+6,X'80'                                                 
         SPACE                                                                  
* READ PATTERNS AND BUILD TABLE OF PATTERNS BY DATE                             
         SPACE                                                                  
         BAS   RE,GETPTNS                                                       
         SPACE                                                                  
* NOW COMBINE INTO ONE TABLE OF PTNS, ROT, AND CML SEQ                          
         SPACE                                                                  
         GOTO1 =A(FPAT),RR=SPTR14RR                                             
         SPACE                                                                  
         OI    TRAPERH+4,X'20'     PERIOD VALID                                 
         OI    TRAESTH+4,X'20'     ESTIMATE VALID                               
         SPACE                                                                  
* ASSIGN COMMERCIALS TO EACH SPOT FROM TABLE OF PATTERNS                        
         SPACE                                                                  
         LA    R3,SPTABLE                                                       
         BAS   RE,ACML             GO ASSGN CMML                                
         SPACE                                                                  
         OI    TABLESW,X'80'       SPOT TABLE IS NOW BUILT                      
         SPACE                                                                  
* ASSIGN TAGS IF POSSIBLE                                                       
         SPACE                                                                  
         BAS   RE,ATAG                                                          
         EJECT                                                                  
* DISPLAY 1 SCREEN FULL OF SPOTS                                                
         SPACE                                                                  
         USING SPTABLED,R3                                                      
DSPLY10  BAS   RE,CLRSCRB          CLEAR BOTTOM OF SCREEN                       
         LR    R0,R3                                                            
         LA    R1,SPTABLE                                                       
         SR    R0,R1                                                            
         ST    R0,TOPASVTB         SAVE TOP OF SCREEN ENTRY ADDR                
         LA    R2,TRAEST1H                                                      
         MVC   TRAINFO+41(35),SPACES                                            
         MVC   TRAINFO+49(27),=CL27'SPOTS NNN TO NNN DISPLAYED'                 
         LA    R6,TRAINFO+55                                                    
         EDIT  (B1,SPTTBLN),(3,(R6))                                            
         SPACE                                                                  
DSPLY20  BAS   RE,BLIN             BUILD DISPLAY LINE                           
         SPACE                                                                  
         LA    R2,NEXTLINE(,R2)                                                 
         LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             TEST END OF TABLE                            
         BE    DSPLY24              YES                                         
         SPACE                                                                  
         LA    R0,TRALAST                                                       
         CR    R2,R0               TEST END OF SCREEN                           
         BL    DSPLY20                                                          
         SPACE                                                                  
         LA    R0,SPTABLE          SAVE NEXT TOP OF SCREEN ADDR                 
         LR    R1,R3                                                            
         SR    R1,R0                                                            
         ST    R1,NXTASVTB                                                      
         B     DSPLY26                                                          
DSPLY24  SR    R0,R0                                                            
         ST    R0,NXTASVTB         SAVE TOP OF SCREEN ADDR                      
         SPACE                                                                  
DSPLY26  SH    R3,=AL2(L'SPTDATA)                                               
         LA    R6,TRAINFO+62                                                    
         EDIT  (B1,SPTTBLN),(3,(R6))                                            
         MVC   TRAINFO+18(9),=C'UNTAGGED='                                      
         LA    R3,TRAINFO+27                                                    
         EDIT  (B2,UNTAGCT),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                    
         SPACE                                                                  
         MVC   TRAINFO+34(7),=C'NO CML='                                        
         LA    R3,TRAINFO+41                                                    
         EDIT  (B2,UNCMLCT),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                    
         OI    TRAINFOH+6,X'80'                                                 
         SPACE                                                                  
         L     R1,=A(MOREMSGD)                                                  
         OC    UNTAGCT,UNTAGCT                                                  
         BNZ   *+8                                                              
         L     R1,=A(DONEMSGD)                                                  
         LA    R2,TRATAGSH                                                      
         TM    TABLESW,X'60'       ANY CML MSGS                                 
         BZ    COMXIT              NO                                           
         L     R1,=A(CMLPATMS)                                                  
         TM    TABLESW,X'20'       WAS IT ILLEGAL CML                           
         BO    COMXIT              YES                                          
         L     R1,=A(CMLDIFMS)                                                  
         NI    TABLESW,X'FF'-X'40' ONLY GIVE DIFFERENT MSG ONCE                 
         B     COMXIT                                                           
         EJECT                                                                  
* RESTORE SPTABLE AND PROCESS ANY OPERATOR ENTRIES                              
         SPACE                                                                  
GEN10    BAS   RE,RDTWA                                                         
         TM    LOG,X'08'           COMPLETED SPECIAL REQUEST?                   
         BZ    GEN12                                                            
         NI    LOG,X'FF'-X'08'     RESET                                        
         LA    R3,SPTABLE                                                       
         B     DSPLY10                                                          
         SPACE                                                                  
* FIND ANY ENTRIES FOR CML,TAG                                                  
         SPACE                                                                  
GEN12    LA    R2,TRACML1H                                                      
         LA    R3,SPTABLE                                                       
         A     R3,TOPASVTB         GET 1ST ENTRY AT TOP OF CURR                 
         USING SPTABLED,R3                                                      
GEN14    TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    GEN16               YES                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   GEN20               YES                                          
GEN16    LA    R2,NEXTLINE(,R2)                                                 
         LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             TEST END OF LIST                             
         BE    GEN18               NOT YET                                      
         LA    R0,TRALAST          END OF SCREEN                                
         CR    R2,R0               PAST END                                     
         BL    GEN14               NO                                           
GEN18    TM    LOG,X'FF'           TEST FOR OTHER THAN NORMAL                   
         BNZ   TOPT10              YES, TEST REQUEST                            
         L     R3,NXTASVTB                                                      
         LA    R3,SPTABLE(R3)                                                   
         B     DSPLY10             PROCESS NEXT SCREEN,  IF ANY                 
         SPACE                                                                  
* PROCESS ANY CMLS OR TAGS ENTERED                                              
         SPACE                                                                  
GEN20    TM    SPTFLAG,SPTFHIA    HIATUS SPOT                                   
         BO    SPTHIAER                                                         
         TM    4(R2),X'08'         IS THIS NUMERIC                              
         BZ    GEN30                                                            
         SPACE                                                                  
* ENTERED TAG ONLY                                                              
         SPACE                                                                  
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         L     R5,BLOCK+4                                                       
GEN24    MVC   WORK(3),SPTTCD      VALIDATE ON THIS DATE                        
         XC    WORK+3(3),WORK+3                                                 
         BAS   RE,VDLR             GO VALIDATE DEALER                           
         BNE   DLRTAGER                                                         
         SPACE                                                                  
         OC    SPTTAG,SPTTAG                                                    
         BNZ   GEN26                                                            
         LH    R0,UNTAGCT                                                       
         BCTR  R0,0                                                             
         STH   R0,UNTAGCT                                                       
GEN26    STCM  R5,3,SPTTAG                                                      
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     GEN16                                                            
         SPACE                                                                  
* ENTERED CML OR CML AND TAG                                                    
         SPACE                                                                  
GEN30    CLI   8(R2),C'A'          IF LESS THAN LETTER, ERROR                   
         BL    CMLTAGER                                                         
         CLI   8(R2),C'O'          IF LETTER BETWEEN A-O, CML                   
         BH    CMLTAGER                                                         
         CLC   SPTCMLA,8(R2)       SAME AS EXISTING                             
         BE    GEN38                                                            
         BAS   RE,VCML             VALIDATE AND FIND CML SEQ                    
GEN38    CLI   5(R2),1             IF ONLY 1 CHAR, DONE                         
         BE    GEN16                                                            
         MVC   8(L'TRACML1-1,R2),9(R2)                                          
         MVI   8+L'TRACML1-1(R2),0                                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         STC   RE,5(R2)                                                         
         LA    RF,7(RE,R2)                                                      
         LA    R1,WORK+7                                                        
         MVC   WORK(8),SPACES                                                   
GEN40    CLI   0(RF),C'0'                                                       
         BL    NUMERR                                                           
         CLI   0(RF),C'9'                                                       
         BH    NUMERR                                                           
         MVC   0(1,R1),0(RF)                                                    
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         BCT   RE,GEN40                                                         
         PACK  DUB,WORK(8)                                                      
         CVB   R5,DUB                                                           
         B     GEN24               GO VALIDATE TAG                              
         SPACE                                                                  
TOPT10   TM    LOG,X'40'           PATTERN                                      
         BZ    TOPT20                                                           
         BAS   RE,CLRSCRB          CLEAR SCREEN                                 
         GOTO1 =A(RPAT),RR=SPTR14RR                                             
         NI    LOG,X'FF'-X'40'                                                  
         B     TOPTXIT                                                          
TOPT20   TM    LOG,X'20'                                                        
         BZ    TOPT30                                                           
         NI    LOG,X'FF'-X'20'                                                  
         CLC   UNTAGCT,SPOTCT      ANY SPOTS TAGGED                             
         BE    TOTERR              NO                                           
         BAS   RE,CLRSCRB          CLEAR SCREEN                                 
         GOTO1 =A(TDLR),RR=SPTR14RR                                             
TOPTXIT  OI    LOG,X'08'                                                        
         L     R1,=A(REQMS)                                                     
         LA    R2,TRATAGSH                                                      
         OI    TRATAGSH+6,X'80'                                                 
         OI    TRAINFOH+6,X'80'                                                 
         B     COMXIT                                                           
TOPT30   DC    H'0'                                                             
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES DEALER TAG NUMBERS                     *                 
***************************************************************                 
         SPACE                                                                  
         DS   0H                                                                
VTAG     NTR1                                                                   
         CLI   8(R2),X'6F'         CK FOR QUESTION MARK                         
         BE    TAGHELP             GIVE HELP MESSAGE                            
         MVI   SVDAYPT,0                                                        
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(L'SVTAGPRS/4,BLOCK),C',=,-'                   
         ZIC   R0,DMCB+4           GET FIELD CT                                 
         LTR   R0,R0                                                            
         BZ    TAGHELP                                                          
         LA    R4,BLOCK                                                         
         LA    R6,SVTAGPRS                                                      
VTAG10   TM    2(R4),X'80'         WAS IT NUMERIC                               
         BZ    VTAG40              NO CK FILTERS                                
         MVC   0(2,R6),6(R4)                                                    
         CLI   1(R4),0             ANY SECOND FLD                               
         BNE   VTAG20              YES                                          
         MVC   2(2,R6),0(R6)       SET PAIR EQ                                  
         B     VTAG32                                                           
VTAG20   TM    3(R4),X'80'         MUST BE NUMERIC                              
         BO    VTAG24                                                           
         TM    TABLESW,X'80'      TABLE BUILT YET                               
         BO    NUMERR                                                           
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         B     NUMERR                                                           
         SPACE                                                                  
VTAG24   MVC   2(2,R6),10(R4)                                                   
         SPACE                                                                  
VTAG30   CLC   0(2,R6),2(R6)                                                    
         BH    TAGSEQER                                                         
VTAG32   LA    R6,4(,R6)                                                        
VTAG34   LA    R4,32(,R4)                                                       
         BCT   R0,VTAG10                                                        
         OC    SVTAGPRS,SVTAGPRS   IF NO TAGS                                   
         BZ    EXIT                DONE                                         
         LA    RE,SVTAGPRS                                                      
         LA    RF,(L'SVTAGPRS/4)-1                                              
VTAG36   OC    4(4,RE),4(RE)       IS NEXT PAIR ZERO                            
         BZ    VTAG38              DONE                                         
         CLC   2(2,RE),4(RE)                                                    
         BNL   TAGSEQER                                                         
         LA    RE,4(,RE)                                                        
         BCT   RF,VTAG36                                                        
VTAG38   MVC   CURRTAG,SVTAGPRS                                                 
         B     EXIT                                                             
VTAG40   CLC   12(4,R4),=C'DPT='                                                
         BNE   TAGHELP             INVALID                                      
         MVC   SVDAYPT,16(R4)      SAVE DAYPART CODE                            
         B     VTAG34                                                           
         EJECT                                                                  
***********************************************                                 
* SUBROUTINE VALIDATES DEALER TAG NUMBER LIST *                                 
***********************************************                                 
         SPACE                                                                  
         DS   0H                                                                
VTAGL    NTR1                                                                   
         LA    R6,L'SVTAGPRS/4     MAX TAG PAIRS                                
         LA    R3,SVTAGPRS                                                      
         LH    R5,SVTAGPRS         GET 1ST TAG                                  
VTAGL10  MVC   WORK(6),SVPERST    VALIDATE ON FLIGHT DATES                      
         BAS   RE,VDLR             GO FIND DEALER REC                           
         BNE   DLRTAGER                                                         
         LA    R3,SVTAGPRS                                                      
VTAGL20  LA    R5,1(,R5)                                                        
         CH    R5,2(,R3)                                                        
         BNH   VTAGL10                                                          
         LA    R3,4(R3)                                                         
         LH    R5,0(,R3)                                                        
         LTR   R5,R5                                                            
         BZ    EXIT                                                             
         BCT   R6,VTAGL10                                                       
         B     EXIT                                                             
         EJECT                                                                  
**************************************************************                  
* SUBROUTINE READS DEALER TAG NUMBER RECORD WITH/WITHOUT MKT *                  
**************************************************************                  
         SPACE                                                                  
         DS   0H                                                                
VDLR     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DLRKEY,R4                                                        
         MVC   DLRKID,=X'0A2C'                                                  
         MVC   DLRKAM(3),BAGYMD AND BCLT                                        
         MVC   DLRKMKT,BMKT                                                     
         MVC   DLRKPROD,QPRD                                                    
         LTR   R5,R5               MUST BE 1                                    
         BZ    TAGSIZER                                                         
         CH    R5,=H'9999'         TO 9999                                      
         BH    TAGSIZER                                                         
         STH   R5,DLRKTAG                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VDLR10                                                           
         MVC   KEY,KEYSAVE                                                      
         XC    DLRKMKT,DLRKMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
VDLR10   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DLRDTAEL,R6                                                      
         CLC   DLRRLSE,WORK        SEE IF RELEASED                              
         BH    DLRDTER                                                          
         SPACE                                                                  
         OC    WORK+3(3),WORK+3    ONLY CHECKING 1 DATE                         
         BZ    VDLR20                                                           
         CLC   DLRRCL,WORK+3       SEE IF RECALL TOO SOON                       
         BL    DLRDTER                                                          
VDLR16   CR    RB,RB               SET CC EQ                                    
         B     EXIT                EXIT WITH CONDITION CODE                     
         SPACE                                                                  
VDLR20   CLC   DLRRCL,WORK         SEE IF RECALL TOO SOON                       
         BNL   VDLR16                                                           
         B     DLRDTER                                                          
         EJECT                                                                  
**********************************************************                      
* SUBROUTINE TO READ PATTERN RECORDS FOR SPTABLE ENTRIES *                      
**********************************************************                      
         SPACE                                                                  
         DS   0H                                                                
GETPTNS  NTR1                                                                   
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         L     R4,APTNSTRT                                                      
         ST    R4,NEXTADDR         INTL PATTERN LIST POINTER/COUNTER            
         USING PTNLISTD,R4                                                      
*                                                                               
         XC    0(L'PTNLIST,R4),0(R4) CLEAR ONE ENTRY                            
         SPACE                                                                  
         BAS   RE,RSTA             READ STATION MASTER FOR AFFL/TYPE            
         SPACE                                                                  
         MVC   BSLN,SPTSLN       SLN                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(5),BAGYMD - BCLT, BPRD, BSLN                               
         MVC   KEY+9(1),QBEST                                                   
         GOTO1 HIGH                                                             
         B     GETP30                                                           
*                                                                               
GETP20   SR    RE,RE                                                            
         ICM   RE,7,KEY+10         GET REF/SUBL                                 
         SRL   RE,10               DROP SUBLINE                                 
         LA    RE,1(,RE)           BUMP REF NUMBER                              
         SLL   RE,10                                                            
         STCM  RE,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    TEST REACHED END                             
         BZ    GETP40                                                           
         GOTO1 HIGH                                                             
*                                                                               
GETP30   CLC   KEY(10),KEYSAVE                                                  
         BNE   GETP40                                                           
*                                                                               
         BAS   RE,BLDLIST          BUILD PATTERN TABLE ENTRY                    
         B     GETP20                                                           
*                                                                               
GETP40   BAS   RE,BLDDATE          BUILD PATTERN DATE LIST                      
         CLI   ERROR,0                                                          
         BE    EXIT                                                             
         DC    H'0'                TEMP - I HOPE                                
         DROP  R3,R4                                                            
         EJECT                                                                  
****************************************************************                
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD *                
* ON ENTRY NEXTADDR POINTS TO NEXT PATTERN LIST SLOT AND       *                
*                   HIGH ORDER BYTE IS NEXT SEQUENCE NUMBER    *                
****************************************************************                
         SPACE                                                                  
         DS   0H                                                                
BLDLIST  NTR1                                                                   
         L     R4,NEXTADDR                                                      
         SPACE                                                                  
         LA    RE,L'PTNLIST(R4)    CHECK IF OUT OF ROOM                         
         LR    RF,R9                                                            
         A     RF,LSYSD                                                         
         CR    RE,RF                                                            
         BNL   SPTSIZER                                                         
         SPACE                                                                  
         XC    0(L'PTNLIST+1,R4),0(R4)    CLEAR 1 ENTRY + 1 BYTE                
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         CLI   1(R6),38            TEST EXTENDED ELEMENT                        
         BNH   *+12                NO                                           
         TM    PATSTAT,X'80'       TEST STATUS = DELETED                        
         BO    EXIT                YES - IGNORE                                 
         CLC   SVPEREND,PATSTART   PERIOD BEFORE PATTERN START                  
         BL    EXIT                                                             
         CLC   SVPERST,PATEND      PERIOD AFTER PATTERN END                     
         BH    EXIT                                                             
         SPACE                                                                  
* TEST PATTERN APPLIES TO THIS STATION *                                        
         SPACE                                                                  
         MVC   ELEM(6),PATSTART   SAVE DATES                                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL           FIND LIST ELEMENT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   BLDL11                                                           
         OC    PATLST,PATLST       TEST ALL MARKET PATTERN                      
         BNZ   BLDL11                                                           
         MVI   ELCODE,14           SET ALL MKT IND                              
         B     BLDL50                                                           
BLDL11   ZIC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1               SET FOR BCT                                  
         LA    R6,2(R6)                                                         
         SPACE                                                                  
         CLI   0(R6),C'G'          TEST MKT GROUP PATTERN                       
         BNE   BLDL11E                                                          
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
         MVC   WORK(18),KEY                                                     
BLDL11A  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(3),1(R6)                                                   
         MVC   KEY+11(2),BMKT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL11D                                                          
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL11B                                                          
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL11B                                                          
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL11D                                                          
         SPACE                                                                  
BLDL11B  MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+3(5),KEY+3      TRY NON-CLIENT SPECIFIC                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL11D                                                          
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL11C                                                          
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL11C                                                          
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL11D                                                          
BLDL11C  LA    R6,5(,R6)                                                        
         BCT   R0,BLDL11A                                                       
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         MVC   KEY(18),WORK                                                     
         B     EXIT                                                             
         SPACE                                                                  
BLDL11D  MVI   ELCODE,12                                                        
         MVC   KEY,WORK                                                         
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         B     BLDL50                                                           
         SPACE                                                                  
BLDL11E  CLI   0(R6),C'M'          TEST MKT PATTERN                             
         BNE   BLDL20                                                           
         SPACE                                                                  
* PROCESS MARKET LIST *                                                         
         SPACE                                                                  
BLDL12   CLC   BMKT,4(R6)          MATCH MARKET CODES                           
         BE    BLDL14                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL12                                                        
         B     EXIT                                                             
BLDL14   MVI   ELCODE,10           SET MARKET ENTRY IND                         
         B     BLDL50                                                           
*                                                                               
BLDL20   CLI   0(R6),C'A'          TEST AFFILIATE LIST                          
         BNE   BLDL21                                                           
         CLC   STAAFFL,1(R6)                                                    
         BNE   EXIT                                                             
         MVI   ELCODE,8            SET AFFILIATE ENTRY IND                      
         B     BLDL50                                                           
         SPACE                                                                  
* PROCESS COMBINED MARKET/AFFILIATE LIST *                                      
         SPACE                                                                  
BLDL21   CLI   0(R6),C'C'          TEST COMBINED                                
         BNE   BLDL30                                                           
         LR    RE,R0                                                            
         LR    RF,R6                                                            
BLDL22   CLI   1(R6),0             THIS AN AFFILIATE                            
         BE    BLDL23               NO                                          
         CLC   STAAFFL,1(R6)                                                    
         BE    BLDL24                                                           
BLDL23   LA    R6,5(R6)                                                         
         BCT   R0,BLDL22                                                        
         B     EXIT                                                             
BLDL24   CLC   BMKT(2),4(RF)       MATCH MARKET CODES                           
         BE    BLDL26                                                           
         LA    RF,5(,RF)                                                        
         BCT   RE,BLDL24                                                        
         B     EXIT                                                             
BLDL26   MVI   ELCODE,6            SET COMBINED                                 
         B     BLDL50                                                           
         SPACE                                                                  
BLDL30   CLI   0(R6),C'T'          TEST STATION TYPE LIMIT                      
         BNE   BLDL40                                                           
         CLC   STATYPE,1(R6)                                                    
         BNE   EXIT                                                             
         MVI   ELCODE,4            SET STATION TYPE ENTRY IND                   
         B     BLDL50                                                           
         SPACE                                                                  
BLDL40   CLI   0(R6),C'S'          TEST STATION LIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
BLDL41   OC    1(2,R6),1(R6)       THIS A CABLEHEAD STA                         
         BNZ   BLDL43               NO                                          
         CLC   BSTA(3),3(R6)                                                    
         BE    BLDL48                                                           
         B     BLDL43C                                                          
BLDL43   CLC   QSTA,1(R6)                                                       
         BE    BLDL48                                                           
         CLC   QSTA(4),1(R6)                                                    
         BNE   BLDL43C                                                          
         CLI   QSTA+4,C' '         IF TV, CAN BE BLANK                          
         BE    BLDL48                                                           
BLDL43C  LA    R6,5(,R6)                                                        
         BCT   R0,BLDL41                                                        
         B     EXIT                                                             
         SPACE                                                                  
BLDL48   MVI   ELCODE,2            SET STATION LIST ENTRY IND                   
         EJECT                                                                  
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
         SPACE                                                                  
BLDL50   L     R4,NEXTADDR                                                      
         LA    R4,0(R4)            CLEAR HOB                                    
         USING PTNLISTD,R4                                                      
*                                                                               
         L     R0,ASVSTOR                                                       
         A     R0,=A(6144-L'PTNLIST+1)                                          
         CR    R4,R0                     TEST ROOM IN LIST                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         XC    0(L'PTNLIST+1,R4),0(R4)   CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
BLDL52   ZIC   RE,NEXTADDR         BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STC   RE,NEXTADDR         AND SAVE                                     
         CLI   NEXTADDR,X'FF'                                                   
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
*                                                                               
         MVC   PTNLTYPE,ELCODE         SET ENTRY TYPE                           
         MVC   PTNLSEQ,NEXTADDR        MOVE SEQUENCE NUMBER                     
         MVC   PTNLSTR(6),ELEM         MOVE DTS FROM TEMP SAVE                  
*                                                                               
         ICM   RE,7,KEY+10                                                      
         SRL   RE,10               SHIFT OUT SUBLINE(10)                        
         X     RE,=X'00003FFF'     GET POSITIVE                                 
         STCM  RE,3,PTNLREF            REF/SUBLINE                              
         MVC   PTNLSLN,KEY+6           SPOT LEN                                 
         MVC   PTNLDSKA,KEY+14         AND DISK ADDRESS                         
*                                                                               
         CLC   PTNLEND,HEXFF       TEST PATTERN RUNS UFN                        
         BNE   *+8                                                              
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
         SPACE                                                                  
* FIND X'30' ELEMENT TO TEST PATTERN IS HIATUS *                                
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'HIATUS',2(R6)                                                 
         BNE   BLDL54                                                           
         OI    PTNLFLAG,PTNLFHIA                                                
*        XC    PTNLREF,PTNLREF     CLEAR REF/SUBLINE FOR HIATUS                 
*                                                                               
BLDL54   LA    R4,L'PTNLIST(R4)        NEXT PATTERN LIST ENTRY                  
         STCM  R4,7,NEXTADDR+1                                                  
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*************************************************************                   
* SUBROUTINE TO PROCESS PATTERN LIST DATA AND BUILD A TABLE *                   
* OF PATTERNS THAT APPLY TO EACH DAY IN THE TELECAST PERIOD *                   
*************************************************************                   
         SPACE                                                                  
         DS   0H                                                                
BLDDATE  NTR1                                                                   
         L     R5,APTNSTRT                                                      
         USING PTNLISTD,R5                                                      
*                                                                               
         XC    PARAS(8),PARAS      CLEAR DATE FOR ERROR RTN                     
         CLI   0(R5),0             TEST FOR NO PATTERN                          
         BE    DTERR                                                            
*                                                                               
BLDDT1   GOTO1 DATCON,DMCB,(3,SVPERST),WORK   FIRST TELECAST DATE               
         GOTO1 (RF),(R1),(3,SVPEREND),WORK+6  LAST TELECAST DATE                
*                                                                               
         LA    R6,DATELIST                                                      
         XC    0(183,R6),0(R6)     CLEAR 367 BYTES                              
         XC    183(184,R6),183(R6)                                              
*                                                                               
         MVI   ELCODE,14                                                        
*                                                                               
BLDDT2   CLI   PTNLTYPE,0          TEST FOR EOL                                 
         BE    BLDDT20                                                          
         CLC   PTNLTYPE,ELCODE     ELSE MATCH CODES                             
         BNE   BLDDT12                                                          
         SPACE                                                                  
* DETERMINE LIMITS OF PATTERN DATES VS TLCST DATES *                            
         SPACE                                                                  
BLDDT4   LA    R0,PTNLSTR          POINT TO PATTERN START                       
         CLC   SVPERST,PTNLSTR    FIRST TLCST DATE TO PATTERN START             
         BL    *+8                 LOW - USE PATTERN START DATE                 
         LA    R0,SVPERST          ELSE POINT TO FIRST TLCST DATE               
         GOTO1 DATCON,DMCB,(3,(R0)),WORK+12                                     
         LA    R0,PTNLEND                                                       
         CLC   SVPEREND,PTNLEND   LAST TLCST TO PATTERN END                     
         BH    *+8                                                              
         LA    R0,SVPEREND                                                      
         GOTO1 (RF),(R1),(3,(R0)),WORK+18                                       
         SPACE                                                                  
* GET DISPLACEMENT TO FIRST DATE IN DATELIST (FROM SVPERST) *                   
         SPACE                                                                  
         GOTO1 =V(PERVERT),(R1),WORK,WORK+12,RR=SPTR14RR                        
         LH    RE,8(R1)            GIVES INCLUSIVE DAYS                         
         LA    R6,DATELIST-1(RE)   POINT TO FIRST DATE                          
*                                                                               
BLDDT10  MVC   0(1,R6),PTNLSEQ     MOVE PATTERN SEQUENCE                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+12,WORK+12,1                                     
         CLC   WORK+12(6),WORK+18  TEST REACHED END LIMIT                       
         BNH   BLDDT10                                                          
*                                                                               
BLDDT12  LA    R5,L'PTNLIST(R5)    NEXT PATTERN LIST ENTRY                      
         B     BLDDT2                                                           
         SPACE 2                                                                
* DECREMENT VALUE IN ELCODE AND RE-PROCESS PATTERN LIST *                       
         SPACE                                                                  
BLDDT20  L     R5,APTNSTRT         RESET POINTERS                               
         LA    R6,DATELIST                                                      
         ZIC   R0,ELCODE                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,ELCODE                                                        
         LTR   R0,R0                                                            
         BP    BLDDT2                                                           
         DROP  R5                                                               
         SPACE                                                                  
* ALL LIST ELEMENTS PROCESSED - SHOULD HAVE *                                   
* PATTERN FOR EVERY DAY IN TELECAST PERIOD  *                                   
         SPACE                                                                  
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6,RR=SPTR14RR                         
         SPACE                                                                  
         LH    RE,8(R1)            NUMBER OF DAYS IN TELECAST PERIOD            
         LA    RF,DATELIST(RE)                                                  
         MVI   0(RF),X'FF'         END MARKER                                   
         SPACE                                                                  
* MAKE SURE THERE IS AT LEAST A PATTERN FOR 1 SPOT                              
         SPACE                                                                  
         SPACE                                                                  
         SPACE                                                                  
         CLI   0(R6),0                                                          
         BNE   BLDDT30             OKAY, THERE IS AT LEAST 1 PAT                
         LA    R6,1(R6)                                                         
         BCT   R0,*-12                                                          
         B     BLDDTERR                                                         
         SPACE                                                                  
* NOW BUILD NEW PATTERN LIST OVER THE OLD ONE - EACH ENTRY WILL  *              
* NOW CONTAIN THE ACTUAL FIRST AND LAST TELECAST DATES. AN ENTRY *              
* CAN APPEAR IN THE NEW TABLE MORE THAN ONCE                     *              
         SPACE                                                                  
BLDDT30  L     R4,AIO3                                                          
         LA    R0,8                                                             
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         L     R4,AIO3                                                          
         LA    R6,DATELIST                                                      
         B     BLDDT46                                                          
*                                                                               
BLDDT42  CLC   1(1,R6),0(R6)       NEXT ENTRY USE SAME PATTERN                  
         BNE   BLDDT44                                                          
         LA    R6,1(R6)                                                         
         B     BLDDT42                                                          
*                                                                               
BLDDT44  ST    R6,ALPD             SAVE END ADDRESS                             
         SPACE                                                                  
         SR    RE,RE                                                            
         ICM   RE,1,0(R6)          GET PATTERN SEQ NUM                          
         BZ    BLDDT45              IT IS A TBA                                 
         SPACE                                                                  
         BCTR  RE,0                                                             
         MH    RE,=AL2(L'PTNLIST)                                               
         A     RE,APTNSTRT         POINT TO ENTRY                               
         MVC   0(L'PTNLIST,R4),0(RE) COPY ENTRY TO BUILD AREA                   
         SPACE                                                                  
* CALCULATE START AND END DATES                                                 
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,SVPERST),WORK                                     
         L     R0,AFPD             GET PATTERN START DATE ADDRESS               
         LA    RE,DATELIST                                                      
         SR    R0,RE                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+8,(R0)                                      
         L     R0,ALPD                                                          
         LA    RE,DATELIST                                                      
         SR    R0,RE                                                            
         GOTO1 (RF),(R1),,WORK+16,(R0)                                          
*                                                                               
         GOTO1 DATCON,(R1),WORK+8,(3,WORK+22)                                   
         GOTO1 (RF),(R1),WORK+16,(3,WORK+25)                                    
*                                                                               
         MVC   PTNLSTR-PTNLIST(6,R4),WORK+22                                    
         LA    R4,L'PTNLIST(R4)                                                 
         SPACE                                                                  
* MAKE SURE THERE IS MORE ROOM *                                                
         SPACE                                                                  
         L     RE,AIO3                                                          
         LA    RE,2000-L'PTNLIST(RE)                                            
         CR    R4,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDDT45  L     R6,ALPD             GET LAST PATTERN DATE ADDRESS                
         LA    R6,1(R6)                                                         
*                                                                               
BLDDT46  ST    R6,AFPD             SET AS NEXT PATTERN START                    
         CLI   0(R6),X'FF'         TEST EOL                                     
         BNE   BLDDT42                                                          
         SPACE                                                                  
* NOW MOVE PATTERN DATA BACK OVER ORIGINAL LIST *                               
         SPACE                                                                  
         L     RE,APTNSTRT                                                      
         L     RF,AIO3                                                          
BLDDT50  MVC   0(L'PTNLIST,RE),0(RF)                                            
         LA    RE,L'PTNLIST(RE)                                                 
         LA    RF,L'PTNLIST(RF)                                                 
         CLI   0(RF),0             TEST EOL                                     
         BNE   BLDDT50                                                          
         MVI   0(RE),0             SET EOL FLAG                                 
         MVI   ERROR,0             RESET ERROR FLAG                             
         EJECT                                                                  
* NOW ASSIGN A PATTERN TO EACH SPOT *                                           
         SPACE                                                                  
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
BLDDT60  L     R5,APTNSTRT                                                      
         USING PTNLISTD,R5                                                      
         SPACE                                                                  
BLDDT64  CLC   PTNLSTR,SPTTCD                                                   
         BH    BLDDT66                                                          
         CLC   PTNLEND,SPTTCD                                                   
         BL    BLDDT66                                                          
         MVC   SPTSLN,PTNLSLN                                                   
         BNE   BLDDT66                                                          
         SPACE                                                                  
         MVC   SPTPREF,PTNLREF                                                  
         MVC   SPTFLAG,PTNLFLAG                                                 
         B     BLDDT68                                                          
BLDDT66  LA    R5,L'PTNLIST(,R5)                                                
         CLI   0(R5),0             TEST EOL                                     
         BNE   BLDDT64                                                          
         SPACE                                                                  
BLDDT68  LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             TEST MORE ENTRIES IN TABLE                   
         BNE   BLDDT60                                                          
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
* MISSING PATTERN ERRORS                                                        
         SPACE                                                                  
BLDDTERR LA    R0,DATELIST                                                      
         SR    R6,R0                         GET DSPL IN TABLE                  
         GOTO1 DATCON,DMCB,(3,SVPERST),DUB  GET START DT IN YYMMDD              
         GOTO1 ADDAY,(R1),DUB,DUB,(R6)       ADD DSPL                           
         GOTO1 DATCON,(R1),DUB,(8,WORK)                                         
         MVC   PARAS(8),WORK       SAVE ERROR DATE IN PARAS                     
         SPACE                                                                  
DTERR    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'* ERROR * NO PATTERN FOR'                         
         LA    R1,BPRD                                                          
         LA    R4,CONHEAD+25                                                    
         BAS   RE,FMTPRLN          FORMAT PRODUCT-SPOT LENGTH                   
*                                                                               
         CLI   BPRD2,0          TEST PIGGYBACK                                  
         BE    DTERR2                                                           
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R1,BPRD2                                                         
         BAS   RE,FMTPRLN          FORMAT PRODUCT-SPOT LENGTH                   
*                                                                               
DTERR2   MVC   0(8,R4),PARAS       MOVE SAVED DATE                              
*                                                                               
         LA    R2,TRAMEDH          POSITION CURSOR                              
         NI    TRAMEDH+4,X'DF'     SET OFF VALID BIT                            
         B     ERREXIT                                                          
         SPACE                                                                  
FMTPRLN  LA    RF,SVCLIST                                                       
*                                                                               
FMTPRLN2 CLC   0(1,R1),3(RF)                                                    
         BE    FMTPRLN4                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    FMTPRLN2                                                         
         DC    H'0'                                                             
*                                                                               
FMTPRLN4 MVC   0(3,R4),0(RF)                                                    
         LA    R4,2(R4)            POINT TO END OF PRD                          
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'-'                                                       
*                                                                               
         ZIC   R0,1(R1)                                                         
         CVD   R0,DUB                                                           
         EDIT  (R0),(3,2(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,3(R4)            POINT 1 BEYOND END                           
         BR    RE                                                               
         EJECT                                                                  
* FIND END OF SPTABLE FOR START OF PATTERN TABLE                                
         SPACE                                                                  
MSPT     LH    R0,SPOTCT                                                        
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         LA    R4,1                                                             
MSPT10   CLI   0(R3),0                                                          
         BE    MSPT20                                                           
         BCTR  R0,0                                                             
         STC   R4,SPTTBLN                                                       
MSPT14   LA    R3,SPTNEXT                                                       
         LA    R4,1(R4)                                                         
         B     MSPT10                                                           
         SPACE                                                                  
MSPT20   LTR   R0,R0               SPOTCT AND REAL TABLE SHOULD MATCH           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   1(7,R3),=C'*PTNTB*'                                              
         LA    R3,8(,R3)           LEAVE END MARKER                             
         ST    R3,APTNSTRT                                                      
         BR    RE                                                               
         EJECT                                                                  
* BUILD DISPLAY LINE *                                                          
         SPACE                                                                  
         USING DSPLINED,R2                                                      
         USING SPTABLED,R3                                                      
         DS   0H                                                                
BLIN     NTR1                                                                   
         ZIC   R0,SPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPEST,DUB                                                       
         SPACE                                                                  
         ZIC   R0,SPTLINE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPLIN,DUB                                                       
         SPACE                                                                  
         GOTO1 UNDAY,DMCB,SPTDAY,DSPDAY                                         
         SPACE                                                                  
         GOTO1 UNTIME,(R1),SPTTIME,DSPTIME                                      
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
         MVC   KEY+14(4),SPTDSKAD                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         USING BUYRECD,R4                                                       
         MVC   DSPPGN,BDPROGRM                                                  
         DROP  R4                                                               
         SPACE                                                                  
         OC    BSTA,BSTA           IS THIS A BY MARKET REQUEST                  
         BNZ   BLIN04                                                           
         XC    WORK(2),WORK                                                     
         MVC   WORK+2(3),SPTSTA                                                 
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+5,WORK+9                           
         MVI   DSPPGN+10,C' '                                                   
         MVC   DSPPGN+11(5),WORK+9                                              
         SPACE                                                                  
BLIN04   MVC   DSPDPT,SPTDPT                                                    
         SPACE                                                                  
         EDIT  (B1,SPTSLN),(3,DSPSLN)                                           
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,SPTTCD),(4,DSPDATE)                               
         SPACE                                                                  
         LR    R4,R2                                                            
         OI    6(R2),X'80'         SET XMT                                      
         ZIC   R0,0(R2)            BUMP TO CML/TAG FIELD                        
         AR    R2,R0                                                            
         LA    R5,8(,R2)                                                        
         CLI   SPTCMLA,0                                                        
         BE    BLIN10                                                           
         MVC   0(1,R5),SPTCMLA                                                  
         LA    R5,1(,R5)                                                        
         OI    6(R2),X'80'         SET XMT                                      
BLIN10   OC    SPTTAG,SPTTAG                                                    
         BZ    BLIN20                                                           
         EDIT  (B2,SPTTAG),(4,(R5)),ALIGN=LEFT                                  
         OI    4(R2),X'20'         SET ON VALIDATED                             
         OI    6(R2),X'80'             AND XMT                                  
         ZIC   R0,0(R2)            BUMP TO DEALER NAME FIELD                    
         AR    R2,R0                                                            
         SR    R5,R5                                                            
         ICM   R5,3,SPTTAG                                                      
         MVC   WORK(3),SPTTCD                                                   
         XC    WORK+3(3),WORK+3                                                 
         BAS   RE,VDLR             GO FIND DEALER REC                           
         BNE   BLIN16              MUST FIND IT, VALIDATED BEFORE               
BLIN14   GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
BLIN16   DC    H'0'                EITHER NO REC/ELEM FOUND                     
         USING DLRDTAEL,R6                                                      
         MVC   8(L'TRADLR1,R2),DLRNAME1                                         
         OI    6(R2),X'80'                                                      
BLIN20   TM    SPTFLAG,SPTFHIA    HIATUS PATTERN                                
         BZ    EXIT                                                             
         LR    R2,R4                                                            
         ZIC   R0,0(R2)            BUMP TO CML/TAG FIELD                        
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            BUMP TO DEALER NAME FIELD                    
         AR    R2,R0                                                            
         MVC   8(6,R2),=C'HIATUS'                                               
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
* UPDATE SPOT TABLE WITH TAG ENTRY                                              
         SPACE                                                                  
         DS   0H                                                                
ATAG     NTR1                                                                   
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         OC    SVTAGPRS(4),SVTAGPRS TAGS ENTERED IN HEADLINE                    
         BZ    EXIT                  NO                                         
ATAG10   OC    SPTTAG,SPTTAG        ALREADY TAGGED                              
         BNZ   ATAG30                YES                                        
         TM    SPTFLAG,SPTFHIA    THIS A HIATUS                                 
         BO    ATAG30                                                           
         SPACE                                                                  
         CLI   SVDAYPT,0           DAY PART LIMIT                               
         BE    ATAG20              NO                                           
         CLC   SVDAYPT,SPTDPT                                                   
         BNE   ATAG30                                                           
ATAG20   LH    R0,UNTAGCT                                                       
         BCTR  R0,0                                                             
         STH   R0,UNTAGCT                                                       
         LH    R4,CURRTAG          GET CURRENT TAG NUMBER TO USE                
         STCM  R4,3,SPTTAG                                                      
         LA    R4,1(,R4)           UPDATE TAG                                   
         LA    RE,SVTAGPRS                                                      
         LA    RF,L'SVTAGPRS/4                                                  
         CH    R4,0(RE)                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
ATAG22   CH    R4,0(RE)                                                         
         BL    ATAG24                                                           
         CH    R4,2(RE)                                                         
         BNH   ATAG26                                                           
         LA    RE,4(RE)                                                         
         OC    0(4,RE),0(RE)                                                    
         BZ    *+8                                                              
         BCT   RF,ATAG22                                                        
         MVC   CURRTAG,SVTAGPRS                                                 
         B     ATAG30                                                           
ATAG24   MVC   CURRTAG,0(RE)                                                    
         B     ATAG30                                                           
ATAG26   STH   R4,CURRTAG                                                       
ATAG30   LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             TEST END OF LIST                             
         BNE   ATAG10                                                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* ASSIGN A CML TO EACH SPOT FROM PATTERN TABLE                                  
         SPACE                                                                  
         USING SPTABLED,R3                                                      
         DS   0H                                                                
ACML     NTR1                                                                   
         SPACE                                                                  
ACML10   TM    SPTFLAG,SPTFHIA    THIS A HIATUS                                 
         BO    ACML36                                                           
         SPACE                                                                  
         LA    R5,PTNTABLE                                                      
         USING PTNTBLED,R5                                                      
ACML20   OC    SPTPREF,SPTPREF     IS THERE A PATTERN FOR THIS                  
         BZ    ACML36                NO, BYPASS SPOT                            
         SPACE                                                                  
         CLC   PTNTREF(3),SPTPREF  COMPARE REF/SPOT LEN                         
         BE    ACML30                                                           
         LA    R5,L'PTNENTRY(,R5)                                               
         OC    PTNTREF(3),PTNTREF                                               
         BNZ   ACML20                                                           
         DC    H'0'                                                             
ACML30   ZIC   R1,PTNTCURR         GET CURR PATTERN ROTATION POINTER            
         LA    R2,PTNTROT(R1)      POINT TO ROT LETTER                          
         LA    R1,1(,R1)           UPDATE                                       
         LA    RF,PTNTROT(R1)      POINT TO ROT LETTER                          
         CLI   0(RF),0             IS POINTER PAST END                          
         BNE   *+6                 NO                                           
         SR    R1,R1               RESET TO START                               
         STC   R1,PTNTCURR         SAVE UPDATED                                 
         SPACE                                                                  
         CLI   SPTCMLA,0           CML ALREADY ASSIGNED                         
         BNE   ACML40              YES                                          
         MVC   SPTCMLA,0(R2)       MOVE IN CMML LETTER A-L                      
         XC    DUB(4),DUB                                                       
         MVN   DUB+3(1),0(R2)                                                   
         SR    RF,RF                                                            
         CLI   0(R2),C'J'                                                       
         BL    ACML34                                                           
         LA    RF,9                                                             
ACML34   A     RF,DUB                                                           
         BCTR  RF,0                                                             
         SLL   RF,2                *4                                           
         LA    RE,PTNTCMLS(RF)                                                  
         MVC   SPTCMLSQ,1(RE)                                                   
         OC    SPTCMLSQ,SPTCMLSQ   MUST NOT BE ZERO                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
ACML36   LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             AT END OF TABLE                              
         BNE   ACML10                                                           
         B     EXIT                                                             
ACML40   LA    R0,MAXCMLS          MAX CMLS                                     
         LA    RE,PTNTCMLS                                                      
         SR    RF,RF                                                            
ACML44   CLC   SPTCMLSQ,1(RE)      THIS THE CML                                 
         BE    ACML46                                                           
         LA    RE,4(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,ACML44                                                        
         MVI   SPTCMLA,C'*'                                                     
         OI    TABLESW,X'20'       SET ON PREV CML NOT IN PATTERN               
         B     ACML36                                                           
ACML46   LA    RF,ALPHATAB(RF)                                                  
         MVC   SPTCMLA,0(RF)                                                    
         CLC   SPTCMLA,0(R2)       SAME AS THIS PATTERN                         
         BE    ACML36                                                           
         OI    TABLESW,X'40'       SET CMLS PREVIOUSLY ASSIGNED                 
         B     ACML36                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES COMMERCIAL LETTER AGAINST PATTERN LIST *                 
***************************************************************                 
         SPACE                                                                  
         DS   0H                                                                
VCML     NTR1                                                                   
         USING SPTABLED,R3                                                      
VCML10   LA    R5,PTNTABLE                                                      
         USING PTNTBLED,R5                                                      
VCML20   CLC   PTNTREF(3),SPTPREF                                               
         BE    VCML30                                                           
         LA    R5,L'PTNENTRY(,R5)                                               
         OC    PTNTREF(3),PTNTREF  AT END OF TABLE                              
         BNZ   VCML20              NO                                           
         DC    H'0'                                                             
VCML30   LA    R1,PTNTROT          START OF PATTERN ROTATION POINTERS           
VCML32   CLC   8(1,R2),0(R1)       POINT TO ROT LETTER                          
         BE    VCML34                                                           
         LA    R1,1(,R1)           UPDATE                                       
         CLI   0(R1),0             IS POINTER PAST END                          
         BNE   VCML32              NO                                           
         B     CMLERR                                                           
VCML34   MVC   SPTCMLA,8(R2)                                                    
         XC    DUB(4),DUB                                                       
         MVN   DUB+3(1),8(R2)                                                   
         SR    RF,RF                                                            
         CLI   8(R2),C'J'                                                       
         BL    VCML36                                                           
         LA    RF,9                                                             
VCML36   A     RF,DUB                                                           
         BCTR  RF,0                                                             
         SLL   RF,2                *4                                           
         LA    RE,PTNTCMLS(RF)                                                  
         MVC   SPTCMLSQ,1(RE)                                                   
         OC    SPTCMLSQ,SPTCMLSQ   MUST NOT BE ZERO                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         TM    TABLESW,X'20'                                                    
         BZ    EXIT                                                             
         LA    R3,SPTABLE                                                       
VCML42   CLI   SPTCMLA,C'*'        ANY CMLS NOT IN PATTERN LEFT                 
         BE    EXIT                YES                                          
         LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             AT END OF TABLE                              
         BNE   VCML42                                                           
         NI    TABLESW,X'FF'-X'20'                                              
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
* UPDATE BUY RECS FROM SPOT TABLE                                               
         SPACE                                                                  
         DS   0H                                                                
UPDATE   NTR1                                                                   
         SPACE                                                                  
* UPDATE BUY RECS FROM SPTABLE                                                  
         SPACE                                                                  
         TM    TABLESW,X'20'       PREV BAD CML STILL HERE                      
         BO    NOSAVER             YES                                          
         SPACE                                                                  
         LA    R3,SPTABLE                                                       
         LH    R4,SPOTCT                                                        
         LR    R0,R4                                                            
         LR    R1,R3                                                            
         USING SPTABLED,R1                                                      
*PD06    XC    SPTLTD(7),SPTLTD    ZERO LTD AND TIME FOR SORT                   
UPD06    XC    SPTTIME,SPTTIME     ZERO TIME FOR SORT                           
         LA    R1,L'SPTDATA(,R1)                                                
         BCT   R0,UPD06                                                         
         DROP  R1                                                               
         USING SPTABLED,R3                                                      
         SPACE                                                                  
* SORT ON DISK ADDRESS, DATE, AND SPOT NUMBER                                   
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A50')  QSORT                          
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),SPTABLE,(R4),L'SPTDATA,15,4                            
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
UPD10    XC    KEY,KEY                                                          
         MVC   KEY+14(4),SPTDSKAD                                               
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,KEY+14,(R6),     C        
               LISTAR                                                           
         SPACE                                                                  
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                  NO                                          
         DC    H'0'                                                             
UPD12    GOTO1 DATCON,DMCB,(3,SPTTCD),(2,WORK)                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
         XC    ELDATE,ELDATE                                                    
UPD16    BAS   RE,BUYEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    6(R6),X'C0'                                                      
         BNZ   UPD16                                                            
         CLI   1(R6),10            UNALLOCATED                                  
         BNH   UPD16                                                            
         CLC   2(2,R6),ELDATE                                                   
         BE    *+8                                                              
         MVI   SPOTNUMB,0                                                       
         MVC   ELDATE,2(R6)                                                     
         ZIC   RE,SPOTNUMB                                                      
         LA    RE,1(,RE)                                                        
         STC   RE,SPOTNUMB                                                      
         CLC   BPRD,10(R6)                                                      
         BNE   UPD16                                                            
         CLC   2(2,R6),WORK        SAME DATE                                    
         BNE   UPD16               NO                                           
         CLC   SPTSPTN,SPOTNUMB    SAME SPOT NUMBER                             
         BNE   UPD16               NO                                           
         LR    R4,R6                                                            
UPD18    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             THIS END OF RECORD                           
         BE    UPD20                YES                                         
         CLI   0(R4),24            THIS A DEALER TAG ELEMENT                    
         BNE   UPD20                NO, LOOK FURTHUR                            
         SPACE                                                                  
         CLI   1(R4),9             THIS HAD BETTER BE DEALER TAG ELEM           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   3(2,R4),SPTCMLSQ                                                 
         MVC   5(2,R4),SPTTAG                                                   
         XC    7(2,R4),7(R4)       ZERO DATE                                    
         B     UPD30                                                            
UPD20    XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING DLTGID,R1                                                        
         MVI   DLTGID,X'18'        ELCODE                                       
         MVI   DLTGLN,9            LENGTH                                       
         MVC   DLTGCSQ+1(2),SPTCMLSQ                                            
         MVC   DLTGTAG,SPTTAG                                                   
         DROP  R1                                                               
         SPACE                                                                  
         L     R1,AIO1                                                          
         SR    RE,RE                                                            
         ICM   RE,3,13(R1)         RECORD LENGTH                                
         ZIC   RF,ELEM+1           ELEMENT LENGTH                               
         AR    RE,RF                                                            
         CH    RE,=H'1975'         OVER MAX LENGTH                              
         BH    BUYSIZER            YES                                          
         L     R6,AIO                                                           
         GOTO1 VRECUP,DMCB,AIO1,ELEM,(R4)                                       
UPD30    LA    R3,SPTNEXT                                                       
         CLI   0(R3),0             AT END OF SPOT TABLE                         
         BE    UPD32               YES                                          
         CLC   SPTDSKAD,KEY+14     SAME REC                                     
         BE    UPD12               YES                                          
UPD32    GOTO1 DATAMGR,DMCB,=C'PUTREC',SYSFIL,KEY+14,AIO1,LISTAR                
         SPACE                                                                  
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                  NO                                          
         DC    H'0'                                                             
         CLI   0(R3),0             AT END OF SPOT TABLE                         
         BNE   UPD10               YES                                          
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         XC    TRATAGS,TRATAGS                                                  
         OI    TRATAGSH+6,X'80'                                                 
         OI    TRAMEDH+1,X'01'     SET MODIFY BIT ON                            
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALIDATED                          
         SPACE                                                                  
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         SPACE                                                                  
         L     R1,=A(SAVEDMSG)                                                  
         OC    UNTAGCT,UNTAGCT     ARE ALL SPOTS TAGGED                         
         BZ    *+8                  YES                                         
         L     R1,=A(UNTAGMS)                                                   
         SPACE                                                                  
         LA    R2,TRAMEDH                                                       
         BAS   RE,CLRCLT           CLEAR SAVE AND VALIDATED                     
         SPACE                                                                  
         B     COMERR                                                           
         DROP  R3                                                               
         EJECT                                                                  
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
         SPACE                                                                  
         USING SPTABLED,R3                                                      
         DS   0H                                                                
RSTA     NTR1                                                                   
         CLC   SVBSTA,SPTSTA                                                    
         BE    EXIT                                                             
         MVC   SVBSTA,SPTSTA                                                    
         MVC   WORK(2),BMKT                                                     
         MVC   WORK+2(3),SPTSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,QMKT,DUB                                        
         MVC   QSTA,DUB                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,(R1),=C'DMREAD',=C'STATION',KEY,(R6)                     
*                                                                               
         USING STARECD,R6                                                       
*                                                                               
         MVC   STAAFFL,SNETWRK                                                  
         MVC   STATYPE,STYPE                                                    
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
         DS   0H                                                                
FPRO     NTR1                                                                   
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 3                                                                
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
         SPACE                                                                  
         DS   0H                                                                
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         B     COMTWA                                                           
         SPACE                                                                  
* RESTORE SPTABLE                                                               
         SPACE                                                                  
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMREAD'                                                    
COMTWA   ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
* CLEAR DISPLAY AREA OF SCREEN *                                                
         SPACE                                                                  
CLRSCR   OC    TRAINFO,TRAINFO                                                  
         BZ    CLRSCRB                                                          
         CLC   TRAINFO,SPACES                                                   
         BE    CLRSCRB                                                          
         XC    TRAINFO,TRAINFO                                                  
         OI    TRAINFOH+6,X'80'                                                 
CLRSCRB  LA    RF,TRAEST1H                                                      
         LA    R1,TRALINES                                                      
*                                                                               
CLRSCR10 OC    8(L'TRAEST1,RF),8(RF)                                            
         BZ    CLRSCR20                                                         
         CLC   8(L'TRAEST1,RF),SPACES                                           
         BE    CLRSCR20                                                         
         XC    8(L'TRAEST1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
CLRSCR20 ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRACML1,RF),8(RF)  CLEAR CML/TAG FIELD                       
         OI    6(RF),X'80'            AND XMT                                   
*                                                                               
         ZIC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         OC    8(L'TRADLR1,RF),8(RF)                                            
         BZ    CLRSCR30                                                         
         CLC   8(L'TRADLR1,RF),SPACES                                           
         BE    CLRSCR30                                                         
         XC    8(L'TRADLR1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
CLRSCR30 IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCT   R1,CLRSCR10                                                      
         BR    RE                                                               
         EJECT                                                                  
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
         SPACE 3                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
         SPACE                                                                  
NOESTER  MVI   ERROR,NOESTS                                                     
         B     TRAPERR                                                          
BDESTPR  MVI   ERROR,BADESTS                                                    
         B     TRAPERR                                                          
DLRLENER MVI   ERROR,INVTXTLN     TOO LONG                                      
         B     TRAPERR                                                          
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
SPTSIZER L     R1,=A(SPTSIZMS)                                                  
         NI    TRAMEDH+4,X'FF'-X'20'  SET OFF VALIDATED                         
         B     COMERRA             NOTHING TO SAVE                              
SPTHIAER L     R1,=A(SPTHIAMS)                                                  
         B     COMERR                                                           
NOSAVER  L     R1,=A(NOSAVEMS)                                                  
         B     COMXIT                                                           
TOTERR   L     R1,=A(TOTERRMS)                                                  
         LA    R2,TRATAGSH                                                      
         B     COMXIT                                                           
CMLERR   L     R1,=A(CMLERRMS)                                                  
         B     COMERR                                                           
CMLTAGER L     R1,=A(CMLTAGMS)                                                  
         B     COMERR                                                           
TAGHELP  L     R1,=A(TAGHLPMS)                                                  
         XC    TRATAGS,TRATAGS                                                  
         MVC   TRATAGS(L'TAGOPTMS),TAGOPTMS                                     
         OI    TRATAGSH+6,X'80'                                                 
         B     TAGERXIT                                                         
ESTCPYER L     R1,=A(ESTCPYMS)                                                  
         B     TAGERXIT                                                         
MISESTER L     R1,=A(MISESTMS)                                                  
         B     TAGERXIT                                                         
TAGSIZER L     R1,=A(TAGSIZMS)                                                  
         B     TAGERXIT                                                         
BUYSIZER L     R1,=A(BUYSIZMS)                                                  
         B     TAGERXIT                                                         
DLRDTER  L     R1,=A(DLRDTEMS)                                                  
         B     TAGERXT                                                          
TAGSEQER L     R1,=A(TAGSEQMS)                                                  
TAGERXIT LA    R2,TRATAGSH                                                      
         TM    TABLESW,X'80'      TABLE BUILT YET                               
         BO    COMERRA                                                          
TAGERXT  NI    TRAMEDH+4,X'FF'-X'20' FORCE REVALIDATION                         
         B     COMERRA                                                          
DLRTAGER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DLRTAGMS),DLRTAGMS                                     
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+35(4),DUB                                                
         TM    TABLESW,X'80'      TABLE BUILT YET                               
         BO    ERREXIT                                                          
         NI    TRAMEDH+4,X'FF'-X'40' FORCE REVALIDATION                         
         B     ERREXIT                                                          
ESTSIZER L     R1,=A(ESTERMS)                                                   
COMXIT   OI    TRATABH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATABH+6,X'80'                                                  
         SPACE                                                                  
* NEED TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                           
         SPACE                                                                  
COMERR   BAS   RE,SVTWA                                                         
COMERRA  XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTR14RR                                                      
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,COMERMVC                                                      
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
WRITERR  MVC   GERROR,=Y(SPTRDONL) SPOT FILES ARE READ ONLY                     
         GOTO1 VTRAERR                                                          
COMERMVC MVC   CONHEAD(0),1(R1)                                                 
         SPACE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ALPHATAB DC    C'ABCDEFGHIJKLMNO'                                               
MAXCMLS  EQU   15                                                               
HEXFF    DC    5X'FF'                                                           
DLRTAGMS DC    C'* ERROR * NO RECORD FOR DEALER TAG N    *'                     
TAGOPTMS DC    C'PATTERN/TOTAL/UNTAG/SAVE/RECML - ONLY VALID'                   
         DC    AL1(L'TAGHLPMS-1)                                                
TAGHLPMS DC    C'* ENTER (DPT=) TAGS(1,5,9) PRS(1-9) BOTH(1,5-9,12) *'          
         DC    AL1(L'ESTCPYMS-1)                                                
ESTCPYMS DC    CL50'* ERROR * EST HAS COPY CODE *'                              
         DC    AL1(L'MISESTMS-1)                                                
MISESTMS DC    CL50'* ERROR * EST NEEDED FOR COPY CDE=EST *'                    
         DC    AL1(L'NOSAVEMS-1)                                                
NOSAVEMS DC    C'* ERROR * BAD CML (*) NOT REASSIGNED YET *'                    
         DC    AL1(L'SPTSIZMS-1)                                                
SPTSIZMS DC    C'* ERROR * MORE THAN 180 SPOTS, USE SMALLER PERIOD *'           
         DC    AL1(L'SPTHIAMS-1)                                                
SPTHIAMS DC    C'* ERROR * NO CML OR TAG FOR HIATUS SPOT *'                     
         DC    AL1(L'TAGSIZMS-1)                                                
TAGSIZMS DC    C'* ERROR * TAG NUMBER MUST BE 1 - 9999 *'                       
         DC    AL1(L'DLRDTEMS-1)                                                
DLRDTEMS DC    C'* ERROR * DEALER RELEASE/RECALL DATE ERROR *'                  
         DC    AL1(L'REQMS-1)                                                   
REQMS    DC    C'REQUEST DISPLAYED, HIT ENTER TO RESUME TAGGING *'              
         DC    AL1(L'TOTERRMS-1)                                                
TOTERRMS DC    C'* ERROR * NO SPOTS TAGGED *'                                   
         DC    AL1(L'ROTLENMS-1)                                                
ROTLENMS DC    C'* ERROR * ROTATION LONGER THAN 63 CHARACTERS *'                
         DC    AL1(L'CMLPATMS-1)                                                
CMLPATMS DC    C'* ERROR * COMMERCIAL (*) ASSIGNED NOT IN PATTERN *'            
         DC    AL1(L'CMLDIFMS-1)                                                
CMLDIFMS DC    C'* NOTE * COMMERCIALS ASSIGNED NOT SAME AS PATTERN *'           
         DC    AL1(L'CMLERRMS-1)                                                
CMLERRMS DC    C'* ERROR * COMMERCIAL NOT IN PATTERN *'                         
         DC    AL1(L'CMLNUMMS-1)                                                
CMLNUMMS DC    C'* ERROR * COMML XXXXXXXX CHAR 6-7 MUST BE NUMERIC *'           
         DC    AL1(L'CMLTAGMS-1)                                                
CMLTAGMS DC    C'* ERROR * ENTER 1 LETTER COMML (A-L) &&/OR TAG *'              
         DC    AL1(L'ESTERMS-1)                                                 
ESTERMS  DC    C'* ERROR * ESTIMATE MUST BE 1 TO 255 *'                         
         DC    AL1(L'TAGSEQMS-1)                                                
TAGSEQMS DC    C'* ERROR * TAG PAIRS MUST BE LOW-HIGH *'                        
         DC    AL1(L'MOREMSGD-1)                                                
MOREMSGD DC    C'ENTER CMLS-TAGS, HIT ENTER FOR NEXT SCREEN'                    
         DC    AL1(L'DONEMSGD-1)                                                
DONEMSGD DC    C'ALL SPOTS DISPLAYED-TYPE SAVE TO UPDATE'                       
         DC    AL1(L'SAVEDMSG-1)                                                
SAVEDMSG DC    C'TAG DATA SAVED, ENTER NEXT REQUEST'                            
         DC    AL1(L'UNTAGMS-1)                                                 
UNTAGMS  DC    C'TAG DATA SAVED, SPOTS NOT TAGGED, ENTER NEXT REQUEST'          
         DC    AL1(L'BUYSIZMS-1)                                                
BUYSIZMS DC    C'BUY LINE HAS TOO MANY SPOTS - SPLIT TO 2 BUY LINES'            
         EJECT                                                                  
         DROP  R7,RB,RC                                                         
***************************************************************                 
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD             *                 
***************************************************************                 
         SPACE                                                                  
VPER     DS    0D                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         XC    SVPERDTS,SVPERDTS                                                
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
         SPACE                                                                  
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         GOTO1 HIGH                                                             
*                                                                               
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
VPER14   BAS   RE,NEXTEL1                                                       
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVGENEND),(5,CONHEAD+21)                            
         GOTO1 ERREX2                                                           
         SPACE                                                                  
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
         SPACE                                                                  
VPER32   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         SPACE                                                                  
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
         SPACE                                                                  
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
         SPACE                                                                  
VPER40   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER50                                                           
         BAS   RE,FFLT                                                          
         SPACE                                                                  
         B     VPER60                                                           
         SPACE                                                                  
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
         SPACE                                                                  
VPER50   CLC   SVPERST,SVGENEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVPERST,SVGENST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
         SPACE                                                                  
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BNZ   VPER56                                                           
         MVC   SVPEREND,SVGENEND   USE EST END DATE                             
         SPACE                                                                  
VPER56   GOTO1 DATCON,DMCB,(3,SVPERST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
         SPACE                                                                  
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
         SPACE                                                                  
VPER60   GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
         XIT1                                                                   
         SPACE                                                                  
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         GOTO1 ERREX2                                                           
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERR1                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERR1                                                         
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR1                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
TRAPERR1 GOTO1 ERREX                                                            
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
         SPACE                                                                  
         DS   0H                                                                
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF4                                                            
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
*                                                                               
CHKF6    BAS   RE,NEXTEL1                                                       
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
         SPACE                                                                  
         CLC   SVPEREND,FLTEND      LAST TLCST DATE TO FLT END                  
         BH    FLTOVLER                                                         
         CLC   SVPERST,FLTSTART                                                 
         BL    FLTOVLER                                                         
         MVC   SVGENDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVPERDTS,FLTSTART   TEST FLIGHT = TLCST DATES                    
         BE    CHKF20                                                           
         MVC   SVGENDTS,SVPERDTS   FORCE FLIGHT DATES = TELECAST                
         B     CHKF20                                                           
         SPACE                                                                  
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
         SPACE                                                                  
CHKF10   CLC   SVPERST,FLTSTART                                                 
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVPEREND,SVGENEND                FORCE END DATE                  
         GOTO1 DATCON,DMCB,(3,SVPEREND),SVQEND  AND REQ END DATE                
         SPACE                                                                  
CHKF20   XIT1                                                                   
GETEL1   AH    R6,DATADISP                                                      
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
*********************************************************************           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON REQUESTED DATES *           
*********************************************************************           
         SPACE                                                                  
BLEST    DS    0D                                                               
         NMOD1 0,**BLEST*                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    SVESTAB,SVESTAB                                                  
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
         CLI   KEY+7,0                                                          
         BNE   *+8                                                              
         MVI   KEY+7,1                                                          
*                                                                               
BLEST2   DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLEST10                                                          
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLEST4                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,SVQEND       EST START AFTER REQ END                      
         BH    BLEST4                                                           
         CLC   EEND,SVQSTART       EST END BEFORE REQ START                     
         BL    BLEST4                                                           
         ZIC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),X'FF'                                                      
*                                                                               
BLEST4   MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         CLI   QBEST,0             TEST NO ESTIMATE REQUEST                     
         BE    BLEST2              YES - CONTINUE                               
         CLC   KEY+7(1),QBESTEND   TEST PAST END OF SERIES OR ONE               
         BL    BLEST2              NO - CONTINUE                                
         SPACE                                                                  
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
         SPACE                                                                  
BLEST10  GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         OC    SVESTAB,SVESTAB     TEST ANY DATA FOUND                          
         BZ    NEQXIT              NO - RETURN WITH CC SET                      
*                                                                               
         MVC   BEST(2),QBESTEND    SET ACTUAL ESTIMATE NUMBERS                  
*                                                                               
         CLI   QBEST,0             TEST EST=NO REQUEST                          
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
EQXIT    CR    RE,RE                                                            
         B     BLESTX                                                           
NEQXIT   CR    R7,RB                                                            
BLESTX   XIT1                                                                   
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
**************************************************                              
* READ THROUGH BUYS AND BUILD SPOT ACTIVITY LIST *                              
**************************************************                              
         SPACE                                                                  
         DS   0H                                                                
BLACT    DS    0H                                                               
         NMOD1 0,**BLACT*                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
* CLEAR 1ST 2 WORK AREAS IN ACTIVITY LIST BUILD AREA *                          
         SPACE                                                                  
         XC    SPTABLE(L'SPTDATA*2),SPTABLE                                     
         SPACE                                                                  
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(4),BAGYMD A/M, CLT, PRD                                      
         MVC   KEY+4(5),BMKT                                                    
         MVC   KEY+9(1),BEST                                                    
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         GOTO1 HIGH                                                             
         B     BLA12                                                            
         SPACE                                                                  
BLA10    GOTO1 SEQ                                                              
*                                                                               
* IF TAGGING BY MARKET, BSTA WILL BE ZERO *                                     
*                                                                               
BLA12    OC    BSTA,BSTA           IS THIS BY MKT                               
         BNZ   BLA13                NO, BY STATION                              
         SPACE                                                                  
         CLC   KEY(6),KEYSAVE      A-M/C/P/MKT                                  
         BNE   BLA14                                                            
         B     BLA13A                                                           
         SPACE                                                                  
BLA13    CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BNE   BLA14               DONE BUILD                                   
*                                                                               
BLA13A   CLI   KEY+10,X'FF'                                                     
         BNE   BLA10                                                            
*                                                                               
         ZIC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNE   BLA16               YES                                          
*                                                                               
         CLC   KEY+9(1),BESTEND    TEST HIGHER THAN END EST                     
         BNH   BLA10                                                            
         SPACE                                                                  
* SORT SPOTS IN DATE ORDER WITHIN STATION                                       
         SPACE                                                                  
BLA14    LH    R2,SPOTCT           GET NUMBER OF SPOTS                          
         LTR   R2,R2               IF NO SPOTS FOUND                            
         BZ    NOSPTER                                                          
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A50')  QSORT                          
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),SPTABLE,(R2),L'SPTDATA,14,8                            
         SPACE                                                                  
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
* GET BUY RECORD *                                                              
*                                                                               
BLA16    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
         TM    15(R4),X'80'        TEST DELETED BUY                             
         BO    BLA10               YES - IGNORE                                 
         SPACE                                                                  
* BYPASS BRAND (NON-POL) BUYS *                                                 
         SPACE                                                                  
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLA10                                                            
         SPACE                                                                  
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
         SPACE                                                                  
         ZIC   R0,BDDAY                                                         
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
         SPACE                                                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SR    R5,R5               ZERO EQ ELEM CTR                             
*                                                                               
BLA20    BAS   RE,BUYELA                                                        
         BNE   BLA10                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   BLA20                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   BLA20                                                            
         CLC   ELDATE,2(R6)        SAME DATE AS LAST                            
         BE    BLA22                YES                                         
         SR    R5,R5               ZERO EQ ELEM CTR                             
BLA22    MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         LA    R5,1(,R5)           ADD TO ELEMENT CTR                           
         SPACE                                                                  
         CLC   BPRD,10(R6)         THIS PROD                                    
         BNE   BLA20                NO                                          
         SPACE                                                                  
         CLC   ELDATE,PERENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    BLA20                                                            
         SPACE                                                                  
         CLC   ELDATE,PERSTP       TEST BEFORE FLIGHT/TELECAST                  
         BL    BLA20                                                            
         SPACE                                                                  
         MVC   ELDATEX,ELDATE      AND PRESET ELEM END DATE                     
         SPACE                                                                  
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLA24                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
         SPACE                                                                  
BLA24    CLC   ELDATEX,PERSTP      TEST BEFORE FLIGHT/TELECAST                  
         BL    BLA20                                                            
         SPACE                                                                  
         XC    SPTWORK,SPTWORK                                                  
         LA    R3,SPTWORK                                                       
         USING SPTABLED,R3                                                      
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,ELDATE),(3,SPTTCD)                                
         STC   R5,SPTSPTN                                                       
         MVC   SPTSTA,KEY+6                                                     
         MVC   SPTDAY,BDDAY                                                     
         MVC   SPTTIME,BDTIMST                                                  
         MVC   SPTEST,BUYKEST                                                   
         MVC   SPTLINE,BUYKEY+10                                                
         MVC   SPTDSKAD,KEY+14                                                  
         MVC   SPTSLN,BDSEC                                                     
         MVC   SPTDPT,BDDAYPT                                                   
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   PIGERR                                                           
         LR    RE,R6                                                            
BLA28    ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),13            THIS ANOTHER SPOT                            
         BNH   BLA40               YES                                          
         CLI   0(RE),X'18'         THIS A DEALER TAG ELEMENT                    
         BNE   BLA28               NO, LOOK FURTHUR                             
         USING DLTGID,RE                                                        
         CLI   DLTGLN,9            THIS DEALER TAG                              
         BNE   SPTAGER                                                          
         SPACE                                                                  
         MVC   SPTCMLSQ,DLTGCSQ+1   SAVE CMML SEQ                               
         MVC   SPTTAG,DLTGTAG        DEALER TAG                                 
         MVC   SPTTAGDT,DLTGDTE        POSSIBLE INSTR RUN DATE                  
         DROP  RE                                                               
         MVI   SPTCMLA,X'FF'                                                    
         SPACE                                                                  
         OC    SPTTAG,SPTTAG       IS THIS SPOT TAGGED                          
         BZ    BLA30                                                            
         LH    R1,UNTAGCT          ADD 1                                        
         LA    R1,1(,R1)                TO                                      
         STH   R1,UNTAGCT                 UNTAGCT                               
         SPACE                                                                  
BLA30    OC    SPTCMLSQ,SPTCMLSQ   IS THERE A CMML ASSIGNED                     
         BZ    BLA40                                                            
         LH    R1,UNCMLCT          ADD 1                                        
         LA    R1,1(,R1)                TO                                      
         STH   R1,UNCMLCT                 UNCMLCT                               
         SPACE                                                                  
* TEST DATA IN TABLE ALREADY *                                                  
         SPACE                                                                  
BLA40    LA    R3,SPTABLE                                                       
*                                                                               
BLA42    CLI   0(R3),0                                                          
         BE    BLA50                                                            
         CLC   0(L'SPTDATA,R3),SPTWORK STA/FTD/LTD/#/DAY/TIME/EST/LIN           
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SPTNEXT                                                       
         B     BLA42                                                            
*                                                                               
BLA50    LA    RE,TOPASVTB         MAKE SURE NOT PAST END OF SAVED AREA         
         A     RE,=A(6144-L'SPTDATA)                                            
         CR    R3,RE                                                            
         BH    SPTSZER                                                          
         SPACE                                                                  
         LH    R1,SPOTCT           UPDATE TOTAL SPOTS COUNTER                   
         LA    R1,1(R1)                                                         
         STH   R1,SPOTCT                                                        
         SPACE                                                                  
         MVC   SPTDATA,SPTWORK                                                  
         XC    L'SPTDATA(L'SPTDATA,R3),L'SPTDATA(R3)                            
         B     BLA20                                                            
         SPACE                                                                  
BUYELA   CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYELA                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYELA                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
PIGERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PIGERRMS),PIGERRMS                                     
         B     BLAEREX2                                                         
SPTAGER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SPTAGMS),SPTAGMS                                       
         B     BLAEREX2                                                         
SPTSZER  L     R1,=A(SPTSIZMS)                                                  
         A     R1,SPTR14RR                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SPTSIZMS),0(R1)                                        
         NI    TRAMEDH+4,X'FF'-X'20'  SET OFF VALIDATED                         
         B     BLAEREX2                                                         
NOSPTER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSPTMS),NOSPTMS                                       
         LA    R2,TRAMEDH                                                       
         NI    TRAMEDH+4,X'FF'-X'20'  SET OFF VALIDATED                         
BLAEREX2 GOTO1 ERREX2                                                           
         LTORG                                                                  
NOSPTMS  DC    C'* ERROR * NO SPOTS SELECTED *'                                 
PIGERRMS DC    C'* ERROR * CAN NOT HANDLE PIGGYBACK COMMERCIALS *'              
SPTAGMS  DC    C'* ERROR * DONE WITH SPOT ASSIGN, CAN''T DO SPOT TAG *'         
         DROP  R3,R4,RB,RC                                                      
         EJECT                                                                  
* FIND PATTERN(S) AND BUILD TABLE(S) OF ROT AND CMMLS                           
         SPACE                                                                  
FPAT     DS    0D                                                               
         NMOD1 0,**FPAT**                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         LH    R0,SPOTCT                                                        
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         SR    R1,R1                                                            
FPAT02   TM    SPTFLAG,PTNLFHIA    THIS A HIATUS SPOT                           
         BZ    *+6                 NO                                           
         BCTR  R1,0                                                             
         LA    R3,L'SPTDATA(,R3)                                                
         BCT   R0,FPAT02                                                        
         LTR   R1,R1                                                            
         BNZ   PTNHIAER           IF ANY HIATUS SPOTS, TELL EM                  
         SPACE                                                                  
         LA    R0,5                                                             
         LA    R1,PTNTABLE                                                      
FPAT04   XC    0(L'PTNENTRY,R1),0(R1)                                           
         LA    R1,L'PTNENTRY(,R1)                                               
         BCT   R0,FPAT04                                                        
         SPACE                                                                  
         LA    R3,SPTABLE                                                       
         XC    PTNTABLE(L'PTNENTRY+4),PTNTABLE                                  
         SPACE                                                                  
FPAT10   LA    R0,(L'PTNTABLE/L'PTNENTRY)   MAX PATTERNS                        
         LA    R5,PTNTABLE                                                      
         USING PTNTBLED,R5                                                      
FPAT12   TM    SPTFLAG,SPTFHIA    THIS A HIATUS SPOT                            
         BO    FPAT18                                                           
         SPACE                                                                  
         OC    SPTPREF,SPTPREF     IS A PATTERN ASSIGNED                        
         BZ    FPAT18               NO                                          
         SPACE                                                                  
         CLC   PTNTREF(3),SPTPREF  REF AND SPOT LEN                             
         BE    FPAT18                                                           
         OC    PTNTREF(3),PTNTREF  EMPTY ENTRY                                  
         BZ    FPAT16                                                           
         LA    R5,PTNEXT                                                        
         BCT   R0,FPAT12                                                        
         DC    H'0'                                                             
FPAT16   MVC   PTNTREF(3),SPTPREF                                               
         SPACE                                                                  
FPAT18   LA    R3,SPTNEXT                                                       
         CLI   0(R3),0                                                          
         BNE   FPAT10                                                           
         SPACE                                                                  
         LA    R5,PTNTABLE                                                      
FPAT20   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   PATKSLN,PTNTSLN                                                  
         MVC   PATKCODE,QBEST                                                   
         OC    PTNTREF,PTNTREF    HIATUS                                        
         BZ    FPAT40                                                           
         SPACE                                                                  
         LH    R0,PTNTREF                                                       
         X     R0,=X'00003FFF'     GET NEGATIVE                                 
         SLL   R0,10                                                            
         STCM  R0,7,PATKREF                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    FPAT24                                                           
         DC    H'0'                                                             
         SPACE                                                                  
FPAT24   SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10                                                            
         X     R1,=X'00003FFF'     GET POSITIVE                                 
         CH    R1,PTNTREF                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'30'        GET CML LIST                                 
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   R2,PATCMLLN                                                      
         SRL   R2,4                DIVIDE BY 16                                 
         SPACE                                                                  
         LA    R3,PATCML                                                        
         LA    R1,PTNTCMLS                                                      
         ST    R1,PTNTROT                                                       
FPAT30   CLI   0(R3),C'*'          DELETED COMMERCIAL                           
         BE    FPAT32                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R3)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         L     R1,PTNTROT                                                       
         MVC   0(3,R1),CMLSEQ                                                   
         B     FPAT34                                                           
         SPACE                                                                  
FPAT32   L     R1,PTNTROT                                                       
         XC    0(4,R1),0(R1)                                                    
FPAT34   LA    R1,4(,R1)                                                        
         ST    R1,PTNTROT                                                       
         LA    R3,16(,R3)                                                       
         BCT   R2,FPAT30                                                        
         XC    PTNTROT(4),PTNTROT                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATPTNEL,R6                                                      
         ZIC   R1,PATPTNLN                                                      
         SH    R1,=H'3'                                                         
         CH    R1,=H'63'           MAX ROTATION LENGTH                          
         BH    ROTLENER                                                         
         EX    R1,FPATMVC                                                       
         SPACE                                                                  
FPAT40   LA    R5,PTNEXT                                                        
         LA    R0,SPTABLE          END OF PTNTABLE                              
         CR    R0,R5                                                            
         BNH   FPAT50                                                           
         OC    PTNTREF(3),PTNTREF  AT END OF TABLE                              
         BNZ   FPAT20                                                           
         SPACE                                                                  
FPAT50   BAS   RE,PREF             GO PRINT PATTERN NUMBERS USED                
         B     EXIT2                                                            
         SPACE                                                                  
FPATMVC  MVC   PTNTROT(0),PATPTN-PATPTNEL(R6)                                   
PTNHIAER LCR   R1,R1                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PTNHIAMS+34(3),DUB                                               
         LH    R0,SPOTCT                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PTNHIAMS+45(3),DUB                                               
         MVC   CONHEAD,PTNHIAMS                                                 
         B     FPATERX                                                          
ROTLENER L     R1,=A(ROTLENMS)                                                  
         B     FPATER                                                           
CMLNUMER L     R1,=A(CMLNUMMS)                                                  
         A     R1,SPTR14RR                                                      
         MVC   16(8,R1),0(R3)      MOVE IN OFFENDING CML                        
         S     R1,SPTR14RR                                                      
FPATER   XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTR14RR                                                      
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,FPATRMVC                                                      
FPATERX  LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
FPATRMVC MVC   CONHEAD(0),1(R1)                                                 
         DROP  R3,R4,R5,R6                                                      
GETEL2   AH    R6,DATADISP                                                      
FIRSTEL2 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL2                19                      43               
PTNHIAMS DC    CL60'* ERROR * HIATUS PATTERN COVERING 000 SPOTS, 000 TOC        
               TAL *'                                                           
         EJECT                                                                  
* PRINT PATTERN NUMBERS USED IN HEADING                                         
         SPACE                                                                  
         DS   0H                                                                
PREF     NTR1                                                                   
         LA    R0,L'PTNTABLE/L'PTNENTRY MAX PATTERNS                            
         LA    R5,PTNTABLE                                                      
         SR    R2,R2                                                            
         USING PTNTBLED,R5                                                      
         LA    RE,PARAS                                                         
         XC    PARAS(22),PARAS                                                  
         SPACE                                                                  
PREF10   CLC   PTNTREF,0(RE)       SAME REF                                     
         BE    PREF14                                                           
         OC    0(2,RE),0(RE)                                                    
         BZ    PREF12                                                           
         LA    RE,2(,RE)                                                        
         B     PREF10                                                           
         SPACE                                                                  
PREF12   MVC   0(2,RE),PTNTREF     SAVE THIS REF #                              
         LA    RE,PARAS                                                         
         SPACE                                                                  
PREF14   LA    R5,L'PTNENTRY(,R5)                                               
         LA    R2,1(,R2)                                                        
         OC    PTNTREF(3),PTNTREF                                               
         BZ    PREF16                                                           
         BCT   R0,PREF10                                                        
         SPACE                                                                  
* SORT ON PATTERN REF NUMBER *                                                  
         SPACE                                                                  
PREF16   DS    0H                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A50')  QSORT                          
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),PARAS,(R2),2,2,0                                       
         LA    R2,PARAS                                                         
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         OI    TRAPREFH+6,X'80'                                                 
         SPACE                                                                  
PREF22   LH    R4,0(,R2)                                                        
         EDIT  (R4),(3,(R3)),ALIGN=LEFT                                         
         LA    R2,2(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BZ    PREF30                                                           
PREF24   CLI   0(R3),C' '                                                       
         BNH   PREF26                                                           
         LA    R3,1(,R3)                                                        
         B     PREF24                                                           
PREF26   MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         B     PREF22                                                           
PREF30   MVC   TRAPREF,ELEM                                                     
         B     EXIT2                                                            
         DROP  R5,RB,RC                                                         
         EJECT                                                                  
* FIND PATTERN(S) AND BUILD TABLE(S) OF ROT AND CMMLS                           
         SPACE                                                                  
RPAT     DS    0D                                                               
         NMOD1 0,**RPAT**                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         LA    R5,PTNTABLE                                                      
         USING PTNTBLED,R5                                                      
         SPACE                                                                  
         LA    R2,TRAEST1H                                                      
RPAT10   LA    R0,TRALAST          CK PAST SCREEN END                           
         CR    R0,R2                                                            
         BL    RPAT30              QUIT, BUT SEND INCOMPLETE MSG                
         MVC   8(4,R2),=C'REF='                                                 
         LH    R3,PTNTREF                                                       
         LR    R4,R3                                                            
         EDIT  (R4),(3,12(R2)),ALIGN=LEFT                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(4),BAGYMD BCLT AND BPRD                                   
         MVC   PATKSLN,PTNTSLN                                                  
         MVC   PATKCODE,QBEST                                                   
         LR    R1,R3                                                            
         X     R1,=X'00003FFF'     GET NEGATIVE                                 
         SLL   R1,10                                                            
         STCM  R1,7,PATKREF                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10                                                            
         X     R1,=X'00003FFF'     GET NEGATIVE                                 
         CR    R1,R3                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'32'        GET ROTATION                                 
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATPTNEL,R6                                                      
         MVC   16(4,R2),=C'ROT='                                                
         ZIC   R1,PATPTNLN                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         CLI   PATPTNLN,L'TRAEST1-12                                            
         BNH   RPAT16                                                           
         LA    R1,L'TRAEST1-13                                                  
RPAT16   EX    R1,RPATMVC                                                       
         CLI   PATPTNLN,L'TRAEST1-13                                            
         BNH   RPAT18                                                           
         ZIC   R1,0(R2)                                                         
         LA    R1,0(R1,R2)                                                      
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         MVC   8(16,R1),=C'PARTIAL ROTATION'                                    
         OI    6(R1),X'80'                                                      
RPAT18   OI    6(R2),X'80'                                                      
         LA    R2,NEXTLINE(,R2)                                                 
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'        GET CML LIST                                 
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   R4,PATCMLLN                                                      
         SRL   R4,4                DIVIDE BY 16                                 
         LA    R3,PATCML                                                        
         LA    R6,ALPHATB+14                                                    
RPAT20   LA    R0,TRALAST          CK PAST SCREEN END                           
         CR    R0,R2                                                            
         BL    RPAT30              QUIT, BUT SEND INCOMPLETE MSG                
         MVC   8(1,R2),0(R6)                                                    
         MVI   9(R2),C'='                                                       
         MVC   10(8,R2),0(R3)                                                   
         CLC   =XL8'5C00000000000000',0(R3) DELETED CML                         
         BE    *+8                                                              
         BAS   RE,PCML                                                          
         OC    8(8,R3),8(R3)                                                    
         BZ    RPAT24                                                           
         MVI   18(R2),C'-'                                                      
         MVC   19(8,R2),8(R3)                                                   
RPAT24   LA    R3,16(,R3)                                                       
         OI    6(R2),X'80'                                                      
         LA    R2,NEXTLINE(,R2)                                                 
         BCTR  R6,0                                                             
         BCT   R4,RPAT20                                                        
         LA    R5,L'PTNENTRY(,R5)                                               
         OC    0(4,R5),0(R5)       AT END OF TABLE                              
         BNZ   RPAT10              NO                                           
         MVC   TRAINFO+34(17),SPACES                                            
         B     RPAT34                                                           
         SPACE                                                                  
RPATMVC  MVC   20(0,R2),PATPTN-PATPTNEL(R6)                                     
         SPACE                                                                  
RPAT30   MVC   TRAINFO+34(9),SPACES                                             
         MVC   TRAINFO+43(8),=C'PARTIAL '                                       
         SPACE                                                                  
RPAT34   MVC   TRAINFO+51(24),=CL24'PATTERN TOTALS DISPLAYED'                   
         B     EXIT2                                                            
         DROP  R5,R6                                                            
         SPACE 3                                                                
* READ CML REC AND PRINT CML NAME                                               
         SPACE                                                                  
         DS   0H                                                                
PCML     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD BCLT                                            
         MVC   CMLKCML,0(R3)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'        GET CML LIST                                 
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   36(15,R2),CMLTITLE                                               
         B     EXIT2                                                            
         DROP  R6                                                               
         SPACE 3                                                                
GETEL3   AH    R6,DATADISP                                                      
FIRSTEL3 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL3                                                         
         DROP  RB,RC                                                            
         EJECT                                                                  
* SUBROUTINE PRINTS TOTAL SPOTS FOR EACH DEALER                                 
         SPACE                                                                  
         DS   0H                                                                
TDLR     NMOD1 0,**TDLR**                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         SR    R5,R5                                                            
         XC    BLOCK(256),BLOCK                                                 
TDLR10   LA    R4,BLOCK                                                         
TDLR14   OC    SPTTAG,SPTTAG       UNTAGGED                                     
         BZ    TDLR40                                                           
         OC    0(4,R4),0(R4)       EMPTY SLOT                                   
         BNZ   TDLR20                                                           
         MVC   0(2,R4),SPTTAG                                                   
         MVI   3(R4),1                                                          
         BCTR  R5,0                                                             
         B     TDLR40                                                           
TDLR20   CLC   SPTTAG,0(R4)                                                     
         BE    TDLR30                                                           
         LA    R4,4(,R4)                                                        
         B     TDLR14                                                           
TDLR30   LH    R1,2(,R4)                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,2(,R4)                                                        
TDLR40   LA    R3,SPTNEXT                                                       
         CLI   0(R3),0                                                          
         BNE   TDLR10                                                           
         LCR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A50')  QSORT                          
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),BLOCK,(R5),4,2,0                                       
         LA    R3,BLOCK                                                         
         SR    R4,R4                                                            
         OI    TRAEST1H+6,X'80'                                                 
TDLR50   LA    R2,TRALINES-1                                                    
         LA    R6,TRAEST1(R4)                                                   
         LA    RF,TRAEST2H                                                      
         MVC   0(12,R6),=C'DEALER SPOTS'                                        
         LA    R6,NEXTLINE(,R6)                                                 
TDLR52   EDIT  (B2,0(R3)),(6,0(R6))                                             
         EDIT  (B2,2(R3)),(5,7(R6))                                             
         OI    6(RF),X'80'                                                      
         LA    R3,4(,R3)                                                        
         LA    R6,NEXTLINE(,R6)                                                 
         LA    RF,NEXTLINE(,RF)                                                 
         BCT   R5,TDLR54                                                        
         MVC   TRAINFO+34(13),SPACES                                            
         MVC   TRAINFO+48(27),=CL27'DEALER TAG TOTALS DISPLAYED'                
         B     EXIT2                                                            
TDLR54   BCT   R2,TDLR52                                                        
         LA    R4,14(,R4)          NEXT COL                                     
         CH    R4,=H'42'           CAN ONLY GO 4 COL                            
         BNH   TDLR50                                                           
         DC    H'0'                                                             
ALPHATB  DC    C'ONMLKJIHGFEDCBA'                                               
         DROP  RB,RC                                                            
         SPACE                                                                  
* EDIT TAG PAIRS INTO TAG LINE                                                  
         SPACE                                                                  
EPRS     DS    0D                                                               
         NMOD1 0,**EPRS**                                                       
         L     RC,SPTR14RC                                                      
         USING GEND,RC                                                          
         XC    TRATAGS,TRATAGS                                                  
         OI    TRATAGSH+6,X'80'                                                 
         OC    SVTAGPRS,SVTAGPRS                                                
         BZ    EXIT2                                                            
         LA    R2,L'SVTAGPRS/4                                                  
         LA    R3,SVTAGPRS                                                      
         LA    R6,TRATAGS                                                       
         CLI   SVDAYPT,0                                                        
         BE    EPRS14                                                           
         MVC   0(6,R6),=C'DPT= ,'                                               
         MVC   4(1,R6),SVDAYPT                                                  
         LA    R6,6(,R6)                                                        
         B     EPRS14                                                           
EPRS10   MVI   0(R6),C','                                                       
         LA    R6,1(,R6)                                                        
EPRS14   L     R4,0(,R3)           PUT                                          
         SRDL  R4,16                   TAG PAIR                                 
         SRL   R5,16                          IN R4 & R5                        
         XC    DMCB(8),DMCB                                                     
         EDIT  (R4),(4,DMCB),ALIGN=LEFT                                         
         LA    RE,DMCB                                                          
EPRS20   MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(,R6)                                                        
         LA    RE,1(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BH    EPRS20                                                           
         CR    R4,R5               ARE BOTH EQUAL                               
         BE    EPRS40                                                           
         MVI   0(R6),C'-'                                                       
         LA    R6,1(,R6)                                                        
         XC    DMCB(8),DMCB                                                     
         EDIT  (R5),(4,DMCB),ALIGN=LEFT                                         
         LA    RE,DMCB                                                          
EPRS30   MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(,R6)                                                        
         LA    RE,1(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BH    EPRS30                                                           
EPRS40   LA    R3,4(,R3)                                                        
         OC    0(4,R3),0(R3)                                                    
         BZ    EXIT2                                                            
         BCT   R2,EPRS10                                                        
EXIT2    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'T21614 - DEALER TAGGING - MISC RECORDS'                         
*   INCLUDE CLTHDR                                                              
*   INCLUDE ESTHDR                                                              
*   INCLUDE BUYRECD                                                             
*   INCLUDE STARECD                                                             
*   INCLUDE SPTRCMML                                                            
*   INCLUDE SPTRPAT                                                             
*   INCLUDE SPTRFLT                                                             
*   INCLUDE SPTRDLR                                                             
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRDLR                                                        
         PRINT ON                                                               
         TITLE 'T21614 - DEALER TAGGING - DSECTS'                               
*   INCLUDE DMDTFIS                                                             
*   INCLUDE DDSPOOLD                                                            
*   INCLUDE DDSPLWORKD                                                          
*   INCLUDE SPTRAFFD                                                            
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRAB4D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
DATELIST DS    367X                                                             
AFPD     DS    A                   A(FIRST DATE THIS PTTN)                      
ALPD     DS    A                   A(LAST DATE THIS PTTN)                       
         SPACE                                                                  
* FROM FLIGHT RECORD OR ENTERED TELECAST DATES                                  
         SPACE                                                                  
PERSTP   DS    H                   FLIGHT/TELECAST START DATE                   
PERENDP  DS    H                   FLIGHT/TELECAST END DATE                     
         SPACE                                                                  
* FROM BUY ELEMENT                                                              
         SPACE                                                                  
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
ROTDAYS  DS    H                                                                
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SVBSTA   DS    XL3                                                              
SVQSTA   DS    CL5                                                              
STAAFFL  DS    CL1                                                              
STATYPE  DS    CL1                                                              
SPOTNUMB DS    XL1                                                              
SPTWORK  DS    CL32                                                             
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
SVESTAB  DS    XL256                                                            
SPTR14RR DS    A                                                                
APTNSTRT DS    A                                                                
ASVSTOR  DS    A                                                                
NEXTADDR DS    A                                                                
SPTR14RC DS    A                                                                
TOPASVTB DS    A                   1ST TABLE ENTRY ON CURR SCREEN               
NXTASVTB DS    A                   1ST TABLE ENTRY ON NEXT SCREEN               
SVTAGPRS DS    CL60                15 TAG PAIRS                                 
CURRTAG  DS    H                                                                
         SPACE                                                                  
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
         SPACE                                                                  
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
         SPACE                                                                  
* FROM FLIGHT OR ESTIMATE RECORD                                                
         SPACE                                                                  
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 ESTAIMATE START DATE                         
SVGENEND DS    XL3                           END DATE                           
         SPACE                                                                  
         DS   0H                                                                
CTRS     DS   0XL6                                                              
SPOTCT   DS    H                                                                
UNTAGCT  DS    H                                                                
UNCMLCT  DS    H                                                                
         SPACE                                                                  
TABLESW  DS    XL1                 80 = TABLE BUILT                             
*                                  40 = SPOT CMLS PRE-ASSIGNED                  
*                                  20 = CML PREV ASSIGNED NOT IN PATTRN         
         SPACE                                                                  
LOG      DS    XL1                 40 - PATTERN                                 
*                                  20 - TOTALS                                  
*                                  08 - COMPLETED SPEC REQUEST                  
         DS    0D                                                               
PTNTABLE DS    XL1280              10-128 BYTE ENTRIES MAX                      
         DS    XL4                 END MARKER                                   
         SPACE                                                                  
SVDAYPT  DS    CL1                                                              
         DS    0D                                                               
SPTABLE  DS    180XL32              SPOT TABLE BUILD AREA                       
         SPACE                                                                  
ENDSYSD  EQU   *                IF THIS ADDRESS >2F08, PAST END OF SYSD         
         SPACE                                                                  
* PATTERN TABLE IS BUILT AT END OF ACTIVE SPTABLE+1                             
         SPACE                                                                  
TRALINES EQU   (TRALAST-TRAEST1H)/(TRAEST2H-TRAEST1H)                           
NEXTLINE EQU   TRAEST2H-TRAEST1H                                                
         EJECT                                                                  
* DSECT FOR SPOT ACTIVITY LIST ENTRIES *                                        
         SPACE                                                                  
SPTABLED DSECT                                                                  
SPTDATA  DS    0XL32                                                            
SPTDAY   DS    XL1                 DAYS                                         
SPTDPT   DS    XL1                 DAY PART                                     
SPTCMLA  DS    XL1                 PATTERN ROT CML LETTER A-L                   
SPTTBLN  DS    XL1                 ASSIGNED SPOT TABLE NUMBER 1 ... N           
SPTDSKAD DS    XL4                 BUY REC DISK ADDR                            
SPTSTA   DS    XL3                 STATION                                      
SPTTCD   DS    XL3                 SPOT TELECAST DATE                           
SPTTIME  DS    XL4                 START/END TIMES                              
SPTSPTN  DS    XL1                 SPOT NUMBER FOR = DATES                      
SPTEST   DS    XL1                 ESTIMATE                                     
SPTLINE  DS    XL1                 BUY LINE                                     
SPTCMLSQ DS    XL2                 CML SEQ NUMBER                               
SPTTAG   DS    XL2                 DEALER TAG ASSIGNED                          
SPTTAGDT DS    XL2                 DLR TAG DATE (FROM X'18' BUY REC EL)         
SPTPREF  DS    XL2        *        PATTERN REF NUMBER \  KEEP REF SLN           
SPTSLN   DS    XL1        *        SPOT LENGTH         \ TOGETHER               
SPTFLAG  DS    XL1                                                              
SPTFHIA  EQU   X'80'              THIS SPOT HIATUS PATTERN                      
         DS    CL1                 SPARE                                        
SPTNEXT  EQU   *                                                                
         SPACE 2                                                                
* DSECT FOR PATTERN LIST ENTRIES *                                              
         SPACE                                                                  
PTNLISTD DSECT                                                                  
PTNLIST  DS    0XL16                                                            
PTNLTYPE DS    XL1                 10=ALL MKT  8=ONE MKT  6=AFFL                
*                                   4=STA TYPE 2=STATION                        
PTNLSEQ  DS    XL1                 PATTERN SEQ NUM (MIN 1)                      
PTNLSTR  DS    XL3                 PATTERN START DATE                           
PTNLEND  DS    XL3                 PATTERN END DATE                             
PTNLREF  DS    XL2                 REF                                          
PTNLSLN  DS    XL1                 SPOT LENGTH                                  
PTNLDSKA DS    XL4                 DISK ADDRESS                                 
PTNLFLAG DS    XL1                                                              
PTNLFHIA EQU   X'80'               PATTERN IS HIATUS                            
PTNLFUFN EQU   X'40'               PATTERN RUNS UFN                             
         EJECT                                                                  
* DSECT FOR PATTERN CML/ROT TABLES                                              
         SPACE                                                                  
PTNTBLED DSECT                                                                  
PTNENTRY DS    0CL128                                                           
PTNTREF  DS    CL2                                                              
PTNTSLN  DS    XL1                                                              
PTNTCMLS DS    15XL4               CML SEQ(3) AND POSSIBLE TAG #(1)             
PTNTCURR DS    XL1                 CURRENT ROTATION POINTER                     
PTNTROT  DS    CL64                ROTATION                                     
PTNEXT   EQU   *                                                                
         SPACE 2                                                                
* DSECT FOR DISPLAY LINE FOR SPOTS                                              
         SPACE                                                                  
DSPLINED DSECT                                                                  
         DS    CL8                 ALLOW FOR FLDHDR                             
DSPEST   DS    CL3                                                              
         DS    CL1                                                              
DSPLIN   DS    CL3                                                              
         DS    CL1                                                              
DSPDAY   DS    CL7                                                              
         DS    CL1                                                              
DSPTIME  DS    CL11                                                             
         DS    CL1                                                              
DSPPGN   DS    CL16                                                             
         DS    CL1                                                              
DSPDPT   DS    CL1                                                              
DSPSLN   DS    CL3                                                              
         DS    CL2                                                              
DSPDATE  DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPTRA14   08/30/04'                                      
         END                                                                    
