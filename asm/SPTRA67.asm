*          DATA SET SPTRA67    AT LEVEL 033 AS OF 09/01/10                      
*PHASE T21667B                                                                  
*INCLUDE PQPROF                                                                 
*        TITLE 'T21667 FAX LETTER GENERATION'                                   
***********************************************************************         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO1 - READ INSTRUCTION RECAP RECS                      *         
*                    OR SHIP RECAP RECS                               *         
*             AIO2 - COMMERCIAL RECS FOR COMML SEARCH                 *         
*                    STATION RECS IF FILTERING ON TYPE/AFFILIATE      *         
*             AIO3 - STORED STEXT FOR LETTERS                         *         
*             BLOCK - USED IN VOPTIONS                                *         
*                     LR RTN TO BUILD PTN TBL FOR EACH STAT           *         
*                     PRT RTN TO FIND COM REV, INS DATE TEL DATES     *         
*                     PRT RTN TO SAVE CMML TEXT KEYS                  *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 -                                                         *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - STATION TABLE POINTER                                   *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - 2ND BASE REGISTER                                       *         
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
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* LEV  2    OCT09/92 ADD MT3 FOR BACKER                               *         
* LEV  3    DEC22/92 FIX FOR MSUNPK AND STATION FILE                  *         
* LEV  4    JAN21/93 FIX FOR PRODUCT NOT ENTERED BUG                  *         
* LEV  5    JAN22/93 ADD COUNT OF STATIONS FOR COPY RUN               *         
* LEV  6    MAR30/93 CABLE HEAD                                       *         
* LEV  7    OCT26/93 ADD MT4 FOR BSNY                                 *         
* LEV  8    JAN11/94 ADD OT4 FOR BSNY                                 *         
* LEV  9    JAN19/94 ADD NEW TRAFFIC                                  *         
* LEV 10    MAR03/94 FIX SINGLE STATION, ALL PRODUCTS REQUEST         *         
* LEV 11    JUL21/94 CHANGE TO FILENAME                               *         
* LEV 12    AUG19/94 FIX CABLE FAX - SEND MEDIA STATION (T1234)       *         
* LEV 13    SEP27/94 USE DEALER INST & SPOT GEN RECAPS (X'20' & X'30')*         
*                    - PROBLEM WITH FILTERING ON COMML - RD ALL BUYS? *         
* LEV 14    FEB01/96 ADD MT6 FOR ZENITH                               *         
* LEV 15    MAR01/96 FIX RUN OVER AIO3 - USE STATBLE                  *         
* LEV 16    NOV08/96 FIX SVTW PROFILE ALLOWING 2                      *         
* LEV 17    APR28/97 DO FAX GEN REGARDLESS OF TW PROFILE              *         
* LEV 18    JUL03/97 BYPASS TBA'S                                     *         
* LEV 19    JUL25/97 ADJUST DATE PRINTING FOR YR2000                  *         
* LEV 20    OCT06/97 IF STATION SPECIFIC REQ, FIX NO PROD             *         
* LEV 21    NOV18/97 DO NOT MARK FILES, DO NOT READ RECAP RECS WHEN   *         
*                    LIST OF STATIONS IS ENTERED                      *         
* LEV 22    DEC01/97 SEND FAX = WIDE EDIWIDE=W                        *         
* LEV 23 SMUR SEP23/98 CALL VALPER INSTEAD OF VALIPER (MOVED TO 00)   *         
* LEV 24 SMUR APR11/01 USE TRAFFIC OFFICE                             *         
* LEV 25 BGRI MAY07/01 CHANGE DUMMY                                   *         
* LEV 27 BGRI NOV01/02 NEW INST RECAP RECS                            *         
* LEV 28 SMUR MAR23/06 USE DEFAULT TIME FOR PQ RETAINS                *         
* LEV 29 MNAS          ARCHIVE                                        *         
* LEV 31 SMUR JUL02/09 ALL ADID SUPPORT                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21667 FAX LETTER GENERATION'                                   
T21667   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21667**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR67RR                                                      
         L     RF,=V(PQPROF)                                                    
         AR    RF,R2                                                            
         ST    RF,PQPROF                                                        
         CLI   MODE,PRINTREP       REPORT                                       
         BE    LR                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY FIELDS ROUTINE                                                   
*                                                                               
VK       DS    0H                                                               
         MVI   SVQLTYP1,0          CLEAR ARCHIVE BYTE                           
         MVI   SVFAXARC,0          CLEAR FAX ARCHIVE BYTE                       
         XC    ELEM,ELEM                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'N'                                                     
         MVC   PROFKEY+1(2),=C'TF'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 PQPROF,DMCB,(X'80',PROFKEY),(0,ELEM),ACOMFACS                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,ELEM                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP1,REMOTTY1                                                
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   VK03                                                             
         CLI   REMOTSUB+1,C'N'                                                  
         BE    VK05                                                             
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   VK03                                                             
         OI    SVFAXARC,REMOTARQ                                                
         B     VK05                                                             
VK03     OI    SVFAXARC,REMOTAEQ                                                
VK05     DS    0H                                                               
         DROP  R4                                                               
                                                                                
         MVI   ASTATBLE,0                                                       
         LA    RE,ASTATBLE                                                      
         LA    R0,1(,RE)                                                        
         LA    RF,1                                                             
         LHI   R1,(ENDSYSD-ASTATBLE+1)                                          
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         BAS   RE,RPR           GO READ T0, T1, & TW PROFILES                   
*                                                                               
* VALIDATE PRODUCT *                                                            
*                                                                               
         LA    R2,TRAPRDH          PRODUCT CODE                                 
         XC    QPRD,QPRD                                                        
         MVI   BPRD,0                                                           
         XC    PRDNM,PRDNM                                                      
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VK10                                                             
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK        PRODUCT POL ENTERED                          
         BE    INVPOLER                                                         
*                                                                               
         MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
         MVC   PRDNM,WORK+5        SAVE PRODUCT NAME                            
*                                                                               
* VALIDATE PARTNER *                                                            
*                                                                               
VK10     LA    R2,TRAPTRH          PARTNER CODE                                 
*                                                                               
         XC    QPRD2,QPRD2                                                      
         MVI   BPRD2,0                                                          
         XC    PRD2NM,PRD2NM                                                    
*                                                                               
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VK20                                                             
         CLC   =C'NONE',8(R2)                                                   
         BNE   *+12                                                             
         MVI   BPRD2,X'FF'                                                      
         B     VK20                                                             
*                                                                               
         CLI   BPRD,X'FF'          WAS POL SELECTED                             
         BE    POLPTRER                                                         
*                                                                               
         CLI   BPRD,0              WAS PRODUCT ENTERED?                         
         BE    MISPRDER                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK        PRODUCT POL INVALID                          
         BE    INVPRDER                                                         
         MVC   QPRD2,WORK           SAVE EBCIC PRODUCT                          
         MVC   BPRD2,WORK+3         SAVE BINARY PRODUCT                         
         MVC   PRD2NM,WORK+5        SAVE PRODUCT NAME                           
*                                                                               
* EDIT STEXT *                                                                  
*                                                                               
VK20     LA    R2,TRASTXH                                                       
*                                                                               
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    MISSERR                                                          
*                                                                               
         BAS   RE,VSTX                                                          
*                                                                               
* VALIDATE OPTIONS                                                              
*                                                                               
         MVI   TABSW,0             CLEAR ALL FLAGS                              
*                                                                               
         LA    R2,TRAOPTH          OPTION VALIDATION                            
*                                                                               
         BRAS  RE,VOPT           VALIDATE OPTIONS                               
*                                                                               
         TM    TABSW,STALIST                                                    
         BZ    VK30                                                             
*                                                                               
         BAS   RE,VBSTA           VALIDATE STATIONS AND BUILD STA LIST          
*                                                                               
* EDIT ESTIMATE *                                                               
*                                                                               
VK30     LA    R2,TRAESTH                                                       
*                                                                               
         MVI   QBEST,0                                                          
*                                                                               
         BRAS  RE,VEST                                                          
*                                                                               
* EDIT PERIOD *                                                                 
*                                                                               
         LA    R2,TRAPERH                                                       
*                                                                               
         BRAS  RE,VPER                                                          
*                                                                               
* VALIDATE COMMERCIAL (IF ENTERED) *                                            
*                                                                               
         LA    R2,TRACMLH                                                       
*                                                                               
         XC    QCML,QCML                                                        
         XC    QCMLTLE,QCMLTLE                                                  
         MVI   QCMLSLN,0                                                        
*                                                                               
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    *+8                                                              
         BAS   RE,VCML                                                          
*                                                                               
         LA    R2,TRACONTH         CONTACT IS REQUIRED BUT NOT EDITED           
         GOTO1 ANY                                                              
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         XC    CONFAX,CONFAX                                                    
         MVC   QUESTOR(L'TRACONT),WORK                                          
*                                                                               
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   VK60                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
         MVC   CNTKNAME,12(R2)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK50                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOCONFND                                                         
*                                                                               
VK50     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   VK60                                                             
         USING CNTFAXEL,R6                                                      
         MVC   CONFAX,CNTFTEL                                                   
         DROP  R4,R6                                                            
*                                                                               
VK60     DS    0H                                                               
         OC    QUESTOR,SPACES      NEEDED IF FAXING                             
*                                                                               
         LA    R2,TRAMKTSH         OPTION VALIDATION                            
*                                                                               
         BRAS  RE,VMKT                                                          
*                                                                               
         LA    R2,TRASTAH                                                       
         XC    BMKTSTA,BMKTSTA                                                  
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 VALISTA                                                          
*                                                                               
         OC    MKTLIST,MKTLIST     IF MARKET LIST ENTERED, ERROR                
         BNZ   MKTLERR                                                          
*                                                                               
VK70     MVI   ERROR,0                                                          
         MVI   PQSW,1           TO SUPPRESS AUTO PRINTQ OPEN                    
*                                                                               
         CLI   OPTCOPY,C'Y'        THIS COPY ONLY PROCESSING                    
         BE    VKX                                                              
         TM    WHEN,X'20'          THIS SOON PROCESSING                         
         BZ    VKX                                                              
         MVC   REMUSER,=C'LET'                                                  
         MVC   SPOOLKEY+QLTYP1-PQPLD(1),SVFAXARC                                
         MVI   SPOOLKEY+PLCLASS-PQPLD,C'G'                                      
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
* READ RECAP RECORDS TO FIND HOW MANY FAX LETTERS TO SEND *                     
*                                                                               
LR       DS    0H                                                               
         TM    TABSW,STALIST       STATION LIST FROM SCREEN                     
         BO    LR150                                                            
*                                                                               
         BRAS  RE,INIT             INITIALIZE                                   
*                                                                               
* READ INSTRUCTION RECAP RECORDS FOR LETTERS FOR REQUESTED PERIOD               
*                                                                               
         LA    R4,KEY              BUILD KEY FOR READHI                         
         USING INSKEY,R4                                                        
         MVC   INSKID(2),=X'0A24'                                               
         MVC   INSKAM(4),BAGYMD   BCLT AND BPRD                                 
         CLI   BPRD,X'FF'          POL                                          
         BNE   *+8                                                              
         MVI   INSKPRD,0                                                        
         MVC   INSKMKT,MKTLIST                                                  
*                                                                               
         OC    BMKTSTA,BMKTSTA                                                  
         BZ    LR010                                                            
         MVC   INSKMKT(5),BMKTSTA                                               
         DROP  R4                                                               
*                                                                               
* REGISTER USAGE                                                                
* R2 - SUBEL POINTER                                                            
* R4 - INSTR RECAP KEY                                                          
* R5 - STATION POINTER - ENTRIES ARE BINARY MARKET STATION                      
* R6 - ELEM PTR                                                                 
*                                                                               
LR010    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY      ANY RECS FOR THIS A/M, BCLT, BPRD            
         BE    LR020                                                            
         CLI   BPRD,0              ALL PRODUCTS                                 
         BNE   NOINSER                                                          
         CLC   KEYSAVE(5),KEY      ANY RECS FOR THIS A/M, BCLT                  
         BNE   NOINSER                                                          
         B     LR022                                                            
*                                                                               
LR020    CLC   KEY(6),KEYSAVE      AT END OF THIS A/M BCLT AND BPRD             
         BE    LR022                NO                                          
         CLI   BPRD,0              PRODUCT REQUESTED                            
         BNE   LR150                YES, SET UP FOR REPORT                      
         CLC   KEYSAVE(5),KEY      AT END OF THIS A/M, BCLT                     
         BNE   LR150                YES, SET UP FOR REPORT                      
*                                                                               
LR022    OC    BMKTSTA,BMKTSTA    ONLY ONE STATION                              
         BZ    LR024                                                            
         CLC   BMKTSTA,KEY+INSKMKT-INSKEY                                       
         BE    LR024                                                            
         BH    LR023                                                            
*                                                                               
         CLI   BPRD,0              WAS A SPECIFIC PROD REQUESTED                
         BNE   LR150                DONE                                        
*                                                                               
         ZIC   R1,KEY+INSKPRD-INSKEY                                            
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+INSKPRD-INSKEY                                            
*                                                                               
LR023    MVC   KEY+INSKMKT-INSKEY(5),BMKTSTA                                    
         B     LR010                                                            
*                                                                               
LR024    CLI   QBEST,0             ANY ESTIMATE ENTERED                         
         BE    LR026                NO                                          
         CLC   QBEST,KEY+11        THIS FOR THIS EST                            
         BNE   LR084                                                            
*                                                                               
LR026    OC    MKTLIST,MKTLIST    ONLY REQUESTED MARKETS                        
         BZ    LR040                                                            
         LA    R0,10                                                            
         LA    R1,MKTLIST                                                       
LR028    CLC   0(2,R1),KEY+6      MARKET SELECTED                               
         BE    LR040                                                            
         BH    LR030                                                            
         CLI   2(R1),X'FF'        AT END OF TABLE                               
         BE    LR086                                                            
         LA    R1,2(,R1)                                                        
         BCT   R0,LR028                                                         
         B     LR086              GET NEXT                                      
*                                                                               
* JUMP OVER UNNEEDED MARKETS AND START WITH NEXT IN TABLE *                     
*                                                                               
LR030    CLI   0(R1),X'FF'        AT END OF TABLE                               
         BE    LR086                                                            
         MVC   KEY(5),KEYSAVE                                                   
         CLI   BPRD,0              ALL PRODUCTS                                 
         BE    *+10                 YES, LEAVE WHAT PROD WE FOUND               
         MVC   KEY+5(1),KEYSAVE+5                                               
*                                                                               
         OC    BMKTSTA,BMKTSTA    ONLY ONE STATION                              
         BZ    LR034                                                            
         MVC   KEY+INSKMKT-INSKEY(5),BMKTSTA                                    
         B     LR036                                                            
*                                                                               
LR034    MVC   KEY+6(2),0(R1)                                                   
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
LR036    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR020                                                            
*                                                                               
* SEE IF ONLY 1 MARKET GROUP REQUESTED *                                        
*                                                                               
LR040    OC    OPTMGRP,OPTMGRP                                                  
         BZ    LR050                                                            
         CLC   OPTMGRPM,KEY+6      THIS MARKET                                  
         BE    LR050                YES                                         
         BL    LR042                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+6(2),OPTMGRPM                                                
         XC    KEY+8(5),KEY+8                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR020                                                            
LR042    MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(6),OPTPGRP                                                 
         MVC   KEY+11(2),SVKEY+6   THIS MARKET                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     AT END OF MARKET GROUP                       
         BE    LR044                                                            
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     AT END OF MARKET GROUP                       
         BE    LR044                                                            
*                                                                               
         XC    FILENAME,FILENAME                                                
         B     LR150                                                            
*                                                                               
LR044    CLC   KEY(13),KEYSAVE     FIND THIS MARKET                             
         BE    LR046                                                            
         CLC   KEY+11(2),SVKEY+6   TO INSTR RECAP MARKET                        
         BNL   *+6                                                              
         DC    H'0'                                                             
LR046    MVC   OPTMGRPM,KEY+11                                                  
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+6(2),OPTMGRPM                                                
         XC    KEY+8(5),KEY+8                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR020                                                            
         EJECT                                                                  
* CHECK ALL PATTERNS FOR THIS STATION WITHIN PERIOD *                           
*                                                                               
LR050    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING INSKEY,R4                                                        
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR082                                                            
         USING INSDTAEL,R6                                                      
*                                                                               
* BYPASS EITHER COPY CODE=EST INSTR OR COPY CODE NOT EST INSTR *                
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODED EST ON                            
         BNE   LR056                                                            
         TM    INSFLAG,X'20'       IF COPY CODE = EST                           
         BO    LR060                USE IT                                      
         B     LR080                                                            
*                                                                               
LR056    TM    INSFLAG,X'20'       IF NOT COPY CODE = EST                       
         BO    LR080                BYPASS                                      
*                                                                               
LR060    CLI   BPRD2,0                                                          
         BE    LR064                                                            
         CLI   BPRD2,X'FF'         PARTNER NONE                                 
         BNE   *+16                                                             
         CLI   INSPRD2,0                                                        
         BNE   LR080                                                            
         B     LR064                                                            
         CLC   BPRD2,INSPRD2       REQUESTED PARTNER                            
         BNE   LR080                                                            
*                                                                               
LR064    LA    R2,INSPTTN                                                       
         ZIC   R0,INSDTALN                                                      
         AHI   R0,-INSBSCEL                                                     
LR066    CLC   SVFLTSTP,5(R2)      FLIGHT START TO LTD                          
         BH    LR074               NOT IN FLIGHT                                
         CLC   SVFLTEDP,3(R2)      FLIGHT END TO FTD                            
         BL    LR074               NOT IN FLIGHT                                
         OI    TABSW,FOUNDENT      FOUND ENTRY                                  
*                                                                               
* NOTE - BYPASS TBA'S AND HIATUS PATTERNS *                                     
*                                                                               
         CLI   ELCODE,X'20'                                                     
         BE    LR067                                                            
*                                                                               
         CLI   ELCODE,X'30'                                                     
         BE    LR067                                                            
*                                                                               
         CLC   =X'FFFFFF',0(R2)    THIS A TBA PATTERN                           
         BE    LR074                YES                                         
         OC    0(3,R2),0(R2)       THIS A HIATUS PATTERN                        
         BZ    LR074                YES                                         
         EJECT                                                                  
LR067    OC    QCML,QCML           FILTERING ON COMML                           
         BZ    LR068                                                            
*                                                                               
         BAS   RE,FCML             READ PATTERN REC, SEE IF COMML IN IT         
         BNE   LR074                                                            
*                                                                               
* NOW QUALIFIES, BUILD STATION ENTRY, DON'T MESS AROUND                         
*                                                                               
LR068    L     R5,ASTATBLE                                                      
*                                                                               
LR070    OC    0(6,R5),0(R5)       EMPTY ENTRY                                  
         BZ    LR072                                                            
         CLC   0(5,R5),INSKMKT                                                  
         BE    LR084                                                            
         LA    R5,6(,R5)                                                        
         C     R5,ASTAEND                                                       
         BL    LR070                                                            
         DC    H'0'                                                             
LR072    MVC   0(5,R5),INSKMKT                                                  
         OC    OPTSTA,OPTSTA                                                    
         BZ    *+8                                                              
         BAS   RE,CKSTA                                                         
*                                                                               
         B     LR084                                                            
*                                                                               
LR074    LA    R2,INSSUBEL(,R2)     CHECK NEXT SUB-ELEM                         
         AHI   R0,-INSSUBEL                                                     
         BP    LR066                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
LR080    BAS   RE,NEXTEL                                                        
         BE    LR060                                                            
*                                                                               
LR082    OC    QCML,QCML           FILTERING ON COMML                           
         BNZ   LR084                                                            
         L     R6,AIO1                                                          
         CLI   ELCODE,X'10'                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'20'                                                     
         B     LR083                                                            
         CLI   ELCODE,X'20'                                                     
         BNE   *+12                                                             
         BNE   LR084                                                            
         MVI   ELCODE,X'30'                                                     
         B     LR083                                                            
*                                                                               
         CLI   ELCODE,X'30'                                                     
         BE    LR084                                                            
         DC    H'0'                                                             
LR083    BAS   RE,GETEL                                                         
         BE    LR060                                                            
*                                                                               
LR084    MVC   KEYSAVE,KEY         SET FOR READ SEQUENTIAL                      
LR084A   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE     ALL SAME BUT COPY CODE                       
         BNE   LR086                                                            
         CLI   QBEST,0             ESTIMATE ENTERED                             
         BE    LR050                NO                                          
         CLC   QBEST,KEY+11        ONLY THIS EST                                
         BE    LR050                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
* CHECK FOR SPECIFIC STATION REQUEST *                                          
*                                                                               
LR086    OC    BMKTSTA,BMKTSTA    ONLY ONE STATION                              
         BZ    LR088                                                            
         CLC   BMKTSTA,KEY+INSKMKT-INSKEY                                       
         BE    LR050                                                            
         B     LR150                                                            
*                                                                               
* CHECK FOR SPECIFIC MARKET(S) REQUEST *                                        
*                                                                               
LR088    OC    MKTLIST,MKTLIST    ONLY SELECTIVE MARKETS                        
         BZ    LR020                                                            
         LA    R0,10                                                            
         LA    R1,MKTLIST                                                       
*                                                                               
LR090    CLC   0(2,R1),KEY+6      MARKET SELECTED                               
         BE    LR020                                                            
         LA    R1,2(,R1)                                                        
         CLI   0(R1),X'FF'        AT END OF TABLE                               
         BE    *+8                                                              
         BCT   R0,LR090                                                         
*                                                                               
         CLI   BPRD,X'FF'          POL REQUEST                                  
         BNE   LR140                                                            
         MVI   KEY+6,X'FF'         FORCE NEXT PRODUCT                           
         GOTO1 HIGH                                                             
         B     LR020                                                            
*                                                                               
LR140    SH    R1,=H'2'                                                         
         CLC   0(2,R1),KEY+6                                                    
         BL    LR150                                                            
         B     LR084A                                                           
         EJECT                                                                  
* SEE IF ANY LETTERS TO PRINT *                                                 
*                                                                               
LR150    L     R1,ASTATBLE                                                      
         OC    0(5,R1),0(R1)      END OF TABLE                                  
         BZ    NOINSER                                                          
*                                                                               
* COUNT STATIONS FOR SORT *                                                     
*                                                                               
         SR    R0,R0                                                            
         SR    R2,R2                                                            
LR160    OC    0(5,R1),0(R1)      END OF TABLE                                  
         BZ    LR170                                                            
*                                                                               
         CLI   5(R1),0             THIS STATION FAIL FILTER                     
         BNE   *+6                                                              
         BCTR  R0,0                COUNT AS GOOD                                
*                                                                               
         LA    R1,6(,R1)                                                        
         BCT   R2,LR160                                                         
*                                                                               
LR170    LTR   R0,R0                                                            
         BZ    NOINSER                                                          
         LTR   R2,R2                                                            
         BZ    NOINSER                                                          
         LPR   R2,R2                                                            
         L     R5,ASTATBLE                                                      
*                                                                               
         GOTO1 XSORT,DMCB,(R5),(R2),6,5,0                                       
         EJECT                                                                  
* READ SPECIAL TEXT RECORD(S) *                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVI   KEY+5,C'='                                                       
         MVC   KEY+6(6),STEXTK                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3             SAVE AREA FOR TEXT                           
         LA    R0,2000(R4)         END OF AREA                                  
         ST    R0,AENDSTX                                                       
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   PST10                                                            
         LA    R4,STATABLE                                                      
         LR    R0,R4                                                            
         AH    R0,=H'5000'                                                      
         ST    R0,AENDSTX                                                       
PST10    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PST20    BAS   RE,NEXTEL                                                        
         BNE   PST30                                                            
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,PSTEX                                                         
         LA    R4,1(RE,R4)                                                      
         C     R4,AENDSTX                                                       
         BL    PST20                                                            
         DC    H'0'                                                             
PSTEX    MVC   0(0,R4),0(R6)                                                    
*                                                                               
PST30    MVI   ELCODE,X'50'        TEST FOR CONTINUATION ELEM                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PST40                                                            
         ZIC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PST10                                                            
         MVI   0(R4),0             MARK END OF STEXT                            
         EJECT                                                                  
PST40    CLI   OPTCOPY,C'Y'        ONLY PRINT COPY LIST                         
         BE    COPY                                                             
*                                                                               
         TM    WHEN,X'20'          TEST SOON                                    
         BO    LRR010               YES                                         
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   LRR010               NO                                          
*                                                                               
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PQSW,1              SET PRINT QUE NEEDS TO BE OPENED             
*                                                                               
* SAVE REMOTE PRINT QUE SETTINGS *                                              
*                                                                               
LRR010   L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   LRR014               NO                                          
*                                                                               
*                                                                               
         OC    REMOTKEY,REMOTKEY     PRINTING REMOTE                            
         BNZ   *+6                    BETTER BE                                 
         DC    H'0'                                                             
*                                                                               
         MVC   SVMOTCPY,REMOTCPY                                                
         MVC   SVMOTCLS,REMOTCLS                                                
         MVC   SVMOTJID,REMOTJID                                                
*                                                                               
*                                                                               
*  FAXING LETTERS - MUST USE GLASS G AND SEND SPECIAL PRINT LINE FIRST          
*                                                                               
LRR014   MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'LET'                                                 
         MVC   REMOTTY1,SVFAXARC                                                
*                                                                               
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
         DROP  RF                                                               
         MVC   SVREMUSR,REMUSER                                                 
         MVC   REMUSER,=C'LET'                                                  
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
         XC    ELEM,ELEM                                                        
         MVC   QLDESC(1),QMED                                                   
         MVC   QLDESC+3(3),QCLT                                                 
         MVC   QLDESC+6(3),QPRD                                                 
         MVI   PLCLASS,C'G'        WESTERN UNION                                
         MVI   QLCLASS,C'G'        ALSO                                         
         MVC   QLTYP1,SVFAXARC                                                  
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
         EJECT                                                                  
*                                                                               
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN) *                          
*                                                                               
* CLASS G SENDS A PRINT LINE WITH FIXED DEST (STATION CALL LETTERS) *           
*                                                                               
PRTLOOP  CLI   5(R5),0             STATION FAIL FILTER                          
         BNE   HDG50                                                            
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
         GOTO1 MSUNPK,DMCB,(X'80',(R5)),QMKT,WORK                               
*                                                                               
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES    CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,WORK                                                        
         BAS   RE,FSTA                                                          
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=CL5'*HDR*'                                               
*                                                                               
         MVI   EDIWIDE,C'W'                                                     
*                                                                               
         MVC   EDIDESID(5),QSTA                                                 
         MVC   EDIFDEST(7),STAPRNT                                              
*                                                                               
         CLI   QSTA,C'0'                                                        
         BL    *+16                                                             
         MVC   EDIDESID(1),QSTA+4                                               
         MVC   EDIDESID+1(4),QSTA                                               
*                                                                               
         MVC   EDIBILL(1),QMED    MEDIA                                         
         MVC   EDIBILL+1(3),QCLT    CLIENT                                      
         MVC   EDIBILL+4(3),QPRD    PRODUCT                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPFRATRN'                                  
*                                                                               
         MVC   EDISTTMD(4),QMED    MEDIA & CLIENT                               
         MVC   EDISTTPR,QPRD                                                    
         MVC   EDISTTP2,QPRD2                                                   
         MVC   EDISTTES,=CL3'NO '  SET EST = NO                                 
         CLI   QBEST,0                                                          
         BE    EDI20                                                            
         ZIC   R0,QBEST            UNLESS EST IS USED                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDISTTES,DUB                                                     
EDI20    GOTO1 DATCON,DMCB,(3,SVGENST),(X'20',EDISTTDT)                         
         GOTO1 (RF),(R1),(3,SVGENEND),(X'20',EDISTTDT+6)                        
         MVC   EDISTTST,QSTA                                                    
         MVC   EDISTTCT,QUESTOR                                                 
*                                                                               
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVI   CLEARHED,C'N'       DO NOT CLEAR HEADLINES                       
         MVI   FORCEHED,C'N'       FORCE HEADLINES                              
         MVC   PAGE,=H'1'                                                       
*                                                                               
* BLANK THE HEADLINES NOW *                                                     
*                                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
*                                                                               
         CLC   AGENCY,=C'TH'       THIS ZENITH                                  
         BE    PRT06                                                            
         CLC   AGENCY,=C'BS'       THIS BACKER                                  
         BNE   PRT20                                                            
PRT06    CLC   QCLT,=C'MT2'        AND CLIENT MT2                               
         BE    PRT10                                                            
         CLC   QCLT,=C'MT3'        AND CLIENT MT3                               
         BE    PRT10                                                            
         CLC   QCLT,=C'MT4'        AND CLIENT MT4 - MILLER                      
         BE    PRT10                                                            
         CLC   QCLT,=C'MT6'        AND CLIENT MT6 - MILLER                      
         BE    PRT10                                                            
         CLC   QCLT,=C'OT4'        AND CLIENT OT4 - MOLSON                      
         BNE   PRT20                                                            
*                                                                               
PRT10    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING STAMASD,R6                                                       
*                                                                               
         XC    0(17,R6),0(R6)                                                   
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         MVC   KEY(17),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(9),0(R6)                                                     
         BNE   MISTAER                                                          
         MVC   QMKT,SMKT                                                        
         DROP  R6                                                               
*                                                                               
PRT20    CLC   LASTMKT,QMKT                                                     
         BE    PRT30                                                            
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         XC    0(17,R6),0(R6)                                                   
         MVI   0(R6),C'0'                                                       
         MVC   1(16,R6),0(R6)                                                   
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
         MVC   KEY(17),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(8),0(R6)                                                     
         BE    *+10                                                             
         MVC   MKTNAME,=C'** UNKNOWN MARKET **'                                 
         MVC   MKTNM,MKTNAME                                                    
         MVC   LASTMKT,QMKT                                                     
         DROP  R6                                                               
         EJECT                                                                  
* NOW PRINT FAX LETTER *                                                        
*                                                                               
PRT30    MVI   P1,C'*'                                                          
         MVC   P1+1(5),P1                                                       
         MVI   SPACING,3                                                        
*                                                                               
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)    SEND SPACING BETWEEN INSTR LETTER             
*                                                                               
HDG06    MVI   P1+99,C'1'                                                       
         MVC   P1(2),=C'TO'                                                     
         MVC   P1+9(7),STAPRNT                                                  
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   P1+9(8),STANET                                                   
*                                                                               
         MVC   P1+20(12),=C'TRAFFIC DEPT'                                       
         GOTO1 DATCON,DMCB,(5,0),(8,P1+41)                                      
         MVC   P1+50(2),=C'AT'                                                  
         EDIT  (TIME,NOW),(8,P1+53)                                             
*                                                                               
         MVC   P2(4),=C'FROM'                                                   
         MVC   P2+9(33),USERNAME                                                
         MVC   P3+9(33),USERADDR                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   HDG10                                                            
         LH    RE,SEQNUM                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+57(4),DUB                                                     
         LA    RE,1(RE)                                                         
         STH   RE,SEQNUM                                                        
*                                                                               
HDG10    MVC   P4(7),=C'CONTACT'                                                
         MVC   P4+9(L'QUESTOR),QUESTOR                                          
*                                                                               
         OC    CONTEL,CONTEL       TEL #                                        
         BZ    HDG12                NO                                          
         MVC   P4+45(12),CONTEL                                                 
         OC    CONTEL+13(5),CONTEL+13  EXT?                                     
         BZ    HDG12                    NO                                      
         MVI   P4+59,C'X'                                                       
         MVC   P4+61(5),CONTEL+13                                               
*                                                                               
HDG12    MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    CONFAX,CONFAX       FAX #                                        
         BZ    HDG14                NO                                          
         MVC   P1+41(3),=C'FAX'                                                 
         MVC   P1+45(12),CONFAX                                                 
         OC    CONFAX+13(5),CONFAX+13  EXT?                                     
         BZ    HDG14                    NO                                      
         MVI   P1+59,C'X'                                                       
         MVC   P1+61(5),CONFAX+13                                               
*                                                                               
HDG14    MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P1(6),=C'CLIENT'                                                 
         MVC   P1+12(3),QCLT                                                    
         MVC   P1+22(20),CLTNM                                                  
*                                                                               
         MVC   P2(7),=C'PRODUCT'                                                
         CLI   BPRD,0              TEST PRODUCT REQUEST                         
         BE    HDG30                NO                                          
*                                                                               
         MVC   P2+12(3),QPRD                                                    
         MVC   P2+22(20),PRDNM                                                  
*                                                                               
* PARTNER PRODUCT REQUEST *                                                     
*                                                                               
         CLI   BPRD2,0              TEST PARTNER REQUEST                        
         BE    HDG30                NO                                          
         MVC   P3(7),=C'PARTNER'                                                
         CLI   BPRD2,X'FF'          TEST PRODUCT NONE REQUEST                   
         BE    HDG20                YES                                         
*                                                                               
         MVC   P3+12(3),QPRD2                                                   
         MVC   P3+22(20),PRD2NM                                                 
         B     HDG30                                                            
HDG20    MVC   P3+12(4),=C'NONE'                                                
*                                                                               
HDG30    MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    QCML,QCML           REQUEST BY COMMERCIAL                        
         BZ    HDG40                                                            
         MVC   P1(10),=C'COMMERCIAL'                                            
         MVC   P1+12(12),QCML                                                   
         MVC   P1+26(15),QCMLTLE                                                
*                                                                               
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
HDG40    MVC   P1(6),=C'MARKET'                                                 
         MVC   P1+12(4),QMKT                                                    
         MVC   P1+22(24),MKTNM                                                  
         EJECT                                                                  
         CLI   QBEST,0             INSTR BY EST                                 
         BE    HDG42                                                            
         MVC   P2(3),=C'EST'                                                    
         EDIT  (B1,QBEST),(3,P2+13)                                             
*                                                                               
         CLI   SVT1PR14,C'Y'       PRINT EST DESC                               
         BNE   HDG42                                                            
         MVC   P2+22(20),QESTDESC                                               
*                                                                               
HDG42    MVI   LINE,2                                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* FORMAT STANDARD TEXT TO PRINT LINES *                                         
*                                                                               
         L     R4,AIO3             FORMATTED COMMENTS ARE HERE                  
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   HDG44                                                            
         LA    R4,STATABLE                                                      
*                                                                               
HDG44    CLI   0(R4),0             TEST REACHED END OF COMMENTS                 
         BE    HDG46                                                            
         ZIC   RE,1(R4)                                                         
         LR    RF,RE                                                            
         SH    RF,=H'4'                                                         
         EX    RF,HDGMVC                                                        
         LA    R4,0(RE,R4)                                                      
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     HDG44                                                            
HDGMVC   MVC   P+1(0),3(R4)                                                     
*                                                                               
HDG46    MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
HDG50    LA    R5,6(,R5)                                                        
         OC    0(5,R5),0(R5)       END OF STATION LIST                          
         BNZ   PRTLOOP                                                          
*                                                                               
         TM    TABSW,STALIST                                                    
         BZ    *+12                                                             
         TM    WHEN,X'20'          THIS A SOON RUN                              
         BO    SOONREQ              YES                                         
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE PRINT QUE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PQSW,1                                                           
*                                                                               
* RESTORE ORIGINAL REQUESTED PRINT QUE STUFF *                                  
*                                                                               
         MVC   REMUSER,SVREMUSR                                                 
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   REPTMSG              NO, NO RESTORE                              
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(,RE)                                           
         USING REMOTED,RF                                                       
*                                                                               
         OC    SVMOTSTR,SVMOTSTR     SWITCHED QUE TO G                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   REMOTCPY,SVMOTCPY                                                
         MVC   REMOTCLS,SVMOTCLS                                                
         MVC   REMOTJID,SVMOTJID                                                
         DROP  RF                                                               
         B     EXIT                                                             
*                                                                               
* PUT SUMMARY REPORT HERE - SHOW SELECTION CRITERIA, THEN ALL MARKETS           
* AND STATIONS FAXED                                                            
*                                                                               
COPY     L     R5,ASTATBLE                                                      
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   PAGE,=H'1'                                                       
         XC    LASTMKT,LASTMKT                                                  
         MVI   STASW,C'Y'                                                       
*                                                                               
         SR    R3,R3               ZERO COUNTER                                 
*                                                                               
COPY10   CLI   5(R5),1              FAIL FILTER TEST                            
         BE    COPY26                                                           
*                                                                               
         LA    R3,1(,R3)           ADD TO COUNT                                 
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R5)),QMKT,WORK                               
*                                                                               
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES    CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
*                                                                               
         MVC   QSTA,WORK                                                        
*                                                                               
         CLC   LASTMKT,QMKT                                                     
         BE    COPY20                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         XC    0(17,R6),0(R6)                                                   
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
         MVC   KEY(17),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(8),0(R6)                                                     
         BE    *+10                                                             
         MVC   MKTNAME,=C'** UNKNOWN MARKET **'                                 
*                                                                               
         MVC   MKTNM,MKTNAME                                                    
         MVC   LASTMKT,QMKT                                                     
         DROP  R6                                                               
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
COPY20   OC    OPTDMGRP,OPTDMGRP     WAS MARKET GROUP FILTER USED               
         BZ    COPY24                                                           
         MVC   PMGRP,OPTDMGRP                                                   
*                                                                               
COPY24   BAS   RE,FSTA                                                          
         MVC   PSTA,STAPRNT                                                     
*                                                                               
         OC    STANET,STANET       CABLE HEAD                                   
         BZ    *+10                                                             
         MVC   PSTA(8),STANET                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
COPY26   LA    R5,6(,R5)                                                        
         OC    0(5,R5),0(R5)       AT END OF STATIONS                           
         BNZ   COPY10                                                           
*                                                                               
         MVI   STASW,C'N'          SET OFF PRINTING STATIONS FOR HDHK           
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PSTA-27(19),=C'STATIONS NOTIFIED ='                              
         EDIT  (R3),(7,PSTA-7),COMMAS=YES,ALIGN=LEFT                            
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   ALLOWLIN,5                                                       
*                                                                               
         MVC   P+25(22),=C'M E S S A G E  S E N T'                              
*                                                                               
         MVC   P2+25(22),=C'----------------------'                             
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R4,AIO3             FORMATTED COMMENTS ARE HERE                  
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BNE   COPY30                                                           
         LA    R4,STATABLE                                                      
*                                                                               
*                                                                               
COPY30   CLI   0(R4),0             TEST REACHED END OF COMMENTS                 
         BE    COPY40                                                           
         ZIC   RE,1(R4)                                                         
         LR    RF,RE                                                            
         SH    RF,=H'4'                                                         
         EX    RF,HDGMVC                                                        
         LA    R4,0(RE,R4)                                                      
         MVI   LINE,2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     COPY30                                                           
*                                                                               
COPY40   MVC   P+21(33),=C'**  E N D  O F  M E S S A G E  **'                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTX     B     EXIT                                                             
*                                                                               
* GENERATE A SOON REQUEST FOLLOW UP COPY RUN *                                  
*                                                                               
SOONREQ  XC    ELEM,ELEM                                                        
         ZIC   R2,TRAOPTH+5                                                     
         LA    R3,TRAOPT(R2)                                                    
         LTR   RE,R2                                                            
         BZ    SOONRQ10                                                         
         MVI   0(R3),C','                                                       
         LA    RE,1(,RE)                                                        
         LA    R3,1(,R3)                                                        
*                                                                               
SOONRQ10 MVC   0(4,R3),=C'COPY'                                                 
         LA    RE,4(,RE)                                                        
         STC   RE,TRAOPTH+5                                                     
         MVC   ELEM,SPACES                                                      
         MVI   ELEM+15,X'01'     GENERATE LINKED REQUESTS                       
*                                                                               
         MVC   ELEM+26(2),=C'TF'                                                
         MVC   ELEM+28(2),AGENCY                                                
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+30(6),MCREQREC+5-MASTD(RE)                                  
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 REQTWA,DMCB,(0,ATWA),ELEM,DATAMGR,RCCOMFAC,WORK                  
*                                                                               
         MVC   0(5,R3),SPACES                                                   
         STC   R2,TRAOPTH+5                                                     
         B     EXIT                                                             
         EJECT                                                                  
* GET T0 & T1 PROFILES *                                                        
*                                                                               
RPR      NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* READ T1 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),WORK,SVT1PROF,DATAMGR                                  
*                                                                               
RPREXIT  B     EXIT                                                             
*                                                                               
* VALIDATE STATION LIST AND BUILD STATION TABLE                                 
*                                                                               
VBSTA    NTR1                                                                   
*                                                                               
         BRAS  RE,INIT             INITIALIZE                                   
*                                                                               
         LA    R2,TRASTAH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    NOSTAERR                                                         
*                                                                               
         LA    R3,TRASTALH         LAST STATION FIELD ON THE SCREEN             
*                                                                               
VBSTA10  XC    BMKTSTA,BMKTSTA                                                  
*                                                                               
         GOTO1 VALISTA                                                          
*                                                                               
         L     R5,ASTATBLE                                                      
VBSTA20  OC    0(5,R5),0(R5)       ANYTHING THERE?                              
         BZ    VBSTA40             GO ADD TO TABLE                              
         CLC   BMKTSTA,0(R5)       DUPLICATE ENTRY?                             
         BE    DUPSTERR                                                         
         LA    R5,6(R5)            MKT-2/STA-3/FAIL FLTR-1                      
         C     R5,ASTAEND                                                       
         BL    VBSTA20                                                          
         DC    H'0'                                                             
*                                                                               
VBSTA40  MVC   0(5,R5),BMKTSTA     SAVE MARKET/STATION IN TABLE                 
*                                                                               
VBSTA50  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         CR    R2,R3               END OF SCREEN?                               
         BH    VBSTAX               YES, DONE                                   
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VBSTA10             GO VALIDATE NEXT STATION                     
*                                                                               
         B     VBSTA50             BUMP TO NEXT STATION FIELD                   
*                                                                               
VBSTAX   XIT1                                                                   
*                                                                               
* VALIDATE STEXT FOR REQUEST                                                    
*                                                                               
VSTX     NTR1                                                                   
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVI   KEY+5,C'='                                                       
         MVC   KEY+6(6),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   NOTEXTER                                                         
         MVC   STEXTK,WORK                                                      
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE COMMERCIAL FOR REQUEST                                               
*                                                                               
VCML     NTR1                                                                   
         CLI   5(R2),8             8 CHAR ISCII                                 
         BNE   VCML06                                                           
*                                                                               
         LA    R0,8                                                             
         LA    R1,TRACML                                                        
VCML02   CLI   0(R1),C'A'                                                       
         BL    VCML04              AND SPECIAL CHAR, NO TRPACK                  
         LA    R1,1(R1)                                                         
         BCT   R0,VCML02                                                        
         B     VCML06                                                           
*                                                                               
VCML04   CLI   SVT1PR10,C'A'       ALLOW ALL NON-STD CML T1 PR 10 ALL           
         BE    VCML10                                                           
         CLI   SVT1PR10,C'Y'       ALLOW NON-STD COML CODES RADIO ONLY          
         BNE   VCMLERR                                                          
         CLI   QMED,C'T'           NOT ALLOWED FOR TV                           
         BE    VCMLERR                                                          
         B     VCML10                                                           
*                                                                               
VCML06   MVC   DMCB+4(4),=X'D9000AFE' GET ADDRESS OF TRPACK                     
         GOTO1 CALLOV,DMCB                                                      
         L     RF,0(R1)                                                         
*                                                                               
         OC    8(L'TRACML,R2),SPACES                                            
*                                                                               
         GOTO1 (RF),DMCB,(C'P',8(R2)),SVPCML  PACKED 8-12 CHAR CML              
         BNE   VCMLERR                                                          
*                                                                               
         CLI   5(R2),8             COMMERCIAL MUST BE 8 TO                      
         BE    VCML10                                                           
         CLI   5(R2),12            12 CHARACTERS                                
         BH    CMLENER                                                          
*                                                                               
         MVC   SVCML,SVPCML        MOVE IN PACKED CML                           
         B     *+10                                                             
VCML10   MVC   SVCML,8(R2)         8 CHAR ISCII                                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,SVCML                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML15                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVI   KEY+1,X'C1'         READ 0AC1 KEY                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
*                                                                               
VCML15   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BNZ   CMLDELER                                                         
         CLC   CMLRLSE,SVFLTEND    SEE IF THIS CMML WILL START IN TIME          
         BH    CMLDTER              NO, CMLRLSE AFTER PAT START                 
         CLC   CMLRCL,SVFLTSTR     SEE IF THIS CMML WILL LAST THRU PAT          
         BL    CMLDTER                                                          
*                                                                               
         MVC   QCMLSLN,CMLSLN      SAVE SPOT LEN                                
         MVC   QCMLTLE,CMLTITLE    SAVE COMMERCIAL TITLE                        
*                                                                               
         CLI   BPRD,0              FILTERING ON PRODUCT                         
         BE    VCML40               NO                                          
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
         CLC   =XL3'2003FF',CMLPRDEL   IS THIS COMML PRD=ALL                    
         BE    VCML40                   YES, COVERS ALL PRODUCTS                
         ZIC   R0,CMLPRDLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
         LR    RE,R0                                                            
         LA    R1,CMLPRDS          START OF PROD LIST                           
VCML30   CLC   BPRD,0(R1)          MATCH TO PROD                                
         BE    VCML40               YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,VCML30                                                        
         B     MATPRDER                                                         
VCML40   MVC   QCML,8(R2)                                                       
         OC    QCML,SPACES                                                      
         B     EXIT                                                             
         EJECT                                                                  
* READ PATTERN RECORD TO SEE IF REQUESTED COMMERCIAL IS IN PATTERN *            
*                                                                               
FCML     NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(3),BAGYMD AND BCLT                                        
         MVC   PATKPRD(4),INSPRD1-INSDTAEL(R6)                                  
         MVC   PATKCODE,SVKEY+INSKCOPY-INSKEY                                   
         MVC   PATKREF,0(R2)                                                    
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6                                                            
         L     R6,AIO2             USE IO2 FOR PATTERN REC                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         ZIC   R3,ELCODE                                                        
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         SRL   R0,4                                                             
         LA    R1,2(,R6)                                                        
FCML20   CLC   SVCML,0(R1)                                                      
         BE    FCML30                                                           
         CLC   SVPCML,0(R1)        8 CHAR PACKED                                
         BE    FCML30                                                           
         LA    R1,16(,R1)                                                       
         BCT   R0,FCML20                                                        
*                                                                               
         MVC   KEY,SVKEY                                                        
         STC   R3,ELCODE                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CR    RB,RD                                                            
         B     EXIT                                                             
*                                                                               
FCML30   MVC   KEY,SVKEY                                                        
         STC   R3,ELCODE                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* READ STATION RECORD, AND SEE IF MATCHES TYPE/AFFILIATE *                      
*                                                                               
CKSTA    NTR1                                                                   
*                                                                               
         MVC   SVKEY,KEY                                                        
         L     R6,AIO2                                                          
         USING STAMASD,R6                                                       
*                                                                               
         XC    0(17,R6),0(R6)                                                   
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         GOTO1 MSUNPK,DMCB,(R5),WORK,WORK+4                                     
*                                                                               
         MVC   STAKCALL,WORK+4                                                  
         CLI   STAKCALL+4,C' '                                                  
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         MVC   KEY(17),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(9),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    OPTAFFL,OPTAFFL     FILTERING ON AFFILIATE                       
         BZ    CKSTA10                                                          
         CLC   OPTAFFL,SNETWRK                                                  
         BE    *+8                                                              
         MVI   5(R5),1                                                          
*                                                                               
CKSTA10  CLI   OPTTYPE,0           FILTERING ON STATION TYPE                    
         BE    CKSTA20                                                          
         CLC   OPTTYPE,STYPE                                                    
         BE    *+8                                                              
         MVI   5(R5),1                                                          
CKSTA20  MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
*                                                                               
FSTA     NTR1                                                                   
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         LA    RF,STAPRNT+3                                                     
         CLI   0(RF),C' '                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         MVC   2(1,RF),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RF),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    FSTA10                                                           
         MVI   3(RF),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    FSTA10                                                           
         MVI   3(RF),C' '                                                       
*                                                                               
FSTA10   B     EXIT                                                             
*                                                                               
* HEADING ROUTINE FOR COPY REPORT *                                             
*                                                                               
HDHK     NTR1                                                                   
         MVC   H3+11(1),QMED                                                    
         MVC   H3+16(L'MEDNM),MEDNM                                             
         MVC   H4+11(L'QCLT),QCLT                                               
         MVC   H4+16(L'CLTNM),CLTNM                                             
*                                                                               
*        CLI   OPTCOPY,C'Y'        WAS THIS A COPY RUN ONLY                     
*        BNE   *+10                                                             
*        MVC   H3+29(18),=C'COPY LIST RUN ONLY'                                 
*                                                                               
         LA    R4,H5                                                            
         CLI   BPRD,0                                                           
         BE    HDHK10                                                           
         MVC   2(7,R4),=C'PRODUCT'                                              
         MVC   11(3,R4),QPRD                                                    
         MVC   16(20,R4),PRDNM                                                  
         LA    R4,132(,R4)                                                      
*                                                                               
HDHK10   CLI   BPRD2,0                                                          
         BE    HDHK20                                                           
         MVC   2(7,R4),=C'PARTNER'                                              
         MVC   11(3,R4),QPRD2                                                   
         MVC   16(20,R4),PRD2NM                                                 
         LA    R4,132(,R4)                                                      
*                                                                               
HDHK20   CLI   QBEST,0                                                          
         BE    HDHK30                                                           
         MVC   2(8,R4),=C'ESTIMATE'                                             
         EDIT  (B1,QBEST),(3,11(R4))                                            
*                                                                               
* MAY PUT IN AS TW PROFILE IF REQUESTED *                                       
*                                                                               
         CLI   SVT1PR14,C'Y'       PRINT EST DESC                               
         BNE   HDHK26                                                           
*                                                                               
         MVC   16(20,R4),QESTDESC                                               
*                                                                               
HDHK26   LA    R4,132(,R4)                                                      
*                                                                               
HDHK30   OC    QCML,QCML           FILTERING ON COMML                           
         BZ    HDHK40                                                           
         MVC   2(5,R4),=C'COMML'                                                
         MVC   11(12,R4),QCML                                                   
         MVC   25(15,R4),QCMLTLE                                                
         LA    R4,132(,R4)                                                      
*                                                                               
HDHK40   LA    R4,132(,R4)                                                      
         OC    OPTDMGRP,OPTDMGRP     WAS MARKET GROUP FILTER USED               
         BZ    HDHK50                                                           
         MVC   7(6,R4),=C'MGROUP'                                               
         MVC   7+132(6,R4),=C'------'                                           
HDHK50   MVC   20(6,R4),=C'MARKET'                                              
         MVC   20+132(6,R4),=C'------'                                          
         MVC   33(11,R4),=C'MARKET NAME'                                        
         MVC   33+132(11,R4),=C'-----------'                                    
         MVC   67(7,R4),=C'STATION'                                             
         MVC   67+132(7,R4),=C'-------'                                         
*                                                                               
         CLI   STASW,C'Y'          PRINTING STATIONS                            
         BNE   EXIT                                                             
         CLI   OPTCOPY,C'Y'        THIS COPY ONLY PROCESSING                    
         BNE   EXIT                                                             
*                                                                               
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
MISTAER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'* ERROR * NO STATION MASTER FOR'                  
         MVC   CONHEAD+32(4),QSTA                                               
         MVI   CONHEAD+36,C'-'                                                  
         MVC   CONHEAD+37(1),QSTA+4                                             
         MVI   CONHEAD+39,C'*'                                                  
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
REPTMSG  XC    CONHEAD,CONHEAD                                                  
*                                                                               
         MVC   CONHEAD(6),=C'REPORT'                                            
         MVC   CONHEAD+7(3),=C'LET'                                             
         MVI   CONHEAD+10,C','                                                  
         LA    R4,CONHEAD+11                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
*                                                                               
         MVC   1(16,R4),=C'HAS BEEN SPOOLED'                                    
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
MKTLERR  L     R1,=A(MKTLMS)                                                    
         B     ERREXIT                                                          
*                                                                               
NOTEXTER L     R1,=A(NOTEXTMS)                                                  
         B     ERREXIT                                                          
*                                                                               
CMLDELER L     R1,=A(CMLDELMS)                                                  
         B     ERREXIT                                                          
*                                                                               
CMLDTER  L     R1,=A(CMLDTEMS)     COMERCIAL DATES NOT IN PERIOD                
         B     ERREXIT                                                          
*                                                                               
POLPTRER L     R1,=A(POLPTRMS)                                                  
         B     ERREXIT                                                          
*                                                                               
INVPOLER L     R1,=A(INVPOLMS)                                                  
         B     ERREXIT                                                          
NOINSER  L     R1,=A(NOINSMSA)                                                  
         LA    R2,TRAMEDH                                                       
*                                                                               
ERREXIT  A     R1,SPTR67RR                                                      
         MVC   CONHEAD,0(R1)                                                    
ERRXIT2  GOTO1 ERREX2                                                           
*                                                                               
INVPRDER MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
NOCONFND MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
VCMLERR  MVI   ERROR,INVCOMM       NO COMERCIAL FOUND FOR CLIENT                
         B     TRAPERR                                                          
CMLENER  MVI   ERROR,INVCMMLN      COMERCIAL MUST BE 8 CHARACTERS LONG          
         B     TRAPERR                                                          
MATPRDER MVI   ERROR,CMLPRDER      REQUESTED PROD NOT COVERED BY COMML          
         B     TRAPERR                                                          
*                                                                               
MISPRDER LA    R2,TRAPRDH                                                       
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
NOSTAERR LA    R2,TRASTAH                                                       
         MVC   CONHEAD,NOSTAMS                                                  
         B     ERRXIT2                                                          
*                                                                               
DUPSTERR DS    0H                                                               
         MVC   CONHEAD,DUPSTMS                                                  
         B     ERRXIT2                                                          
*                                                                               
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
*                                                                               
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,25,C'FAX LETTER DISTRIBUTION LIST'                            
         SSPEC H1,63,AGYNAME                                                    
         SSPEC H3,3,C'MEDIA'                                                    
         SSPEC H2,25,C'----------------------------'                            
         SSPEC H2,63,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,63,REPORT                                                     
         SSPEC H4,75,RUN                                                        
         SSPEC H5,63,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         DC    X'00'               END MARKER FOR SSPECS                        
*                                                                               
         EJECT                                                                  
*                                                                               
NOCMLSCH DC    CL32' ** NO COMMERCIALS SCHEDULED **'                            
DUPSTMS  DC    CL60'* ERROR * DUPLICATE STATION ENTERED *'                      
NOSTAMS  DC    CL60'* ERROR * ENTER STATIONS *'                                 
MKTLMS   DC    CL60'* ERROR * WHY MARKET LIST IF STATION SPECIFIC ? *'          
CMLDELMS DC    CL60'* ERROR * COMMERCIAL IS DELETED *'                          
CMLDTEMS DC    CL60'* ERROR * COMMERCIAL DATES NOT IN REQUEST PERIOD *'         
NOTEXTMS DC    CL60'* ERROR * NO SPECIAL TEXT FOUND *'                          
NOINSMSA DC    CL60'* NOTE * NO INSTRUCTIONS RUN FOR MEDIA/CLIENT/PROD/C        
               PERIOD *'                                                        
POLPTRMS DC    CL60'* ERROR * CAN''T DO PROD POL AND LIMIT PARTNER PRODC        
               UCT *'                                                           
INVPOLMS DC    CL60'* ERROR * TO RUN FOR ALL PRODUCTS, LEAVE BLANK *'           
*                                                                               
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
* VALIDATE ESTIMATE *                                                           
*                                                                               
VEST     NMOD1 0,**+VES**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VEST20                                                           
VEST10   CLI   SVPROF11,C'E'       IF COPY CODE = EST, MUST ENTER EST           
         BE    VESTER10                                                         
         B     VESTX                                                            
VEST20   GOTO1 ANY                                                              
         CLC   =C'NO',WORK                                                      
         BE    VEST10                                                           
*                                                                               
         CLI   SVPROF11,C'E'       IF COPY CODE NE EST, NO EST ALLOWED          
         BNE   VESTER20                                                         
         GOTO1 VALINUM                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         CLI   BPRD,0              WAS PRODUCT ENTERED                          
         BNE   *+10                 YES                                         
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),ACTUAL                                                  
         MVC   QBEST,ACTUAL                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA                      
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,NOESTS                                                     
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   VESTER                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA                      
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   VESTER30                                                         
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVESTSTR)                              
         GOTO1 (RF),(R1),(0,EEND),(3,SVESTEND)                                  
         MVC   QESTDESC,EDESC                                                   
VESTX    XIT1                                                                   
VESTER   GOTO1 ERREX                                                            
VESTER10 MVC   CONHEAD,=CL60'* ERROR * COPY CODE = EST REQUIRES EST *'          
         B     VESTERX                                                          
*                                                                               
VESTER20 MVC   CONHEAD,=CL60'* ERROR * EST NOT ALLOWED *'                       
         B     VESTERX                                                          
*                                                                               
VESTER30 MVC   CONHEAD,=CL60'* ERROR * EST IS COPY CODED *'                     
VESTERX  GOTO1 ERREX2                                                           
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    MISSERRA                                                         
         GOTO1 ANY                                                              
*                                                                               
         CLI   8(R2),C'?'            IF QUESTION MK, TELL MEL FLT DATES         
         BNE   VPER30                                                           
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
*                                                                               
         CLI   5(R2),1                 SEE IF DATE ENTERED TOO                  
         BE    VPER10                  NO                                       
*                                                                               
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB                 GET LENGTH OF FIELD                      
         LTR   R4,R4                                                            
         BZ    BDATERR                                                          
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVFLTSTR)                            
         B     VPER12                                                           
*                                                                               
VPER10   GOTO1 DATCON,DMCB,(5,0),(3,SVFLTSTR)                                   
*                                                                               
VPER12   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD     BCLT, BPRD (TRY PRD SPECIFIC REC)            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE                                                   
         BNE   NOFLTER                                                          
VPER14   CLC   SVFLTSTR,KEY+6      FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER16                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER16   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R4,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VPER20   BRAS  RE,NEXTEL                                                        
         BNE   VPER24                                                           
         USING FLTDTAEL,R6                                                      
         CLC   SVFLTSTR,FLTEND                                                  
         BNH   VPER22                                                           
         CLC   SVFLTSTR,FLTSTART                                                
         BH    VPER20                                                           
VPER22   GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R4,VPER20                                                        
VPER24   MVI   0(R5),C'*'                                                       
         GOTO1 ERREX2                                                           
*                                                                               
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVESTSTR),(5,CONHEAD+9)                           
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVESTEND),(5,CONHEAD+21)                            
         GOTO1 ERREX2                                                           
*                                                                               
VPER30   CLI   5(R2),8             TEST ONE DATE ENTERED                        
         BH    VPER34                                                           
*                                                                               
* ACCEPT ONE DATE AS FLIGHT START DATE *                                        
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER32                                                           
*                                                                               
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         MVC   SVFLTDTS,SVESTSTR                                                
         GOTO1 DATCON,DMCB,(3,SVFLTSTR),(5,TRAPER)                              
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVFLTEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVFLTSTR),SVQSTART                                  
         GOTO1 (RF),(R1),(3,SVFLTEND),SVQEND                                    
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER46                                                           
*                                                                               
VPER32   GOTO1 VALIDATE,DMCB,SVQSTART                                           
         GOTO1 DATCON,(R1),SVQSTART,(3,SVFLTSTR)                                
         XC    SVFLTEND,SVFLTEND                                                
         BAS   RE,CHKFLT             FILLS IN SVQEND/SVFLTEND                   
         B     VPERX                                                            
         EJECT                                                                  
* VALIDATE TWO DATES *                                                          
*                                                                               
VPER34   DS    0H                                                               
         GOTO1 VALPER,DMCB,SVQSTART                                             
*                                                                               
         GOTO1 DATCON,DMCB,SVQSTART,(3,SVFLTSTR)                                
         GOTO1 (RF),(R1),SVQEND,(3,SVFLTEND)                                    
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER40                                                           
*                                                                               
         BAS   RE,CHKFLT             FILLS IN SVQEND/SVFLTEND                   
*                                                                               
         CLC   SVFLTEND,SVGENEND                                                
         BNE   FLTDTER                                                          
         B     VPERX                                                            
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER40   CLC   SVFLTSTR,SVESTEND    PER START AFTER EST END                     
         BH    ESTDTERR                                                         
         CLC   SVFLTSTR,SVESTSTR    PER START BEFORE EST STR                    
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVFLTEND,SVFLTEND    ANY END DATE ENTERED                        
         BNZ   VPER44                                                           
         MVC   SVFLTEND,SVESTEND    USE EST END DATE                            
         B     VPER46                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
*                                                                               
VPER44   CLC   SVFLTEND,SVESTEND    LAST TLCST MUST BE EST END                  
         BNE   ESTDTERR                                                         
*                                                                               
VPER46   MVC   SVGENDTS,SVFLTDTS                                                
*                                                                               
VPERX    DS   0H                                                                
         GOTO1 DATCON,DMCB,(3,SVFLTSTR),(2,SVFLTSTP)                            
         GOTO1 (RF),(R1),(3,SVFLTEND),(2,SVFLTEDP)                              
         XIT1                                                                   
*                                                                               
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
*                                                                               
CHKFLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         CLI   BPRD,0              TEST ALL PRODUCTS INST                       
         BE    *+10                                                             
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
         BNE   NOFLTER                                                          
         CLC   SVFLTSTR,KEY+6      FIRST TLCST DATE TO RECORD END DATE          
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
         OC    SVFLTEND,SVFLTEND   TEST END DATE GIVEN                          
         BZ    CHKF10                                                           
*                                                                               
         CLC   SVFLTSTR,FLTEND     FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   SVFLTEND,FLTSTART   LAST TLCST BEFORE FLIGHT START               
         BL    CHKF6                                                            
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVFLTEND,FLTEND     LAST TLCST DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   SVFLTSTR,FLTSTART                                                
         BL    FLTOVLER                                                         
         MVC   SVGENDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVFLTEND,FLTEND       TEST LAST TLCST = FLIGHT END               
         BNE   VPERX                                                            
         MVC   SVGENST,SVFLTSTR    SAVE FLIGHT START                            
         B     VPERX                                                            
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF10   CLC   SVFLTSTR,FLTSTART                                                
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVFLTEND,SVGENEND                 FORCE END DATE                 
         GOTO1 DATCON,DMCB,(3,SVFLTEND),SVQEND   AND REQ END DATE               
         B     VPERX                                                            
BDATERR  MVI   ERROR,INVDATE                                                    
         B     TRAPERRA                                                         
FLTDTER  MVI   ERROR,BADFLT                                                     
         B     TRAPERRA                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERRA                                                         
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERRA                                                         
NOFLTER  MVI   ERROR,NOFLTREC                                                   
         B     TRAPERRA                                                         
MISSERRA MVI   ERROR,MISSING                                                    
*                                                                               
TRAPERRA GOTO1 ERREX                                                            
*                                                                               
         DROP  RB,RC                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OPTIONS *                                                            
*                                                                               
* VALID OPTIONS ARE    'MGRP'  NEEDED FOR OPTION MARKET GROUP                   
*                                                                               
VOPT     NMOD1 0,**+VOP**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   8(R2),C'?'         HELP                                          
         BE    VOPTH                                                            
*                                                                               
         XC    OPTIONS,OPTIONS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPTX               NO MUST GO CHECK PROFILE                     
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK)                                      
         LA    R4,BLOCK                                                         
*                                                                               
VOPT10   CLI   0(R4),0             TEST FOR MORE DATA                           
         BE    VOPTX                                                            
*                                                                               
VOPT20   ZIC   R1,0(R4)                                                         
         EX    R1,VOPTCLCA                                                      
         BNE   VOPT40                                                           
         CLI   1(R4),2                                                          
         BL    MGRENTER                                                         
         CLI   1(R4),5                                                          
         BH    MGRENTER                                                         
         CLI   22(R4),C'A'                                                      
         BL    MGRENTER                                                         
         CLI   22(R4),C'Z'                                                      
         BH    MGRENTER                                                         
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LA    R1,22+1(,R4)                                                     
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VOPT22   CLI   0(R1),C'0'                                                       
         BL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VOPT22                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   OPTMGRP(1),22(R4)                                                
         MVC   OPTMGRP+1(2),WORK                                                
         MVC   OPTPMGRP(1),22(R4)                                               
         MVC   OPTPMGRP+1(4),DUB                                                
         MVC   OPTDMGRP(5),22(R4)                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
         MVC   KEY+8(3),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    VOPT26                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BE    VOPT24                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NOMGRPER                                                         
*                                                                               
VOPT24   CLC   KEY+8(3),OPTMGRP                                                 
         BE    VOPT26                                                           
         MVC   KEY+8(3),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VOPT26   CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VOPT28                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VOPT28   CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPER                                                         
         MVC   OPTMGRPM,KEY+11                                                  
         TM    TABSW,STALIST       STATION LIST ENTERED                         
         BO    STLSTERR             YES, ERROR                                  
         OI    TABSW,MKTGRP        FILTER ON MARKET GROUP                       
         B     VOPT80                                                           
*                                                                               
VOPT40   EX    R1,VOPTCLCC         COPY SUMMARY REPORT OF STATIONS SENT         
         BNE   VOPT50                                                           
         MVI   OPTCOPY,C'Y'                                                     
         B     VOPT80                                                           
*                                                                               
VOPT50   EX    R1,VOPTCLCF         STATION AFFILIATE                            
         BNE   VOPT60                                                           
         TM    TABSW,STALIST       STATION LIST ENTERED                         
         BO    STLSTERR             YES, ERROR                                  
         OI    TABSW,STAFFL                                                     
         MVC   OPTAFFL,22(R4)                                                   
         B     VOPT80                                                           
*                                                                               
VOPT60   EX    R1,VOPTCLCB         STATION LIST ENTERED                         
         BNE   VOPT70                                                           
         TM    TABSW,MKTGRP+STAFFL+STATYPE                                      
         BNZ   STLSTERR                                                         
         OI    TABSW,STALIST                                                    
         B     VOPT80                                                           
*                                                                               
VOPT70   EX    R1,VOPTCLCT         COPY SUMMARY REPORT OF STATIONS SENT         
         BNE   VOPTH                                                            
         TM    TABSW,STALIST       STATION LIST ENTERED                         
         BO    STLSTERR             YES, ERROR                                  
         OI    TABSW,STATYPE                                                    
         MVC   OPTTYPE,22(R4)                                                   
*                                                                               
VOPT80   LA    R4,32(R4)                                                        
         B     VOPT10                                                           
VOPTCLCA CLC   12(0,R4),=C'MGR '   FILTER ON MARKET GROUP                       
VOPTCLCB CLC   12(0,R4),=C'SL '    PROCESS FOR STATION LIST                     
VOPTCLCC CLC   12(0,R4),=C'COPY '  COPY OF STATIONS SENT                        
VOPTCLCF CLC   12(0,R4),=C'AFF '   STATION AFFILIATE                            
VOPTCLCT CLC   12(0,R4),=C'TYPE '  STATION TYPE                                 
VOPTINV  MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
VOPTH    MVC   CONHEAD,VOPTHELP                                                 
         B     VOPTERRX                                                         
*                                                                               
VOPT90   MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(25),=C'* ERROR * - UNKNOWN INPUT'                        
         MVC   CONHEAD+26(8),12(R4)                                             
VOPTERRX GOTO1 ERREX2                                                           
*                                                                               
STLSTERR MVC   CONHEAD,STLSTMS                                                  
         B     VOPTERRX                                                         
*                                                                               
MGRENTER MVC   CONHEAD,MGRENTMS                                                 
         B     VOPTERRX                                                         
*                                                                               
NOMGRPER MVC   CONHEAD,NOMGRPMS                                                 
         MVC   CONHEAD+20(5),OPTDMGRP                                           
         B     VOPTERRX                                                         
*                                                                               
VOPTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
KEYWRDER MVI   ERROR,BADKEYWD                                                   
         GOTO1 ERREX                                                            
VOPTHELP DC    CL60'* VALID OPTIONS=COPY/MGROUP/AFFIL/TYPE=/SL *'               
MGRENTMS DC    CL60'* ERROR * MGROUP MUST BE LETTER AND 1-4 DIGITS *'           
STLSTMS  DC    CL60'* ERROR * DO ONLY STATION LIST *'                           
NOMGRPMS DC    CL60'* ERROR * NO MGROUP X0000 FOUND *'                          
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE MARKET LIST (IF NONE, WILL TRY TO DO ALL *                           
*                                                                               
         DROP  RB,RC                                                            
VMKT     NMOD1 0,**+VMK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VMKTX                                                            
         L     R5,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(11,(R5)),0                                    
         ZIC   R4,DMCB+4           COUNT OF BLOCKS FOUND                        
*                                                                               
         CH    R4,=H'10'          ONLY ALLOW 10 MARKET ENTRIES                  
         BH    EXTRAMER                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),0(R2)                                                    
         LA    R2,ELEM                                                          
         MVI   ERROPT,C'Y'         SET ERROPT FOR RETURN HERE                   
         LA    R3,MKTLIST                                                       
         MVI   BYTE,1                                                           
*                                                                               
VMKT10   MVC   ELEM+5(1),0(R5)     DATA LENGTH                                  
         PACK  ELEM+4(1),2(1,R5)   INVERT BITS                                  
         MVC   ELEM+8(10),12(R5)      MOVE IN MARKET                            
         GOTO1 VALIMKT                                                          
         CLI   ERROR,0             IF NON-ZERO, ERROR                           
         BNE   BADMKT              BAD MARKET/STATION                           
         MVC   0(2,R3),BMKT                                                     
         ZIC   R1,BYTE             COUNTER                                      
         LA    R1,1(,R1)           UPDATE # VALID ENTRIES                       
         STC   R1,BYTE             COUNTER                                      
         LA    R1,MKTLIST          START OF LIST                                
VMKT20   CR    R1,R3               AT END OF LIST?                              
         BNL   VMKT30              YES                                          
         CLC   0(2,R1),0(R3)       EQUAL ENTRY?                                 
         BE    VMKT34              YES, IGNORE                                  
         LA    R1,2(,R1)           NEXT SLOT                                    
         B     VMKT20                                                           
*                                                                               
VMKT30   LA    R3,2(,R3)                                                        
*                                                                               
VMKT34   LA    R5,32(,R5)                                                       
         BCT   R4,VMKT10                                                        
*                                                                               
* SORT INTO MARKET ORDER *                                                      
*                                                                               
         ZIC   R4,BYTE                                                          
         BCTR  R4,0                                                             
         MVC   0(2,R3),=X'FFFF'                                                 
         GOTO1 XSORT,DMCB,MKTLIST,(R4),2,2,0                                    
*                                                                               
         MVI   ERROPT,0            RESET ERROPT                                 
VMKTX    XIT1                                                                   
*                                                                               
BADMKT   MVC   CONHEAD,BADMKTMS                                                 
         LA    R4,CONHEAD+16                                                    
         EDIT  (B1,BYTE),(2,(R4))                                               
         B     VMKTEREX                                                         
*                                                                               
EXTRAMER MVC   CONHEAD,EXTRAMMS                                                 
*                                                                               
VMKTEREX LA    R2,TRAMKTSH                                                      
         GOTO1 ERREX2                                                           
BADMKTMS DC    CL60'* ERROR * ENTRY XX IS A BAD MARKET *'                       
EXTRAMMS DC    CL60'* ERROR * MORE THAN 10 MARKETS ENTERED *'                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
* VALIDATE MARKET GROUP FROM PROFILE AND BUILD MARKET GROUP TABLE *             
*                                                                               
VMG      NMOD1 0,**+VMG**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA                      
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    VMG20                                                            
         CLC   KEY(5),KEYSAVE                                                   
         BE    VMG10                                                            
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   MGRPERR                                                          
*                                                                               
VMG10    CLC   KEY+8(1),KEY       MUST BE MKT GRP SCHEME LETTER                 
         BE    VMG30                                                            
         MVC   KEY+8(1),KEY       MUST BE MKT GRP SCHEME LETTER                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
VMG20    CLC   KEY(9),KEYSAVE     FIND A MARKET GROUP                           
         BE    VMG30                                                            
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   BDMGRPER                                                         
*                                                                               
VMG30    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA                      
         GOTO1 GETREC                                                           
         CLI   24(R6),01                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MGRPTITL,26(R6)                                                  
         MVI   KEY+1,X'82'                                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVGRPKEY(4),KEY+5                                                
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         XIT1                                                                   
*                                                                               
MGRPERR  MVC   CONHEAD,MGRPERMS                                                 
         B     VMGERRX                                                          
BDMGRPER MVC   CONHEAD,BDMGRPMS                                                 
*        MVC   CONHEAD+20(1),?????????                                          
VMGERRX  GOTO1 ERREX2                                                           
MGRPERMS DC    CL60'* ERROR * MARKET GROUP IN TC PROFILE MUST BE A-Z *'         
BDMGRPMS DC    CL60'* ERROR * NO MGROUP X FOUND, ADD GROUP OR CHANGE TCC        
                PROFILE 2 *'                                                    
         LTORG                                                                  
         EJECT                                                                  
* READ MARKET GROUP RECORDS TO FIND MARKETS FOR THIS MARKET GROUP *             
*                                                                               
         DROP  RB,RC                                                            
RMG      NMOD1 0,**+RMG**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD(3),BAGYMD & BCLT                                        
         MVC   MKGPPID(4),SVGRPKEY       MOVE IN PRD GRP/MKT GRP SCHEME         
*                                        FROM VMG (VALIDATE MARKET GRP)         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA                      
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(9),KEYSAVE                                                   
         BE    RMG14                                                            
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   RMG40                                                            
*                                                                               
RMG10    CLC   KEY(9),KEYSAVE                                                   
         BNE   RMG40                                                            
*                                                                               
RMG14    MVC   SVGRPKEY,MKGPPID                                                 
         CLC   BMKT,SVMGRPMK        THIS THE MARKET                             
         BE    RMG20                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RMG10                                                            
*                                                                               
RMG20    MVC   BLOCK+2(2),MKGPMGRP                                              
*                                                                               
         LA    R0,15                                                            
         LR    R1,R9                                                            
         A     R1,=A(MGRPHDG-SYSD)                                              
RMG22    OC    0(2,R1),0(R1)       EMPTY SLOT                                   
         BZ    RMG24                                                            
         CLC   MKGPMGRP,0(R1)      ALREADY READ THIS                            
         BE    RMG36                                                            
         LA    R1,26(,R1)                                                       
         BCT   R0,RMG22                                                         
RMG24    MVI   RDUPDATE,C'N'                                                    
         XC    MKGPMKT,MKGPMKT                                                  
         MVI   KEY+1,02                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA                      
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         CLI   24(RE),X'10'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,15                                                            
         LR    R1,R9                                                            
         A     R1,=A(MGRPHDG-SYSD)                                              
RMG26    OC    0(2,R1),0(R1)       EMPTY SLOT                                   
         BZ    RMG28                                                            
         LA    R1,26(,R1)                                                       
         BCT   R0,RMG26                                                         
         DC    H'0'                                                             
RMG28    MVC   0(2,R1),SVMGRP+1                                                 
         MVC   2(24,R1),26(RE)                                                  
*                                                                               
RMG36    MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RMGX                                                             
         DC    H'0'                                                             
RMGX     XIT1                                                                   
*                                                                               
RMG40    MVC   BLOCK+2(2),=XL2'9999'  PUT IN UNKNOWN MARKET GROUP               
         MVC   SVMGRP+1(2),=XL2'9999'                                           
         MVC   SVMGRPMK,BMKT                                                    
         B     RMG36                                                            
         LTORG                                                                  
         DROP  R4,RB,RC                                                         
         EJECT                                                                  
* BUILD STATION TABLE ENTRIES - EXTRA ENTRY IF ALL PRODUCTS *                   
*                                                                               
INIT     NMOD1 0,**+INT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         LA    R1,STATABLE                                                      
         ST    R1,ASTATBLE                                                      
         A     R1,=A(L'STATABLE-6)                                              
         ST    R1,ASTAEND                                                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   INIT20                                                           
*                                                                               
         L     R1,VADUMMY                                                       
         MVC   0(8,R1),=CL8'*STATBL*'                                           
         LA    R1,8(,R1)                                                        
         ST    R1,ASTATBLE                                                      
         AH    R1,=H'21760'       ROOM FOR 1360 STATIONS                        
         LR    R0,R1                                                            
         SH    R0,=H'5'                                                         
         ST    R0,ASTAEND                                                       
*                                                                               
*                                                                               
* CLEAR STATION TABLE                                                           
*                                                                               
INIT20   L     RE,ASTATBLE                                                      
         MVI   0(RE),0                                                          
         LA    R0,1(,RE)                                                        
         LA    RF,1                                                             
         L     R1,ASTAEND                                                       
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         MVCL  R0,RE                                                            
*                                                                               
         MVI   STASW,C'N'                                                       
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STAMASD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA77D                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR67RR DS    F                                                                
ASTATBLE DS    F                                                                
ASTAEND  DS    F                                                                
AENDSTX  DS    F                                                                
PQPROF   DS    A                    ADDRESS OF PQPROF                           
SVCML    DS    CL8                 8 CHAR ISCII/8 BYTES PACKED                  
SVPCML   DS    CL8                 PACKED CML                                   
QCML     DS    CL12                                                             
QCMLTLE  DS    CL15                                                             
QCMLSLN  DS    XL1                                                              
*                                                                               
SEQNUM   DS    H                                                                
LASTMKT  DS    CL4                                                              
*                                                                               
SVMOTSTR DS    0XL5                                                             
SVMOTCPY DS    XL1                                                              
SVMOTCLS DS    XL1                                                              
SVMOTJID DS    XL3                                                              
*                                                                               
SVREMUSR DS    CL3                                                              
*                                                                               
SVESTSTR DS    XL3                 ESTIMATE START                               
SVESTEND DS    XL3                          END                                 
*                                                                               
SVFLTDTS DS    0XL6                                                             
SVFLTSTR DS    XL3                 FLIGHT START                                 
SVFLTEND DS    XL3                        END                                   
*                                                                               
SVFLTDTP DS    0XL4                                                             
SVFLTSTP DS    XL2                 FLIGHT START                                 
SVFLTEDP DS    XL2                        END                                   
*                                                                               
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                                                              
SVGENEND DS    XL3                                                              
*                                                                               
STEXTK   DS    CL6                 STEXT KEY ENTERED BY OPERATOR                
*                                                                               
QUESTOR  DS    CL24                                                             
CONTEL   DS    CL18                                                             
CONFAX   DS    CL18                                                             
*                                                                               
STASW    DS    XL1                 PRINTING COPY LIST STATIONS                  
TABSW    DS    XL1                                                              
STAFFL   EQU   X'80' - STATION AFFIL                                            
FOUNDENT EQU   X'40' - INSTR RECAPS FOUND FOR MEDIA/CLT/PRD/PER                 
STALIST  EQU   X'20' - STATION LIST ENTERED SWITCH                              
MKTGRP   EQU   X'10' - MARKET GROUP                                             
STATYPE  EQU   X'08' - STATION TYPE                                             
*                                                                               
MKTLIST  DS    CL22                                                             
*AR                                                                             
PROFKEY  DS    CL5                 READ PROGRAM PROFILE RECORD                  
SVQLTYP1 DS    CL1                 STORE ARCHIVE SETTING                        
SVFAXARC DS    CL1                 ARCHIVE FOR FAX COPIES                       
*AR                                                                             
*                                                                               
SVGRPKEY DS   0XL8                                                              
SVPGRPL  DS    XL3                 REDEFINED AS SVPGRP IS IN SPTRAWORKD         
SVMGRP   DS    XL3                                                              
SVMGRPMK DS    XL2                                                              
MGRPTITL DS    CL12                ROOM FOR MGRP TITLE                          
*                                                                               
OPTIONS  DS    0CL(ENDOPT-OPTPGRP)                                              
*                                                                               
OPTPGRP  DS    XL3                 MGRP LETTER PLUS PWS 4 DIGITS                
OPTMGRP  DS    XL3                 MGRP LETTER PLUS PWS 4 DIGITS                
OPTMGRPM DS    XL2                                                              
OPTPMGRP DS    CL5                 MGRP WITH TRAILING ZEROS                     
OPTDMGRP DS    CL5                 DISPLAYABLE MGRP AS ENTERED                  
OPTCOPY  DS    CL1                                                              
OPTSTA   DS    0CL4                                                             
OPTTYPE  DS    CL1                                                              
OPTAFFL  DS    CL3                                                              
ENDOPT   EQU   *                                                                
*                                                                               
         DS    0D                                                               
STATABLE DS    XL6000                                                           
MGRPHDG  DS    XL390               ROOM FOR 15 MGRP ID AND TITLES               
*                                  EACH ENTRY 2 MGRP, 24 TITLE                  
*                                                                               
ENDSYSD  EQU   *                   IF THIS PAST 1F70, PAST END OF SYSD          
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL8                                                              
PMGRP    DS    CL5                                                              
         DS    CL8                                                              
PMKT     DS    CL4                                                              
         DS    CL8                                                              
PMKTNM   DS    CL24                                                             
         DS    CL10                                                             
PSTA     DS    CL7                                                              
         ORG   P                                                                
*                                                                               
       ++INCLUDE EDIDESTD                                                       
         ORG   P                                                                
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPTRA67   09/01/10'                                      
         END                                                                    
