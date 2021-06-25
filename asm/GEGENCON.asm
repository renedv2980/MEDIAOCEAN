*          DATA SET GEGENCON   AT LEVEL 038 AS OF 06/20/17                      
*PHASE T00A30A                                                                  
         TITLE 'T00A30 - GENERAL SPOOL/MAINT CONTROLLER'                        
T00A30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENCON,R7,R9,RR=R2                                           
         L     R8,0(R1)                                                         
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R2,GENCRELO                                                      
         ST    R8,ASPOOLD                                                       
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
                                                                                
         BRAS  RE,INITIAL                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
***********************************************************************         
* NOTE YOU CAN GET HERE WHEN WE NOTICE THAT REQUEST DISPLAY                     
* WAS ACTIVE AND ISN'T ANY MORE                                                 
***********************************************************************         
I0       CLI   TWASCR,X'FF'        TEST REQUEST DISPLAY ACTIVE                  
         BE    I1                                                               
         TM    GENSTAT4,SVTWA0FF   DO WE SKIP THE RESTORE?                      
         BO    *+8                 YES                                          
         BAS   RE,RESTSTOR         RESTORE SYSTEM SAVED AREAS                   
         GOTO1 GETUSER             SYSTEM SPECIFIC USER NAME & ADDRESS          
*                                                                               
I1       CLI   GCMODE,C'S'         EXIT HERE IF SLAVED MODE                     
         BE    XIT                                                              
*                                                                               
         MVC   SVUSEIO,USEIO       SAVE FOR POSSIBLE GCMODE 4                   
         MVC   SVSYSDIR,SYSDIR     *   SYSTEM SWITCH                            
         MVC   SVSYSFIL,SYSFIL     *                                            
         MVC   SVDATADI,DATADISP   *                                            
         MVC   SVLKEY,LKEY         *                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE RECORD AND ACTION                                       
***********************************************************************         
REC0     L     R2,EFHREC           RECORD                                       
         MVI   ERROR,INVREC                                                     
         MVI   ONLYSPUL,C'N'                                                    
         L     R1,EFHKEY                                                        
         TM    1(R1),X'20'         SPOOL ONLY SYSTEMS                           
         BNO   REC1                HAVE KEY FIELD PROTECTED                     
         GOTO1 ANY                                                              
         MVC   REMUSER,WORK        THEIR FIRST FIELD IS REQUESTOR               
         L     R2,EFHACT           AND SECOND FIELD IS REPORT                   
         MVI   ERROR,INVREP                                                     
         MVI   ONLYSPUL,C'Y'                                                    
         B     REC2                                                             
         SPACE 1                                                                
REC1     MVI   DUB,X'04'           LOOK UP TYPE 4 ENTRIES (PROG RECS)           
         MVI   GCMODE,C'4'         PRE-SET TO HANDLE PROGRAM RECS               
         ICM   R3,15,ARECACT1      ALTERNATIVE START FOR 01 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         BAS   RE,LOOKUP                                                        
         BE    REC2A               FOUND IT - CONTINUE ON                       
         SPACE 1                                                                
REC2     MVI   GCMODE,C' '                                                      
         MVI   DUB,X'01'           LOOK UP X'01' ENTRY                          
         ICM   R3,15,ARECACT1      ALTERNATIVE START FOR 01 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         BAS   RE,LOOKUP                                                        
         SPACE 1                                                                
REC2A    L     R3,WORK                                                          
         MVC   RECNUM,9(R3)        EXTRACT RECORD NUMBER                        
         MVC   PHDTADCT,10(R3)             DATA DICTIONARY PHASE                
         MVC   PHHELP,11(R3)               HELP PHASE                           
         BAS   RE,ANYHELP          POSSIBLE HELP EXIT                           
         CLC   TWALREC,RECNUM      CLEAR LIST DIRECTORY                         
*                                  ON CHANGE OF RECORD                          
         BE    ACT2                                                             
         XC    LISTDIR,LISTDIR                                                  
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
         MVI   TWALACT,0                                                        
         SPACE 1                                                                
ACT2     L     R2,EFHACT           ACTION                                       
         CLI   ONLYSPUL,C'Y'                                                    
         BNE   ACT2B                                                            
         MVI   ACTNUM,12                                                        
         MVI   ACTEQU,12                                                        
         B     RAB                                                              
         SPACE 1                                                                
ACT2B    MVI   ACTIVSW,0                                                        
         CLC   8(#ACTLQ,R2),LP@ACT           ACTIVE IS SPECIAL DISPLAY          
         BNE   ACT3                                                             
         MVI   ACTIVSW,1                                                        
         MVC   8(#DISLLQ,R2),LP@DISL                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),7                                                          
         B     ACT8                                                             
         SPACE 1                                                                
ACT3     CLI   LISTSW,X'FF'        TEST RETURNING FROM SELECT SCREEN            
         BNE   ACT3D                                                            
         MVI   LISTSW,C'N'         SET TO CONTINUE TO NEXT PAGE                 
         TM    GENSTAT2,DISTHSPG                                                
         BZ    *+8                                                              
         MVI   LISTSW,C'T'         USER WANTS THIS PAGE RE-DISPLAYED            
         B     ACT8                                                             
         SPACE 1                                                                
ACT3D    MVI   LISTSW,0            CHECK FOR SPECIAL LIST ACTIONS               
         GOTO1 ANY                                                              
         LA    R3,LISTLIST                                                      
         MVC   DUB(2),LARF00       LA  RF,0(0)                                  
         USING LISTLD,R3                                                        
         SPACE 1                                                                
ACT4     MVC   DUB+2(2),LISTSNAM   GENERATE BASE DISP FOR LA RF,0(0)            
         EX    0,DUB               RF=A(LIST OPTION NAME)                       
         CLC   WORK(8),0(RF)                                                    
         BE    ACT6                                                             
         LA    R3,LISTLLQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   ACT4                                                             
         B     ACT8                                                             
         SPACE 1                                                                
ACT6     MVC   LISTSW,LISTSET                                                   
         MVC   8(#LISTPLQ,R2),LP@LISTP                                          
         MVI   5(R2),#LISTPLQ                                                   
         OI    6(R2),X'80'                                                      
         DROP  R3                                                               
         SPACE 1                                                                
ACT8     MVI   DUB,X'02'           LOOK UP X'02' ENTRY                          
         MVI   ERROR,INVACT                                                     
         MVI   ACTNUM,0                                                         
         CLI   5(R2),3             ONLY TEST 3 BYTES MAX                        
         BL    *+8                                                              
         MVI   5(R2),3                                                          
         ICM   R3,15,ARECACT2      ALTERNATIVE START FOR 02 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         BAS   RE,LOOKUP                                                        
         L     R3,WORK                                                          
         CLI   9(R3),ACTSEL        FOR ACTION SELECT                            
         BNE   *+12                                                             
         CLI   TWALACT,ACTSEL      LAST ACTION MUST ALSO HAVE BEEN SEL.         
         BNE   ERRXIT              OR INTERNALLY SWITCHED TO SELECT             
         MVC   ACTNUM,9(R3)        EXTRACT ACTION NUMBER                        
         MVC   ACTEQU,10(R3)               AND ACTION EQUATE                    
         BAS   RE,ANYHELP                                                       
         CLI   USEIO,C'Y'          TEST CTFILE I/O (NOT GENDIR/GENFIL)          
         BNE   RAB                                                              
         CLI   TWALACT,ACTLIST     TEST PREVIOUS ACTION WAS LIST                
         BE    *+12                                                             
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BNE   RAB                                                              
         CLI   ACTNUM,ACTLIST      BUT CURRENT ACTION ISN'T                     
         BE    RAB                                                              
         CLI   ACTNUM,ACTSEL       EITHER OF THEM                               
         BE    RAB                                                              
         XC    LASTSELK,LASTSELK   CLEAR SAVED LIST KEY                         
         EJECT                                                                  
*              CHECK RECORD/ACTION COMBINATION - SET UP PHASES                  
         SPACE 1                                                                
RAB      MVC   WORK+2(1),ACTEQU    SET ACTION EQUATE FOR LOOKUP                 
         MVI   ERROR,0             PRESET NO 'WHEN' ERROR                       
         XR    R3,R3                                                            
*                                                                               
RA2A     BAS   RE,LKRECACT         LOOKUP RECORD/ACTION ENTRY IN TABLE          
         BE    RA4                                                              
         CLI   ERROR,0             WAS THERE A 'WHEN' ERROR                     
         BNE   ERRXIT              YES - HONO(U)R IT                            
         MVI   ERROR,INVRCACT                                                   
         B     ERRXIT                                                           
                                                                                
RA3      MVC   WORK(3),0(R3)       IF WHEN DOESN'T MATCH, CONTINUE              
         B     RA2A                SEARCH                                       
                                                                                
RA4      L     R3,WORK+4           R3=A(ENTRY) FROM LOOKUP                      
         MVC   PHSCREEN(4),3(R3)   EXTRACT SCREEN/EDIT/SPECS/REPORT             
         MVC   WHENOK,7(R3)                AVAILABLE PRINT OPTIONS              
         MVC   RCPROG+2(2),8(R3)   HEADLINE PROGRAM CODE                        
         MVC   QCRDCODE,10(R3)     EOD PROGRAM HANDLE                           
*                                                                               
         ICM   R2,15,EFHOTH        POINT TO 'OTHER'                             
         BZ    RA10                                                             
         CLI   5(R2),0             TEST INPUT                                   
         BE    RA10                NO                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             TEST 3 CHAR                                  
*&&US*&& BNE   ERRXIT                                                           
*&&UK*&& BNE   RA10                                                             
         CLC   8(#REQLQ,R2),LP@REQ TEST REQUEST DISPLAY                         
*&&US*&& BNE   ERRXIT                                                           
*&&UK*&& BNE   RA10                                                             
         TM    WHENOK,X'18'        TEST REQUEST DATA POSSIBLE (OV/DDS)          
         BZ    RA3                 GO SEE IF MORE MATCHING RECACTS              
*                                                                               
         MVC   TWALREC,RECNUM      SAVE RECORD NUMBER                           
         SPACE 1                                                                
* LOAD REQUEST DISPLAY LOGIC (TFD800) *                                         
         SPACE 1                                                                
         MVC   AOVERLAY,SYSDUMMY                                                
         GOTO1 GETFACT,DMCB,0      GET A(PROGRAMS AREA)                         
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         TM    GENSTAT1,APPLIC     USE IT IF NORMALLY REQUIRE APPLIC            
         BZ    RA8                                                              
         NI    GENSTAT1,X'FF'-APPLIC  INSURE MAINTENANCE CALL                   
         L     RF,FAPGMADR         AND SET TO LOAD IN PGMAREA                   
         AHI   RF,20000            ALLOW ROOM FOR ROOT PHASE                    
         ST    RF,AOVERLAY                                                      
RA8      MVC   SYSPHASE,=X'D90FD800' SET PHASE ID                               
         GOTO1 LOADEM,DMCB,0                                                    
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
RA10     CLI   TWASCR,X'FF'        TEST REQ DISPLAY SCREEN LOADED               
         BNE   RA12                NO                                           
         MVI   TWASCR,0            RESET IT NOW                                 
         XC    LISTDIR,LISTDIR     CLEAR REMAINS - ITS GONE NOW                 
         B     I0                  GO RESTORE/CALL GETUSER                      
         SPACE 1                                                                
RA12     CLI   GCMODE,C'4'         TEST PROG RECS                               
         BNE   RA20                NO                                           
         CLI   ACTNUM,ACTREP                                                    
         BE    RA20                                                             
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   RA14                                                             
         CLI   ACTNUM,ACTSEL       TEST REPORT WAS SELECTED                     
         BNE   RA14                                                             
         CLC   THISLSEL,LP@RESS    R=REP NOT R=RES                              
         BE    RA20                                                             
         SPACE 1                                                                
RA14     MVI   WHENOK,X'80'                                                     
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+8                                                              
         MVI   WHENOK,X'F8'                                                     
         BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
         BE    RA20                TEST SWITCH WAS SUCCESSFUL                   
         MVI   OKNO,19                                                          
         B     OKEX                TELL USER TO START CONTROL                   
         SPACE 1                                                                
RA20     EQU   *                                                                
         TM    GENSTAT5,VRAHOOK    TEST APPLIC WANTS MODE VALRA PASSED          
         BZ    RAX                                                              
         CLI   ACTNUM,ACTSEL       SELECT ACTION?                               
         BE    RAX                 SKIP CALL, DO IT LATER FOR SEL CODE          
         MVI   MODE,VALRA          APPLIC CONTROLLER GETUSER MODE               
         GOTO1 GETUSER                                                          
RAX      EQU   *                                                                
         EJECT                                                                  
* VALIDATE PRINT OPTIONS (WHEN) *                                               
         SPACE 1                                                                
         L     R2,EFHWHEN                                                       
         MVI   PHHELP,X'EF'                                                     
         BAS   RE,ANYHELP                                                       
         MVI   TWAWHEN,0                                                        
         MVI   WHEN,X'80'          DEFAULT IS ON SCREEN                         
         CLI   ONLYSPUL,C'Y'       SPOOL ONLY DEFAULT TO NOW                    
         BNE   *+8                                                              
         MVI   WHEN,X'40'          NOW                                          
         CLI   5(R2),0                                                          
         BE    VPOX                                                             
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),X'6B7EFF6B'  C',=X,'                 
         CLI   4(R1),1             MUST ONLY BE ONE ENTRY                       
         BNE   VPINVAL                                                          
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
         LA    RE,PRTOPTS          RE=A(VALID PRINT OPTIONS)                    
         USING PRTOPTD,RE                                                       
         MVC   DUB,LARF00          LA RF,0(0)                                   
         CLI   GCMODE,C'4'         PROGRECS GET EXTENDED LIST                   
         BNE   VPO1A                                                            
         CLI   ACTNUM,ACTSEL       ONLY IF ACTION IS SELECT (FOR                
         BNE   VPO1A                                         REPORT)            
         LA    RE,PRGOPTS                                                       
*                                                                               
VPO1A    CLI   POPTSNAM,X'FF'      TEST EOT                                     
         BE    VPINVAL                                                          
         MVC   DUB+2(2),POPTSNAM   GENERATE BASE DISP FOR LA RF,0(0)            
         EX    0,DUB               RF=A(PRINT OPTION NAME)                      
         CLC   SC1STFLD(WHENLQ),0(RF)                                           
         BE    VPO1C                                                            
VPO1B    LA    RE,POPTLQ(RE)                                                    
         B     VPO1A                                                            
*                                                                               
VPO1C    LA    R1,POPTBR           R1=A(OPTION BRANCH (NON-SPOOL))              
         CLI   ONLYSPUL,C'Y'                                                    
         BNE   *+8                                                              
         LA    R1,POPTSPUL         DITTO SPOOL-ONLY                             
         ICM   RF,15,0(R1)                                                      
         BZ    VPINVAL                                                          
         A     RF,GENCRELO                                                      
         MVC   WHEN,POPTWHEN                                                    
         MVC   TWAWHEN,POPTTWAW                                                 
         BR    RF                                                               
         DROP  RE                                                               
         SPACE 1                                                                
VPINVAL  MVI   ERROR,INVPRINT                                                   
         B     ERRXIT                                                           
*                                                                               
VPREMU   MVC   REMUSER,SC2NDFLD    USER ID FOR NOW/SOON/ON                      
         DROP  R3                                                               
                                                                                
VPOX     CLI   OFFLINE,C'Y'        DON'T CHECK IF OFF LINE                      
         BE    VOUT                                                             
         MVC   DUB,WHEN                                                         
         NC    DUB(1),WHENOK                                                    
         MVI   ERROR,POPTNTOK                                                   
         CLI   DUB,0                                                            
         BE    RA3                  GO SEE IF MORE MATCHING RECACTS             
         TM    WHEN,X'20'+X'10'     SOON/OV CHECK FOR REMUSER                   
         BZ    VOUT                                                             
         CLC   REMUSER,SPACES                                                   
         BH    *+12                                                             
         MVI   ERROR,NOUSRNAM       NO USER NAME ENTERED                        
         B     ERRXIT                                                           
         EJECT                                                                  
*                                                                               
*              VALIDATE OUTPUT                                                  
*                                                                               
VOUT     XC    TWAOUT,TWAOUT                                                    
*                                                                               
         ICM   R2,15,EFHOUT        IGNORE IF NO OUTPUT FIELD                    
         BZ    VOUTX                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VOUT10                                                           
*                                                                               
         OC    ARFPBLK,ARFPBLK     IF RFP PROCESSING                            
         BZ    VOUTX                                                            
*                                                                               
         MVI   ERROR,MISSING       THEN INPUT REQUIRED                          
         B     ERRXIT                                                           
*                                                                               
VOUT10   DS    0H                                                               
*                                                                               
         BRAS  RE,VALOUT                                                        
*                                                                               
VOUTX    DS    0H                                                               
*                                                                               
         EJECT                                                                  
*              VALIDATE DESTINATION                                             
         SPACE 1                                                                
VDEST    XC    TWADEST,TWADEST     INIT DESTINATION                             
*                                                                               
         ICM   R2,15,EFHDEST       SKIP IF NO DESTINATION FIELD                 
         BZ    VDESTX                                                           
*                                                                               
         CLI   5(R2),0             IF DESTINATION NOT ENTERED                   
         BNE   VDEST10                                                          
*                                                                               
         OC    ARFPBLK,ARFPBLK     IF RFP PROCESSING                            
         BZ    VDESTX                                                           
*                                                                               
         MVI   ERROR,MISSING       THEN INPUT REQUIRED                          
         B     ERRXIT                                                           
*                                                                               
VDEST10  DS    0H                                                               
*                                                                               
         BRAS  RE,VALDEST                                                       
*                                                                               
VDESTX   DS    0H                                                               
*                                                                               
         EJECT                                                                  
*              LOAD IN SCREEN                                                   
         SPACE 1                                                                
LOAD     L     R2,EFHACT           RESET CURSOR TO ACTION                       
         MVI   OVERLAY,2                                                        
*&&US                                                                           
         CLI   TWAFIRST,X'FF'      CHECK FOR RUNLAST                            
         BNE   LOADB                                                            
         L     RF,TWADCONS                                                      
         MVC   AGO,TAOVER-TWADCOND(RF)                                          
         B     REPORT                                                           
*&&                                                                             
         SPACE 1                                                                
LOADB    CLI   GCMODE,C'4'         CHECK FOR PROG RECORDS LIST                  
         BNE   *+16                   *                                         
         CLI   ACTNUM,ACTLIST         *                                         
         BNE   *+8                    *                                         
         MVI   PHSCREEN,1             *                                         
         CLI   PHSCREEN,0          DO WE NEED A SCREEN                          
         BE    LOAD5                                                            
         CLC   PHSCREEN,TWASCR     DO WE HAVE IT YET                            
         BE    LOAD5                                                            
         CLI   ACTNUM,ACTLIST      IF THIS IS A LIST ACTION                     
         BNE   LOAD1                                                            
         CLI   TWALACT,ACTSEL      AND LAST ACTION WAS SELECT                   
         BNE   LOAD1                                                            
         CLC   TWALREC,RECNUM      FOR SAME RECORD TYPE                         
         BNE   LOAD1                                                            
         L     RF,EFHKEY           RF=A(KEY FIELD)                              
         MVC   BLOCK(88),0(RF)     SAVE HEADER AND CONTENTS OF FIELD            
         ZIC   R1,LISTSW           WE NEED TO SAVE SPECIAL SWITCH               
         BAS   RE,RESTLIST         WE NEED TO RESTORE LIST SCREEN               
         BNE   LOAD1               UNLESS NONE SAVED                            
         BAS   RE,TRANSBTS                                                      
         STC   R1,LISTSW           NOW REPLACE SWITCH                           
         ICM   R1,1,BLOCK+5        RESTORE CONTENTS OF KEY FIELD                
         BZ    LOAD5                                                            
         STC   R1,5(RF)            MOVE L'FIELD TO HEADER                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),BLOCK+8                                                  
         B     LOAD5                                                            
         SPACE 1                                                                
LOAD1    L     R3,EFHTAG           SAVE CURRENT SCREEN FOR KEYMERGE             
         LA    R0,IO                                                            
         LA    R1,L'IO             ENOUGH TO COVER ALL KEY FIELDS               
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SPACE 1                                                                
         CLI   GCMODE,C'4'         CHECK FOR PROG RECORDS LIST                  
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    LOAD1A                                                           
         MVC   TWASCR,PHSCREEN                                                  
         MVC   OVERLAY,PHSCREEN                                                 
         GOTO1 LOADEM,DMCB,1                                                    
         CLI   TWACOMMN,1          EXIT NOW IF FIRST SPOOF CALL                 
         BE    XIT                                                              
         TM    GENSTAT4,NEWSCRM    TEST APPLIC WANTS NEW SCREEN PASSED          
         BZ    LOAD1B                                                           
         MVI   MODE,NEWSCR         APPLIC CONTROLLER GETUSER MODE               
         GOTO1 GETUSER                                                          
         B     LOAD1B                                                           
         SPACE 1                                                                
LOAD1A   XC    DMCB(12),DMCB       LOAD PROG RECORDS LIST SCREEN                
         MVC   DMCB+4(4),=X'D90FD8FE'                                           
         ST    R3,DMCB                                                          
         GOTO1 CALLOV,DMCB                                                      
         OC    DMCB+9(3),DMCB+9                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TWASCR,PHSCREEN                                                  
         MVC   OVERLAY,PHSCREEN                                                 
         MVI   NLISTS,16                                                        
         SPACE 1                                                                
*                                  MERGE IN KEYS FROM SAVED SCREEN              
LOAD1B   BRAS  RE,KEYMERGE                                                      
         BAS   RE,SETTWADD         ESTABLISH ADDRESSES                          
         BAS   RE,TRANSBTS         RETRANSMIT ORIGINAL PART OF SCREEN           
         B     LOAD5                                                            
         SPACE 1                                                                
LOAD2    L     R2,EFHKEY                                                        
         ST    R2,DMCB             THEN USE SCUNKEY TO OUTPUT KEYS              
         MVI   DMCB,C','           WE USE COMMAS AS SEPARATOR                   
         CLI   MYSEP,C' '          IF OVERRIDE SEPARATOR DEFINED                
         BNH   *+10                                                             
         MVC   DMCB(1),MYSEP       USE IT                                       
         MVC   DMCB+4(4),AFRSTKEY                                               
         GOTO1 SCUNKEY,DMCB                                                     
         GOTOR CLEARFLD            CLEAR KEY FIELD                              
         B     LOAD6                                                            
         SPACE 1                                                                
LOAD5    BAS   RE,SETTWADD                                                      
         ICM   R2,15,AFRSTKEY      ARE THERE ANY FIELDS                         
         BZ    LOAD6               NO, SO IGNORE "KEY" FIELD                    
         CLI   ACTNUM,ACTSEL       IGNORE KEY FIELD IF ACTION SELECT            
         BE    *+16                                                             
         L     R1,EFHKEY                                                        
         CLI   5(R1),0             ANY KEY FIELDS?                              
         BNE   LOAD2               GO BACK FOR SCUNKEY                          
         CLI   OFFLINE,C'Y'        DON'T CHECK IF OFFLINE                       
         BE    LOAD6                                                            
LOAD5A   TM    1(R2),X'20'         CHECK ONLY UNPROTECTED                       
         BO    LOAD5B                                                           
         CLI   8(R2),0             IS KEY EMPTY                                 
         BNE   LOAD6               NO                                           
LOAD5B   BAS   RE,BUMP             TRY REST                                     
         CLI   0(R2),0             CHECK END OF                                 
         BE    LOAD5C               SCREEN                                      
         C     R2,AFRSTREC         TEST END OF KEY                              
         BL    LOAD5A                FIELDS                                     
         CLC   AFRSTREC,AFRSTKEY   IF ALL FIELDS KEY FIELDS                     
         BE    LOAD5A                CHECK ALL FIELDS                           
*                  WE HAVE NO KEY FIELDS FILLED IN, IS THIS OK?                 
LOAD5C   CLI   ACTNUM,ACTSEL       DONT NEED ANY FOR SELECT                     
         BE    LOAD6                                                            
         CLC   OVERLAY,PHSCREEN    CHECK IF REENTRY-IF NOT ASK                  
         BE    LOAD5D                 FOR INPUT                                 
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    LOAD6                                                            
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BNL   LOAD6                                                            
LOAD5D   L     R2,AFRSTKEY                                                      
         MVI   OKNO,2              WE NEED HELP FROM USER                       
         B     MODEX                                                            
         SPACE 3                                                                
         EJECT                                                                  
*              LOAD IN DICTIONARY AND/OR EDIT PHASE                             
         SPACE 1                                                                
LOAD6    XC    CONHEAD,CONHEAD     PRE-CLEAR ERROR AREA                         
         MVC   AOVERLAY,SYSDUMMY                                                
         CLI   PHDTADCT,0          OPTION TO LOAD IN DICTIONARY                 
         BE    LOAD8                                                            
         MVC   OVERLAY,PHDTADCT                                                 
         GOTO1 LOADEM,DMCB,0                                                    
         ST    R3,ADTADICT                                                      
         MVC   AGO,AOVERLAY        THIS IS LOAD ADDR FOR REPORT                 
*                                   PHASE IF NO EDIT PHASE..                    
*                                   USED BY REPLOAD                             
         SPACE 1                                                                
LOAD8    CLI   PHEDIT,0            IS AN EDIT REQUIRED                          
         BE    REPORT                                                           
         MVC   OVERLAY,PHEDIT                                                   
         MVC   SVSYSPH,SYSPHASE+1       (KEY TO PROG REC)                       
         GOTO1 LOADEM,DMCB,0                                                    
         ST    R3,AGO                                                           
         MVC   KEY,TWAKEYSV                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TWAKEYSV),TWAKEYSV                                      
*                                                                               
         SR    R0,R0               SET KEY NOT CHANGED FLAG!!!                  
         CLI   ONLYSPUL,C'Y'       ALWAYS FOR SPOOL                             
         BE    LOAD9                                                            
         CLI   ACTNUM,ACTSEL       NEVER FOR SELECT                             
         BE    LOAD10                                                           
         CLC   TWALREC,RECNUM      ALWAYS FOR NEW RECORD TYPE                   
         BNE   LOAD9                                                            
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   LOAD9                                                            
         CLI   TWALACT,ACTADD      VALIDATE IF LAST WAS ADD                     
         BE    LOAD9                                                            
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BE    LOAD9                                                            
         BRAS  RE,ANYKEY           OR IF ANY KEY FIELDS ENTERED                 
         BE    LOAD9                                                            
         TM    TWASTAT1,VALKEYOK   IF RETURNED LAST TIME, DON'T BOTHER          
         BO    LOAD10                                                           
         SPACE 1                                                                
LOAD9    NI    TWASTAT1,X'FF'-VALKEYOK                                          
         MVI   MODE,VALKEY         GO AND VALIDATE KEYS                         
         BAS   RE,GO                                                            
         CLI   MODE,RUNEXIT        END THIS RUN DUE TO ERROR                    
         JE    XIT                                                              
*                                                                               
         LA    RE,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    RE,BIGKEY                                                        
*                                                                               
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,LOADCLC                                                       
         BE    *+8                                                              
         LA    R0,1                SET KEY CHANGED FLAG !!!                     
         MVC   TWAKEYSV,0(RE)      MOVE KEY TO SAVE AREA                        
         OI    TWASTAT1,VALKEYOK   KEY HAS BEEN VALIDATED                       
         B     LOAD10                                                           
*                                                                               
LOADCLC  CLC   TWAKEYSV(0),0(RE)                                                
         SPACE 1                                                                
LOAD10   TM    WHENOK,X'01'        USER NOW HAS THE OPTION                      
         BO    OTHERS              OF DOING HIS OWN MAINTENANCE                 
         CLI   ACTNUM,ACTLIST      SELECT ROUTINE FROM ACTION                   
         BE    LIST                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    SELECT                                                           
         CLI   USEIO,C'Y'          IF NO DISK ADDRESSES                         
         BE    LOAD14              DON'T MATCH THEM IDIOT                       
         CLI   ACTNUM,ACTCHA                                                    
         BNE   LOAD12                                                           
         CLI   TWALACT,ACTSEL                                                   
         BNE   LOAD12                                                           
         LTR   R0,R0               TEST KEY CHANGED                             
         BNZ   LOAD12                                                           
*MH                                                                             
* FIND LIST ENTRY NUMBER BY MATCHING DISK ADDRESS                               
* TWALACT WILL BE SELECT IF YOU GET TO HERE                                     
         MVI   ACTNUM,ACTSEL                                                    
         LA    RE,LISTDIR          START OF LIST                                
         LLC   R0,LISTNUM          NUMBER OF ENTRIES                            
*                                                                               
LOAD11A  CLC   LASTSEL,2(RE)                                                    
         BE    LOAD11B                                                          
         LA    RE,6(RE)                                                         
         BCT   R0,LOAD11A                                                       
         B     *+8                 IF NOT FOUND, JUST IGNORE                    
LOAD11B  MVI   0(RE),CHASELQ                                                    
         B     LOAD14                                                           
*MH                                                                             
         SPACE 1                                                                
LOAD12   XC    LISTDIR,LISTDIR     CLEAR LIST DIRECTORY IF ACTION               
         MVI   THISLSEL,0                IS NOT SELECT OR LIST                  
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
         SPACE 1                                                                
LOAD14   L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTLIST                                                   
         BH    REPORT                                                           
         ZIC   RF,ACTNUM           MAINTENANCE ACTIONS                          
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     MAINT(RF)                                                        
         SPACE 1                                                                
MAINT    B     ADD2                1                                            
         B     CHANGE              2                                            
         B     DISPLAY             3                                            
         B     DELETE              4                                            
         B     SELECT              5                                            
         B     RESTORE             6                                            
         B     OTHERS              7                                            
         B     OTHERS              8                                            
         B     OTHERS              9                                            
         SPACE 1                                                                
OTHERS   MVI   MODE,VALREC         FOR OTHERS USER DOES WORK                    
         BAS   RE,GO                                                            
         CLI   CONHEAD,0           ANY MESSAGE YET                              
         BNE   EXIT                                                             
         MVI   OKNO,17             NO SO USE A GENERAL MESSAGE                  
         B     OKEX                                                             
         EJECT                                                                  
*              MAINTENANCE ROUTINES - ADD                                       
         SPACE 1                                                                
ADD2     BAS   RE,ADDVKEY          VALIDATE KEY FOR ADD                         
         BE    ADD2B               RETURNS CC EQ IF OK                          
         L     R1,AFRSTKEY         ELSE POINT CURSOR TO FIRST KEY FIELD         
         OI    6(R1),X'80'+X'01'   TRANSMIT & MODIFY NEXT TIME TO               
         B     ERRXIT              TAKE MODIFIED KEY AND DO DISPLAY             
         SPACE 1                                                                
ADD2B    L     R2,AFRSTREC                                                      
ADD2D    TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         CLI   5(R2),0             ANY DATA YET IN ANY RECORD FIELD             
         BNE   ADD3                IF NOT,                                      
         BAS   RE,BUMP                                                          
         CLI   0(R2),0                                                          
         BNE   ADD2D                                                            
         L     R2,AFRSTREC                                                      
         MVI   OKNO,2              USER NEEDS TO FILL IN DATA                   
         B     MODEX                                                            
         SPACE 1                                                                
ADD3     MVI   MODE,VALREC         SET MODE FOR RECORD VALIDATION               
         BAS   RE,ADDIT            ADD THE RECORD                               
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,6                                                           
         B     OKEX                                                             
         SPACE 3                                                                
*              VALIDATE KEY FOR ADD                                             
         SPACE 1                                                                
ADDVKEY  NTR1                                                                   
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                CHECK RECORD DOES NOT YET EXIST              
         NI    DMINBTS,X'F7'                                                    
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BO    *+12                                                             
         EX    R1,KEYCOMP                                                       
         B     *+8                                                              
         EX    R1,BKEYCOMP         COMPARE USING BIGKEY                         
         BNE   ADDVKX                                                           
         MVI   ERROR,RECEXIST                                                   
         TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         BNO   NO                                                               
         TM    GENSTAT1,OKADDEL    IS IT OK TO ADD DELETED                      
         BO    ADDVKX                                                           
         MVI   ERROR,DELEXIST      DIFFERENT ERROR MESSAGE                      
         B     NO                                                               
ADDVKX   MVC   KEY,KEYSAVE         OK TO ADD - RESTORE KEY                      
         TM    GENSTAT4,USEBIGKY                                                
         BNO   YES                                                              
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         B     YES                 RETURN CC EQ                                 
         SPACE 2                                                                
KEYCOMP  CLC   KEY(0),KEYSAVE                                                   
BKEYCOMP CLC   BIGKEY(0),KEYSAVE                                                
         EJECT                                                                  
*              ROUTINE CONTROLS ADDING A RECORD                                 
         SPACE 1                                                                
*                                  MODE = RECORD VALIDATION MODE                
ADDIT    NTR1                                                                   
         L     RE,AIO              PRE-CLEAR I/O                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,AIO              MOVE KEY INTO I/O                            
         LA    R4,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R4,BIGKEY                                                        
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
         SPACE 1                                                                
         BAS   RE,GO               PASS REC. VALIDATION MODE (PASSED)           
         MVI   MODE,RECADD                                                      
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    ADIT5                                                            
         CLI   USEIO,C'Y'                                                       
         BE    ADIT3                                                            
*                                                                               
         TM    GENSTAT1,OKADDEL    OK TO ADD DELETED RECORDS                    
         BZ    ADIT2               -NO                                          
         GOTO1 ACHKDELN,DMCB,(RC)  RETURNS CC EQUAL ONLY IF DELETED             
         BE    ADIT5                                                            
ADIT2    GOTO1 ADDREC              ADD RECORD TO FILE                           
         MVC   TWAKEYSV,KEY        KEY MAY HAVE CHANGED                         
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         B     ADIT5                                                            
*                                                                               
ADIT3    TM    GENSTAT1,OKADDEL    OK TO WRITE BACK DELETED RECORDS             
         BZ    ADIT4               ON AN ADD                                    
         GOTO1 ACHKDEL,DMCB,(RC)   RETURNS CC EQUAL ONLY IF DELETED             
         BE    ADIT5               RECORD ON FILE                               
ADIT4    GOTO1 ADD                                                              
*                                                                               
ADIT5    MVI   MODE,XRECADD                                                     
         BAS   RE,GO                                                            
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - CHANGE                                    
         SPACE 1                                                                
CHANGE   DS    0H                                                               
         TM    GENSTAT3,MULTFILS   IF SYSTEM HAS MULTIPLE FILES                 
         BZ    *+12                                                             
         MVI   MODE,SETFILE        GIVE APPLIC. A CHANCE TO SWITCH              
         BAS   RE,GO                                                            
         SPACE 1                                                                
         BAS   RE,READUP                                                        
         GOTO1 READ                                                             
         CLI   USEIO,C'Y'                                                       
         BE    CHANGE1                                                          
         BAS   RE,READUP                                                        
         GOTO1 GETREC                                                           
         MVC   GLOBDA,DMDSKADD                                                  
         SPACE 1                                                                
CHANGE1  CLC   TWALREC,RECNUM      IF NEW RECORD TYPE                           
         BNE   *+12                ALWAYS DISPLAY FIRST                         
         BRAS  RE,ANYKEY           SEE IF ANY KEY WAS INPUT                     
         BNE   CHANGE1B                                                         
         MVI   MODE,DISPREC        YES - SO DISPLAY RECORD                      
         BAS   RE,GO                                                            
         L     R2,AFRSTREC                                                      
         MVI   OKNO,4              AND ASK USER TO ENTER CHANGES                
         B     OKEX                                                             
         SPACE 1                                                                
CHANGE1B MVI   MODE,VALREC         NO KEY FIELDS CHANGED                        
         BAS   RE,CHANGE2          SO VALIDATE RECORD FIELDS                    
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,5                                                           
         CLI   TWALACT,ACTSEL      ARE WE STILL IN SELECT MODE?                 
         BNE   OKEX                                                             
         L     R1,EFHACT                                                        
         MVC   8(#SELPLQ,R1),LP@SELP RETURN TO SELECT NEXT TIME                 
         MVI   5(R1),6                                                          
         MVI   ACTNUM,ACTSEL                                                    
         B     OKEX                                                             
         SPACE 1                                                                
CHANGE2  NTR1                                                                   
         BAS   RE,GO               PASS VALIDATE RECORD MODE (PASSED)           
         MVI   MODE,RECPUT                                                      
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    CHANGE4                                                          
         CLI   USEIO,C'Y'                                                       
         BE    CHANGE3                                                          
         CLC   GLOBDA,DMWORK+4     PROTECT AGAINST PUTREC DRAMA                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUTREC              AND WRITE BACK                               
         B     CHANGE4                                                          
         SPACE 1                                                                
CHANGE3  GOTO1 WRITE                                                            
         SPACE 1                                                                
CHANGE4  MVI   MODE,XRECPUT                                                     
         BAS   RE,GO                                                            
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - DISPLAY - ACTIVITY                        
         SPACE 1                                                                
DISPLAY  GOTO1 READ                DISPLAY                                      
         CLI   USEIO,C'Y'                                                       
         BE    DISP1                                                            
         GOTO1 GETREC                                                           
         SPACE 1                                                                
DISP1    CLI   ACTIVSW,0                                                        
         BNE   DISP2                                                            
         MVI   MODE,DISPREC                                                     
         BAS   RE,GO                                                            
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,3                                                           
         B     OKEX                                                             
         SPACE 1                                                                
DISP2    MVI   OVERLAY,X'DF'       ACTIVITY - LOAD SCREEN                       
         L     R3,AFRSTRCH                                                      
         GOTO1 LOADEM,DMCB,1                                                    
*&&UK*&& BAS   RE,TRANSBTS                                                      
         MVI   TWASCR,X'DF'                                                     
         LR    R2,R3                                                            
         USING ACDD,R2                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP8                                                            
         USING ACTVD,R6                                                         
         SPACE 1                                                                
         MVC   ACDTXT1(#RECADLQ),LP@RECAD                                       
         LA    R3,ACTVADDT         ADD DETAILS ON LINE 1                        
         BAS   RE,DISPDID                                                       
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         CLI   ACTVCHNM,0          SHOW CHANGES ON LINE 2                       
         BE    DISP4                                                            
         MVC   ACDTXT1(#CHGNOLQ),LP@CHGNO    CHANGE NUMBER                      
         EDIT  (1,ACTVCHNM),(3,ACDCHNO),ALIGN=LEFT                              
         LA    R3,ACTVCHDT                                                      
         BAS   RE,DISPDID                                                       
         SPACE 1                                                                
DISP4    BAS   RE,BUMP             REASONS ON LINE 3                            
         OC    ACTVCHRE,ACTVCHRE                                                
         BZ    DISP6                                                            
         MVC   CHREASON,ACTVCHRE                                                
         MVI   MODE,XPREASON                                                    
         MVC   WORK,SPACES                                                      
         BAS   RE,GO                                                            
         CLC   WORK,SPACES                                                      
         BE    DISP6                                                            
         MVC   ACDTXT1(#CHGRLQ),LP@CHGR      CHANGE REASON                      
         MVC   ACDCHRSN,WORK                                                    
         SPACE 1                                                                
DISP6    CLI   1(RA),C'*'          DDS TERMINALS GET MORE                       
         BNE   DISP8                                                            
         BAS   RE,BUMP             DISK ADDRESS AND RECORD LENGTH               
         MVC   ACDTXT1(#DSKADLQ),LP@DSKAD                                       
         GOTO1 HEXOUT,DMCB,DMDSKADD,ACDDSKAD,4,0                                
         BAS   RE,BUMP                                                          
         MVC   ACDTXT1(#RECLNLQ),LP@RECLN                                       
         L     R3,AIO                                                           
         AH    R3,LKEY                                                          
         EDIT  (2,0(R3)),(4,ACDRECL),ALIGN=LEFT                                 
         SPACE 1                                                                
DISP8    MVI   ELCODE,X'F3'        SHOW SECURITY STAMPING                       
         BAS   RE,NEXTEL                                                        
         BNE   DISPEND                                                          
         BAS   RE,BUMP                                                          
         USING SECURED,R6                                                       
         MVC   ACDTXT1(#SECLQ),LP@SEC                                           
         CLI   SECURLEV,0                                                       
         BE    DISP10                                                           
         MVC   ACDTXT1(#SECLVLQ),LP@SECLV                                       
         MVI   ACDTXT1+#SECLVLQ+1,C'='                                          
         EDIT  (1,SECURLEV),(3,ACDSECLV),ALIGN=LEFT                             
         SPACE 1                                                                
DISP10   OC    SECURID,SECURID                                                  
         BZ    DISP12                                                           
         MVC   ACDTXT2(#IDLQ),LP@ID                                             
         MVI   ACDTXT2+#IDLQ+1,C'='                                             
         EDIT  (2,SECURID),(4,ACDID),ALIGN=LEFT                                 
         SPACE 1                                                                
DISP12   OC    SECURPAS,SECURPAS                                                
         BZ    DISPEND                                                          
         MVC   ACDTXT2(#PASSWLQ),LP@PASSW                                       
         SPACE 1                                                                
DISPEND  MVI   OKNO,14                                                          
         B     MODEX                                                            
         EJECT                                                                  
*              SHOW DATE AND ID OF ACTIVITY                                     
         SPACE 1                                                                
DISPDID  NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,(R3)),(8,ACDDATE)                                 
         MVC   ACDTXT2(#BYIDLQ),LP@BYID                                         
         EDIT  (2,3(R3)),(4,ACDIDNO),ALIGN=LEFT                                 
         MVC   AIO,AIO2            GET ID RECORD FROM CONTROL FILE              
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVC   SAVUSEIO,USEIO                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   GLOBKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),3(R3)                                                
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DISPDID2                                                         
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   DISPDID2                                                         
         USING CTDSCD,R6                                                        
         ZIC   R1,CTDSCLEN                                                      
         AHI   R1,-3                                                            
         CHI   R1,10                                                            
         BL    *+8                                                              
         LA    R1,10                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACDID(0),CTDSC                                                   
         SPACE 1                                                                
DISPDID2 MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         MVC   USEIO,SAVUSEIO                                                   
         MVC   KEY,GLOBKEY                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              MAINTENANCE ROUTINES - DELETE                                    
         SPACE 1                                                                
DELETE   OI    DMINBTS,X'08'       (PASS DELETED RECORDS)                       
         BAS   RE,READUP                                                        
         GOTO1 READ                DELETE                                       
         SPACE 1                                                                
         TM    GENSTAT4,CONFDEL    TEST CONFIRMATION OF DELETE REQ'D            
         BZ    DEL1                                                             
         CLC   TWALREC,RECNUM      IF NEW RECORD TYPE                           
         BNE   *+12                ALWAYS DISPLAY FIRST                         
         BRAS  RE,ANYKEY           SEE IF ANY KEY WAS INPUT                     
         BNE   DEL1                                                             
         CLI   USEIO,C'Y'                                                       
         BE    DEL0                                                             
         GOTO1 GETREC                                                           
DEL0     MVI   MODE,DISPREC        YES - SO DISPLAY RECORD                      
         BAS   RE,GO                                                            
         L     R2,EFHACT                                                        
         MVI   OKNO,24             AND ASK USER TO HIT ENTER TO DELETE          
         B     OKEX                                                             
         SPACE 1                                                                
DEL1     BAS   RE,DEL2             GO DELETE                                    
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,7                                                           
         B     OKEX                                                             
         SPACE 1                                                                
DEL2     NTR1                                                                   
         MVI   ERROR,RECISDEL                                                   
         CLI   USEIO,C'Y'                                                       
         BE    DEL4                                                             
         LA    R6,KEY              DELETE FOR DA FILES                          
*                                                                               
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
*                                                                               
         AH    R6,LKEY                                                          
         CLI   IOOPT,C'Y'                                                       
         BE    *+12                                                             
         TM    0(R6),X'80'                                                      
         BO    ERRXIT                                                           
         BAS   RE,READUP                                                        
         GOTO1 GETREC                                                           
         MVI   MODE,RECDEL                                                      
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    DEL3                                                             
         L     R6,AIO                                                           
         AH    R6,LKEY                                                          
         OI    2(R6),X'80'                                                      
         GOTO1 PUTREC                                                           
         LA    R6,KEY                                                           
*                                                                               
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
*                                                                               
         AH    R6,LKEY                                                          
         OI    0(R6),X'80'                                                      
         GOTO1 WRITE                                                            
DEL3     MVI   MODE,XRECDEL                                                     
         BAS   RE,GO                                                            
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         SPACE 1                                                                
DEL4     L     R6,AIO              DELETE FOR IS FILES                          
         AH    R6,LKEY                                                          
         CLI   IOOPT,C'Y'                                                       
         BE    *+12                                                             
         TM    2(R6),X'80'                                                      
         BO    ERRXIT                                                           
         MVI   MODE,RECDEL                                                      
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    DEL5                                                             
         OI    2(R6),X'80'                                                      
         GOTO1 WRITE                                                            
DEL5     MVI   MODE,XRECDEL                                                     
         BAS   RE,GO                                                            
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - RESTORE                                   
         SPACE 1                                                                
RESTORE  OI    DMINBTS,X'08'       RESTORE                                      
         BAS   RE,READUP                                                        
         GOTO1 READ                                                             
         BAS   RE,REST1                                                         
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,8                                                           
         B     OKEX                                                             
         SPACE 1                                                                
REST1    NTR1                                                                   
         MVI   ERROR,RECNTDEL                                                   
         CLI   USEIO,C'Y'                                                       
         BE    REST4                                                            
         LA    R6,KEY              RESTORE FOR DA FILES                         
*                                                                               
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
*                                                                               
         AH    R6,LKEY                                                          
         CLI   IOOPT,C'Y'                                                       
         BE    *+12                                                             
         TM    0(R6),X'80'                                                      
         BZ    ERRXIT                                                           
         BAS   RE,READUP                                                        
         GOTO1 GETREC                                                           
         MVI   MODE,RECREST                                                     
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    REST3                                                            
         L     R6,AIO                                                           
         AH    R6,LKEY                                                          
         NI    2(R6),X'7F'                                                      
         GOTO1 PUTREC                                                           
         LA    R6,KEY                                                           
*                                                                               
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
*                                                                               
         AH    R6,LKEY                                                          
         NI    0(R6),X'7F'         TURN OFF DELETE BIT                          
         GOTO1 WRITE                                                            
REST3    MVI   MODE,XRECREST                                                    
         BAS   RE,GO                                                            
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         SPACE 1                                                                
REST4    L     R6,AIO              RESTORE FOR IS FILES                         
         AH    R6,LKEY                                                          
         CLI   IOOPT,C'Y'                                                       
         BE    *+12                                                             
         TM    2(R6),X'80'                                                      
         BZ    ERRXIT                                                           
         MVI   MODE,RECREST                                                     
         BAS   RE,GO                                                            
         CLI   IOOPT,C'Y'                                                       
         BE    REST5                                                            
         NI    2(R6),X'7F'         TURN OFF DELETE BIT                          
         GOTO1 WRITE                                                            
REST5    MVI   MODE,XRECREST                                                    
         BAS   RE,GO                                                            
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - SELECT                                    
         SPACE 1                                                                
SELECT   DS    0H                                                               
         CLI   PFAID,0             TEST PFKEY PRESSED                           
         BE    SEL2                NO                                           
*                                                                               
         CLI   TWALACT,ACTSEL      TEST PREVIOUS ACTION WAS SELECT              
         BNE   SEL1                NO                                           
*                                                                               
         L     R1,SYSPARMS         SEE IF ANY CHANGES WERE MADE                 
         L     RE,0(R1)            A(TIOB)                                      
         CLI   SYSTEM,C'C'         TEST CONTROL SYSTEM                          
         BNE   *+8                 NO                                           
         L     RE,28(R1)           SYSPARMS IS DIFFERENT                        
         L     RF,ATWA                                                          
         USING TIOBD,RE                                                         
         AH    RF,TIOBLAST         A(LAST FIELD INPUT)                          
         DROP  RE                                                               
         C     RF,AFRSTREC         IF IT'S ON/AFTER 1ST DATA FIELD              
         BNL   SEL2                GO EDIT CHANGED DATA, ELSE ...               
*                                                                               
SEL1     MVI   MODE,PROCPFK        SET PFKEY MODE                               
         BAS   RE,GO               GIVE APPLICATION A SHOT                      
*                                                                               
         CLI   GENPFOPT,C'Y'       TEST SPECIAL OPT TO JUST DO PFKEY            
         BNE   SEL2                NO                                           
         MVI   OKNO,11             SET RECORD DISPLAYED MESSAGE                 
         NI    GENSTAT2,X'FF'-NEXTSEL  DO NOT GO TO NEXT SELECT                 
         B     SELX                                                             
*                                                                               
SEL2     LA    R3,LISTDIR          ADDRESS DIRECTORY                            
         ZIC   R0,LISTNUM          (MAX 18)                                     
         LTR   R0,R0                                                            
         BZ    ERRXIT                                                           
         SPACE 1                                                                
SEL4     TM    0(R3),X'FF'-X'40'   LOOK FOR FIRST/NEXT SELECTION                
         BNZ   SEL6                                                             
         LA    R3,6(R3)                                                         
         BCT   R0,SEL4                                                          
         ICM   R0,15,LASTSEL                                                    
         MVC   KEY,LASTSELK                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'LASTSELK),LASTSELK                                      
         BAS   RE,RESTLIST         NO MORE - RESTORE LIST SCREEN                
         BNE   ERRXIT              UNLESS NONE SAVED                            
         BAS   RE,TRANSBTS                   TURN ON TRANSMIT BITS              
         MVI   LISTSW,X'FF'        FORCE FOR RECAP                              
         STCM  R0,15,LASTSEL                                                    
         MVC   LASTSELK,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   LASTSELK,BIGKEY                                                  
         MVC   LASTLIST,LASTSEL    RE-START LIST AFTER LAST SEL.                
         B     ACT2                GO BACK TO LOAD APPLIC                       
         SPACE 1                                                                
SEL6     LLC   RE,LISTNUM          MOVE OUT DISK ADDRESS                        
         SR    RE,R0                                                            
         STC   RE,SELLISTN         SET RELATIVE LINE NUM                        
         MVC   THISLSEL,0(R3)      PASS THIS LIST SELECT CODE                   
         MVC   THISLARG,1(R3)      PASS THIS LIST ARGUMENT                      
*                                                                               
         TM    GENSTAT5,VRAHOOK    TEST APPLIC WANTS MODE VALRA PASSED          
         BZ    SEL6A                                                            
         CLI   THISLSEL,X'40'      HAVE A SELECT ACTION                         
         BNH   SEL6A               NO - NOT 1ST PASS                            
         MVI   MODE,VALRA          APPLIC CONTROLLER GETUSER MODE               
         GOTO1 GETUSER                                                          
*                                                                               
SEL6A    EQU   *                                                                
         CLI   0(R3),REPSELQ       TEST REPORT WAS SELECTED                     
         BE    SEL20                                                            
         BAS   RE,SELREAD          READ RECORD                                  
*                                                                               
         CLC   0(1,R3),LP@CHAS                                                  
         BE    SEL10                                                            
         TM    GENSTAT4,CONFDEL                                                 
         BZ    *+14                                                             
         CLC   0(1,R3),LP@DELS                                                  
         BE    SEL11                                                            
         CLI   0(R3),CHASELQ                                                    
         BE    SEL12                                                            
         CLI   0(R3),DELSELQ                                                    
         BE    SEL7D                                                            
         CLI   GCMODE,C'4'         TEST PROGRECS                                
         BNE   SEL7C                                                            
         CLC   0(1,R3),LP@REPS     TEST REPORT ACTION                           
         BE    SEL7R                                                            
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   SEL7C                                                            
         CLC   0(1,R3),LP@RESS     R=REP NOT R=RES                              
         BE    SEL7R                                                            
SEL7C    MVI   MODE,DISPKEY        SELECT AND DISPLAY                           
         BAS   RE,GO                                                            
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         BAS   RE,GO                                                            
         L     R2,EFHACT                                                        
         CLC   0(1,R3),LP@DELS                                                  
         BE    SEL7D                                                            
         CLC   0(1,R3),LP@RESS                                                  
         BE    SEL7G                                                            
         CLC   0(1,R3),LP@REPS                                                  
         BE    SEL7P                                                            
         B     SEL7X                                                            
         SPACE 1                                                                
SEL7D    BAS   RE,SELCLEAR         DELETE                                       
         BAS   RE,DEL2                                                          
         MVI   OKNO,13                                                          
         LA    R3,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
         SPACE 1                                                                
SEL7E    CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BH    SELX                                                             
         LA    R3,6(R3)                                                         
         BCT   R0,SEL7E            NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     SELX                                                             
         SPACE 1                                                                
SEL7P    BAS   RE,SELCLEAR         PRINT REPORT                                 
         L     R2,EFHREC                                                        
         MVC   REMUSER,8(R2)                                                    
         GOTO1 STRTPRNT                                                         
         L     R2,AFRSTKEY                                                      
         BAS   RE,CONPRINT         CONTROL PRINTING                             
         B     SEL2                                                             
         SPACE 1                                                                
SEL7G    BAS   RE,SELCLEAR         RESTORE                                      
         BAS   RE,REST1                                                         
         MVI   OKNO,18                                                          
         LA    R3,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
         SPACE 1                                                                
SELH     CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BH    SELX                                                             
         LA    R3,6(R3)                                                         
         BCT   R0,SELH             NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     SELX                                                             
         SPACE 1                                                                
SEL7R    MVI   MODE,DISPKEY        DISPLAYING FOR REPORT                        
         BAS   RE,GO                                                            
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         BAS   RE,GO                                                            
         MVI   0(R3),REPSELQ                                                    
         L     R2,EFHWHEN                                                       
         CLC   8(#NEXTLQ,R2),LP@NEXT IF 'NEXT' AROUND FROM LAST TIME            
         BNE   *+14                                                             
         XC    8(4,R2),8(R2)       CLEAR IT                                     
         OI    6(R2),X'80'                                                      
         MVI   OKNO,30             (HIT ENTER TO GENERATE REPORT)               
         CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         BNE   MODEX               NO                                           
         MVI   PQSW,X'FF'                                                       
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         B     MODEX                                                            
         SPACE 1                                                                
SEL7X    BAS   RE,SELCLEAR                                                      
         MVI   OKNO,11             (SELECTION DISPLAYED)                        
SEL7XX   LA    R3,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
         SPACE 1                                                                
SEL8     CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BH    SELX                                                             
         LA    R3,6(R3)                                                         
         BCT   R0,SEL8             NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     SELX                                                             
         SPACE 1                                                                
SEL10    MVI   0(R3),CHASELQ       DISPLAYING FOR CHANGE                        
         MVI   OKNO,4              (RECORD DISPLAYED)                           
         B     *+12                                                             
SEL11    MVI   0(R3),DELSELQ       DISPLAYING FOR DELETE                        
         MVI   OKNO,24             (REC DISPLAYED - ENTER TO DELETE)            
         MVI   MODE,DISPKEY                                                     
         BAS   RE,GO                                                            
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         BAS   RE,GO                                                            
         L     R2,AFRSTREC                                                      
         B     SELX                                                             
         SPACE 1                                                                
SEL12    TM    GENSTAT5,GENSELVR   ALWAYS GO TO VALREC                          
         BO    SEL14                                                            
         OC    AFRSTREC,AFRSTREC   ARE THERE ANY FIELDS TO CHANGE               
         BZ    SEL16                                                            
         L     R1,SYSPARMS         SEE IF ANY CHANGES WERE MADE                 
         L     RE,0(R1)            A(TIOB)                                      
         CLI   SYSTEM,C'C'         TEST CONTROL SYSTEM                          
         BNE   *+8                 NO                                           
         L     RE,28(R1)           SYSPARMS IS DIFFERENT                        
         L     R2,ATWA                                                          
         USING TIOBD,RE                                                         
         AH    R2,TIOBLAST         A(LAST FIELD INPUT)                          
         DROP  RE                                                               
         C     R2,AFRSTREC         IF IT'S ON/AFTER 1ST DATA FIELD              
         BL    SEL16                                                            
         SPACE 1                                                                
SEL14    MVI   MODE,VALREC         SET VALIDATE RECORD MODE                     
         BAS   RE,CHANGE2          AND GO HANDLE CHANGES                        
         SPACE 1                                                                
SEL16    CLI   GENPFOPT,C'Y'                                                    
         BE    *+12                                                             
         TM    GENSTAT2,RETEQSEL                                                
         BZ    SEL18                                                            
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         L     R2,AFRSTREC                                                      
         MVI   OKNO,5              (RECORD CHANGED)                             
         B     SELX                                                             
         SPACE 1                                                                
SEL18    BAS   RE,SELCLEAR         GO AND GET ANOTHER SELECTION                 
         B     SEL2                                                             
         SPACE 1                                                                
SEL20    CLI   WHEN,0              TEST PRINT OPTION 'NO'                       
         BE    SEL22               YES - IGNORE SELECTION                       
         SPACE 1                                                                
         BAS   RE,SREPORT          GENERATE REPORT REQUEST                      
         SPACE 1                                                                
         L     RF,EFHWHEN          RF=A(PRINT FIELD HEADER)                     
         TM    WHEN,X'80'          IF REPORT IS ON-SCREEN                       
         BZ    SEL21                                                            
         MVC   8(#NEXTLQ,RF),LP@NEXT  SET TO SKIP 'NEXT' TIME                   
         OI    6(RF),X'80'                                                      
         B     MODEX               RETURN TO USER NOW                           
         SPACE 1                                                                
SEL21    L     RE,AREPINFO         A(REPORT ID INFO TABLE)                      
         ZIC   R1,SELLISTN         RELATIVE SELECT NO.                          
         MH    R1,=H'6'            DISP TO THIS SELECTION IN TABLE              
         AR    RE,R1               A(THIS SELECTION)                            
         SPACE 1                                                                
         CLI   OKNO,22             TEST 'NO DATA GENERATED'                     
         BNE   *+12                NO                                           
         MVI   0(RE),C'X'                                                       
         B     SEL22                                                            
         SPACE 1                                                                
         MVC   0(1,RE),8(RF)       'WHEN' CODE (N, S, O, D)                     
         MVC   1(3,RE),REMUSER     3-CHAR REQUESTOR                             
         CLI   3(RE),C' '          PAD WITH ASTERISKS                           
         BNE   *+8                                                              
         MVI   3(RE),C'*'                                                       
         CLI   2(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   2(RE),C'*'                                                       
         CLI   1(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   1(RE),C'*'                                                       
         SPACE 1                                                                
         CLI   TWAWHEN,4           TEST 'OVERNIGHT'                             
         BE    SEL22               YES                                          
         SPACE 1                                                                
         CLI   TWAWHEN,0           TEST 'NOW'                                   
         BNE   *+14                NO                                           
         MVC   4(2,RE),SPOOLRPN    REPORT NUMBER                                
         B     SEL22                                                            
         SPACE 1                                                                
         CLI   TWAWHEN,5           UPDATIVE SOON?                               
         BE    SEL21C                                                           
         CLI   TWAWHEN,2           TEST 'SOON'                                  
         BE    *+6                                                              
         DC    H'0'                                                             
SEL21C   MVC   4(2,RE),HALF        REPORT NUMBER                                
         SPACE 1                                                                
SEL22    BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
         BE    SEL18                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
SELX     TM    GENSTAT2,NEXTSEL    UNLESS REQUESTED TO DO OTHERWISE             
         BZ    MODEX               RETURN CONTROL TO USER                       
         NI    GENSTAT2,X'FF'-(NEXTSEL+RETEQSEL)                                
         BAS   RE,SELCLEAR         CLEAR THIS SELECTION                         
         B     SEL2                APPLIC. WANTS NEXT SELECTION                 
         SPACE 2                                                                
SELCLEAR DS    0H                                                               
         TM    GENSTAT2,RETEQSEL   UNLESS REQUESTED TO DO OTHERWISE             
         BO    *+8                                                              
         MVI   0(R3),0             CLEAR THIS SELECTION                         
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO READ A SELECTED RECORD                                
         SPACE 1                                                                
*                                  R3=A(LISTDIR ENTRY)                          
SELREAD  NTR1                                                                   
         TM    GENSTAT3,MULTFILS   IF SYSTEM HAS MULTIPLE FILES                 
         BZ    SELR10                                                           
*                                                                               
         LA    R4,KEY              POINT R4 TO KEY                              
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R4,BIGKEY                                                        
         AH    R4,LKEY                                                          
         AH    R4,LSTATUS                                                       
         MVC   0(4,R4),2(R3)       MOVE DISK ADDRESS TO KEY FOR USER            
*                                                                               
         MVI   MODE,SETFILE        GIVE APPLIC. A CHANCE TO SWITCH              
         BAS   RE,GO                                                            
*                                                                               
         CLI   MODE,UPDATEDA       DID APP SET NEW DISK ADDRESS                 
         BNE   SELR10                                                           
         MVC   2(4,R3),0(R4)       UPDATE DA IN LIST ENTRY!                     
*                                                                               
SELR10   OI    DMINBTS,X'08'       RECORDS MAY BE DELETED                       
         CLI   USEIO,C'Y'                                                       
         BE    SELR30                                                           
*                                                                               
         LA    R4,KEY              SET R4 = A(KEY)                              
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R4,BIGKEY                                                        
*                                                                               
         LR    R1,R4               MOVE OUT DISK ADDRESS                        
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   0(4,R1),2(R3)                                                    
         MVC   LASTSEL,2(R3)       SAVE DA OF LAST SELECTION                    
*                                                                               
         OC    LASTSEL,LASTSEL     TEST DSKAD = 0                               
         BZ    SELR50              YES - NOT A REAL DISK ADDRESS                
*                                                                               
         CLI   0(R3),CHASELQ       IF WE'RE ABOUT TO CHANGE                     
         BNE   *+8                                                              
         BAS   RE,READUP           THEN MAKE SURE WE READ FOR UPDATE            
*                                                                               
         TM    GENSTAT7,GES7RSEL   TEST APP WANTS TO DO THIS READ               
         BZ    SELR20                                                           
         MVI   MODE,READSEL        READ FILE                                    
         BAS   RE,GO                                                            
         B     SELR50                                                           
*                                                                               
SELR20   GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         MVC   GLOBDA,DMDSKADD                                                  
         L     R6,AIO                                                           
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)       MOVE OUT THE KEY                             
*                                                                               
         CLC   0(1,R3),LP@DELS     IF WE'RE ABOUT TO DELETE                     
         BE    *+14                                                             
         CLC   0(1,R3),LP@RESS     OR WE'RE ABOUT TO RESTORE                    
         BNE   *+8                                                              
         BAS   RE,READUP           THEN MAKE SURE WE READ FOR UPDATE            
*                                                                               
         GOTO1 READ                READ KEY                                     
         B     SELR50                                                           
         SPACE 1                                                                
SELR30   LA    RF,LISTKEYS         FOR IS FILES DIG OUT KEY                     
         ZIC   RE,SELLISTN                                                      
         MH    RE,LKEY                                                          
         AR    RF,RE                                                            
         XC    KEY,KEY                                                          
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),0(RF)                                                     
         MVC   LASTSELK,KEY        SAVE LAST SELECTED FOR NEXT TIME             
         CLI   0(R3),CHASELQ       TEST FOR CHA,DEL,RESTORE                     
         BE    SELR40                                                           
         CLC   0(1,R3),LP@DELS                                                  
         BE    SELR40                                                           
         CLC   0(1,R3),LP@RESS                                                  
         BNE   *+8                                                              
SELR40   BAS   RE,READUP           INSURE READ FOR UPDATE SET                   
         GOTO1 READ                                                             
         SPACE 1                                                                
SELR50   NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - LIST                                      
         SPACE 1                                                                
LIST     DC    0H'0'                                                            
         XC    KEY,KEY                                                          
         XC    BIGKEY,BIGKEY                                                    
         TM    WHEN,X'80'          ONLY HANDLING ON-SCREEN HERE                 
         BNO   REPORT                                                           
         CLI   TWALACT,ACTSEL      WAS THE LAST ACTION SELECT?                  
         BNE   LIST1                                                            
         CLC   TWALREC,RECNUM      (UNLESS CHANGE OF RECORD)                    
         BNE   LIST1                                                            
         BAS   RE,RESTLIST         YES - NEED TO RESTORE LIST SCREEN            
         BNE   LIST1               UNLESS NONE SAVED                            
         BAS   RE,TRANSBTS                                                      
         B     LIST2                                                            
         SPACE 1                                                                
LIST1    CLI   LISTSW,0                                                         
         BNE   LIST2                                                            
         BRAS  RE,ANYKEY           IF KEY GOT INPUT AGAIN                       
         BNE   LIST2                                                            
         MVI   LISTSW,C'F'         CONSIDER THIS TO BE FIRST                    
         XC    VERYFRST,VERYFRST                                                
         SPACE 1                                                                
LIST2    MVI   TWALACT,ACTLIST                                                  
         CLI   LISTSW,C'F'         ADJUST D/A DIRECTORIES FOR SPECIAL           
         BE    LIST8               LIST FUNCTIONS                               
         CLI   LISTSW,C'L'                                                      
         BE    LIST6                                                            
         CLI   LISTSW,C'N'                                                      
         BE    LIST4                                                            
         CLI   LISTSW,C'T'                                                      
         BE    LIST10                                                           
         SPACE 1                                                                
         BAS   RE,ANYCHGS          IF ANY CHANGES MADE TO LIST DATA             
         BNE   *+16                                                             
         BAS   RE,VALLIST          HANDLE VALIDATION OF LISTED RECORDS          
         MVI   LISTSW,C'T'         SET TO RE-DISPLAY THIS PAGE                  
         B     LIST2                                                            
         SPACE 1                                                                
         BRAS  RE,ANYSELS          IF ANY SELECTIONS WERE MADE                  
         BE    LIST20              GO HANDLE SELECTED RECORDS                   
         MVI   LISTSW,C'N'                                                      
         SPACE 1                                                                
LIST4    L     RF,AREPINFO         TEST ANY REPORT INFO SAVED                   
         OC    0(L'REPINFO,RF),0(RF)                                            
         BZ    LIST5               NO                                           
         SPACE 1                                                                
         L     R2,AFRSTREC                                                      
LIST4A   BAS   RE,BUMP             BUMP TO LISTAR AREA                          
         LA    R1,58(R2)           BUMP PAST HEADER, NAME, DESCRIPTION          
         XC    0(11,R1),0(R1)      CLEAR EXISTING DATA ON SCREEN                
         CLI   0(RF),0             TEST ANY SAVED INFO FOR THIS ITEM            
         BE    LIST4B              NO                                           
         CLI   0(RF),C'X'          TEST 'NO DATA GENERATED'                     
         BNE   *+14                NO                                           
         MVCDD 0(#NODATLQ,R1),GE#NODAT                                          
         B     LIST4B                                                           
         SPACE 1                                                                
         MVC   0(1,R1),0(RF)       MOVE IN 'WHEN' INDICATOR                     
         MVI   1(R1),C'/'                                                       
         MVC   2(3,R1),1(RF)       REQUESTOR INITIALS                           
         CLC   0(1,RF),LP@O        TEST OVERNIGHT                               
         BE    LIST4B              YES                                          
         MVI   5(R1),C','          FILL IN REPORT NUMBER                        
         EDIT  (2,4(RF)),(5,6(R1)),ALIGN=LEFT                                   
         SPACE 1                                                                
LIST4B   LA    RF,6(RF)            BUMP TO NEXT ENTRY IN REPORT TABLE           
         BAS   RE,BUMP             BUMP TO NEXT SELECT FIELD                    
         CLI   0(R2),9             TEST ANY MORE SELECTS                        
         BH    LIST4A              YES                                          
         SPACE 1                                                                
         L     RF,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RF),0(RF)                                            
         L     R2,AFRSTREC                                                      
         MVI   OKNO,9              LIST DISPLAYED - SELECT OR HIT ENTER         
         CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         BNE   MODEX               NO                                           
         MVI   PQSW,X'FF'                                                       
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         B     MODEX                                                            
         SPACE 1                                                                
LIST5    MVC   PAGEDAS,PAGEDAS+4             NEXT                               
         MVC   PAGEDAS+60(4),LASTLIST                                           
         B     LIST10                                                           
         SPACE 1                                                                
LIST6    MVC   WORK,PAGEDAS                  LAST                               
         MVC   PAGEDAS+4(60),WORK                                               
         XC    PAGEDAS(4),PAGEDAS                                               
         XC    LASTSELK,LASTSELK                                                
         B     LIST10                                                           
         SPACE 1                                                                
LIST8    XC    PAGEDAS,PAGEDAS               FIRST                              
         XC    LASTSELK,LASTSELK                                                
         MVC   PAGEDAS+60(4),VERYFRST                                           
         SPACE 1                                                                
LIST10   XC    LISTDIR(108),LISTDIR                                             
         MVI   LISTNUM,0                                                        
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
         MVI   THISLSEL,0          ENSURE NO SELECT CODE AROUND                 
         XC    KEY,KEY                                                          
         XC    BIGKEY,BIGKEY                                                    
         SPACE 1                                                                
         TM    GENSTAT3,MULTFILS   IF SYSTEM HAS MULTIPLE FILES                 
         BZ    *+12                                                             
         MVI   MODE,SETFILE        GIVE APPLIC. A CHANCE TO SWITCH              
         BAS   RE,GO                                                            
         SPACE 1                                                                
         CLI   USEIO,C'Y'                                                       
         BNE   LIST11                                                           
         OC    LASTSELK,LASTSELK                                                
         BZ    LIST12                                                           
         MVC   KEY,LASTSELK        RESTORE FOR IS FILES                         
         OI    DMINBTS,X'08'       IN CASE LAST RECORD WAS JUST DELETED         
         GOTO1 READ                                                             
         NI    DMINBTS,X'F7'                                                    
         B     LIST12                                                           
         SPACE 1                                                                
LIST11   OC    PAGEDAS+60(4),PAGEDAS+60                                         
         BZ    LIST12                                                           
         LA    R4,KEY              SET R4 = A(KEY)                              
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R4,BIGKEY                                                        
         LR    R1,R4               RESTORE THIS RECORD                          
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   0(4,R1),PAGEDAS+60                                               
         SPACE 1                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)               AND KEY                              
         MVI   MODE,LISTKEY                                                     
         BAS   RE,GO                                                            
         GOTO1 READ                                                             
         NI    DMINBTS,X'F7'                                                    
         CLI   LISTSW,C'N'                                                      
         BNE   LIST12                                                           
         GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST12   CLI   GCMODE,C'4'         DON'T LOAD IF PROG REC                       
         BE    *+8                                                              
         BAS   RE,REPLOAD                                                       
         L     R2,AFRSTREC                                                      
         BAS   RE,TESTSEL          IF SELECT FIELD PRESENT                      
         BNE   *+8                                                              
         BAS   RE,BUMP             BUMP PAST IT                                 
         ST    R2,ATHISLST         NOTE FIRST LIST                              
*                                                                               
         MVI   MODE,LISTRECS                                                    
         BAS   RE,GO                                                            
*                                                                               
         XC    LASTLIST,LASTLIST                                                
         XC    LASTSELK,LASTSELK                                                
*                                                                               
         ZIC   R4,NLISTS           CLEAR FIELDS REMAINING FIELDS                
         ZIC   R0,LISTNUM                                                       
         SR    R4,R0               R4=NUMBER OF FIELDS LEFT                     
         BNP   LIST18                                                           
         L     R2,ATHISLST         R2=A(1ST REMAINING LINE)                     
LIST14   LR    R3,R2                                                            
         AH    R3,LLIST            R3=A(1ST FIELD ON NEXT LINE)                 
         GOTOR CLEARFLD            CLEAR ALL FIELDS ON THIS LINE                
         BAS   RE,BUMP                                                          
         CR    R2,R3                                                            
         BL    *-10                                                             
         BAS   RE,TESTSEL          IF SELECT FIELD PRESENT                      
         BNE   *+8                                                              
         BAS   RE,BUMP             BUMP PAST IT                                 
         BCT   R4,LIST14                                                        
         SPACE 1                                                                
LIST18   MVI   OKNO,16             END OF LIST - HIT ENTER FOR ...              
         L     R2,ATHISLST                                                      
         BAS   RE,BUMP                                                          
         BAS   RE,TESTSEL          TEST IF SELECT FIELD PRESENT                 
         BNE   *+8                                                              
         MVI   OKNO,10             END OF LIST - SELECT OR HIT ENTER...         
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,33             END OF LIST - INPUT CHANGES OR ...           
         L     R2,AFRSTREC                                                      
         B     MODEX                                                            
         SPACE 1                                                                
LIST20   BAS   RE,SAVELIST         USER SELECTED SOME RECORD(S)                 
         L     R1,EFHACT                                                        
         MVC   8(#SELPLQ,R1),LP@SELP       SO SAVE LIST SCREEN AND              
         MVI   5(R1),6                       CHANGE ACTION TO SELECT            
         OI    6(R1),X'80'                                                      
         MVI   TWALACT,ACTSEL                                                   
         B     ACT2                AND GO BACK TO LOAD SCREEN & APPLIC          
         SPACE 3                                                                
TESTSEL  DS    0H                  * TEST IF SELECT FIELD PRESENT               
         TM    GLSTSTAT,NOSELFLD   IF EXPLICITLY STATED NO                      
         BO    TSELNO                                                           
         TM    1(R2),X'20'         OR IF FIELD IS PROTECTED                     
         BO    TSELNO                                                           
         TM    1(R2),X'0C'         OR ZERO INTENSITY                            
         BO    TSELNO              THEN NO SELECT FIELD - RETURN CC NE          
         CR    RE,RE                                                            
         BR    RE                  ELSE RETURN CC EQ                            
TSELNO   LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*              MAINTENANCE ROUTINES - REPORT                                    
         SPACE 1                                                                
SREPORT  NTR1                      REPORT WAS SELECTED FROM LIST                
         L     R2,EFHACT                                                        
         MVC   8(#REPLQ,R2),LP@REP FORCE ACTION TO REPORT                       
         MVI   5(R2),#REPLQ                                                     
         SPACE 1                                                                
REPORT   CLI   GCMODE,C'4'         IF PROG RECS REPORT. . .                     
         BNE   REPORT0             . . . THEN LET USER VALIDATE REQUEST         
         MVI   MODE,VALREC                                                      
         BAS   RE,GO                                                            
REPORT0  CLI   TWAWHEN,0           IF OVERNIGHT OR SOON WAS REQUESTED           
         BE    REP20                  WRITE OUT REQUESTS NOW                    
         CLI   OFFLINE,C'Y'        UNLESS WE ARE OFFLINE NOW                    
         BE    REP20                                                            
VREQ     XC    IO(26),IO           ENTER HERE TO JUST GENERATE REQUEST          
         MVC   IO+26(80),SPACES                                                 
         LA    R4,IO+26                                                         
         MVC   0(2,R4),QCRDCODE    COMPLETE REQUEST                             
*&&US*&& MVC   2(2,R4),AGENCY                                                   
*&&UK*&& MVC   2(2,R4),TWAAGY                                                   
         CLI   RCPROG,C'A'         FOR ACCOUNTING SYSTEMS                       
         BNE   *+8                                                              
         MVI   3(R4),C'*'          SHOW THIS IS A SPOOL REQUEST                 
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
         EDIT  (4,FASIN),(6,IO+31),FILL=0                                       
         ZIC   R4,REQSEQNO                                                      
         LA    R4,1(R4)                                                         
         STC   R4,REQSEQNO                                                      
         LA    R4,REQSEQTAB(R4)                                                 
         MVC   IO+37(1),0(R4)                                                   
         LA    R4,IO                                                            
         USING REQOFFC,R4                                                       
         MVC   REQOUT,TWAOUT                                                    
         MVC   REQDEST,TWADEST                                                  
         TM    GENSTAT7,GES7COMS   COMSCORE OVERNIGHT REQUEST?                  
         BZ    *+8                 NO                                           
         MVI   REQCTRL,X'01'       YES                                          
         SPACE 1                                                                
REP0     LA    R1,BLOCK            SET SPOOK FOR SOON PROCESSING                
         USING SPOOK,R1                                                         
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,TWAORIG                                                 
         MVC   SPOOKDES,TWADEST                                                 
         MVC   SPOOKTID,2(RA)                                                   
         MVC   SPOOKSEN,SYSPHASE+1                                              
         MVC   SPOOKERN,GETMSYS                                                 
         MVC   SPOOKAGY,TWAAGY                                                  
         L     RE,SYSPARMS                                                      
         MVC   SPOOKAGX,0(RE)                                                   
         MVC   SPOOKDID,REMUSER                                                 
         MVC   SPOOKSYS,RCPROG                                                  
         MVC   SPOOKEOD,RCPROG+2                                                
         MVC   SPOOKJCL,QCRDCODE                                                
         MVC   SPOOKPR1,REQPRI1                                                 
         MVC   SPOOKPR2,REQPRI2                                                 
         MVC   SPOOKSML,REQSML                                                  
         MVC   SPOOKWEN,TWAWHEN                                                 
         MVC   SPOOKXT,=C'XT='                                                  
         MVC   SPOOKTY,REQRTYP                                                  
         CLI   SPOOKDID+2,C' '                                                  
         BNE   REP0B                                                            
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BNE   REP0B                                                            
         MVI   SPOOKDID+1,C'*'                                                  
         CLI   SPOOKDID+0,C' '                                                  
         BNE   REP0B                                                            
*                                                                               
         XC    SPOOKDID,SPOOKDID                                                
*                                                                               
REP0B    CLI   TWAOUT,C'@'                                                      
         BNE   REP0C                                                            
         MVC   SPOOKSQL,TWAOUT+1                                                
         OI    SPOOKTY,X'08'                                                    
         B     REP0D                                                            
REP0C    CLI   TWAOUT,C'/'                                                      
         BNE   REP0D                                                            
         CLC   TWAOUT+3(3),SPACES                                               
         BNE   *+14                                                             
         MVC   SPOOKSUB,TWAOUT+1   /XX INPUT                                    
         B     REP10                                                            
         MVC   SPOOKSUB,TWAOUT+4   /SPPXX INPUT                                 
         B     REP10                                                            
REP0D    EQU   *                                                                
*                                                                               
         DROP  R1                                                               
*                                                                               
REP10    DS    0H                                                               
*                                                                               
         OC    ARFPBLK,ARFPBLK     IF RFP REQUESTED                             
         BZ    REP19                                                            
*                                                                               
*        MAKE SURE THERE IS ROOM FOR 3 CHARACTER REQUESTOR                      
*                                                                               
         L     R2,EFHWHEN          SCAN FOR REQUESTOR INITIALS                  
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),X'6B7EFF6B'  C',=X,'                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1         GET REQUESTOR LENGTH                         
         BNZ   REP12                                                            
         LA    RF,8(R2)            NO REQUESTOR NAME                            
*                                                                               
         CLI   0(RF),C' '          INSERT COMMA                                 
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(RF),C','                                                       
         LA    RF,4                AND LEAVE ROOM FOR 3 CHARS                   
         B     *+8                                                              
REP12    LA    RF,3                MAX REQUESTOR LENGTH                         
         SR    RF,RE               EXCESS TO BE ADDED                           
         IC    RE,5(R2)            L'WHEN FIELD                                 
         AR    RE,RF               NEW LENGTH                                   
         STC   RE,5(R2)            UPDATE L'WHEN                                
*                                                                               
         ICM   RE,15,EFHDEST       PRE-FILL DESTINATION FIELD                   
         BZ    REP13                                                            
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         SHI   RF,9                                                             
         TM    1(RE),X'02'                                                      
         BZ    *+8                                                              
         SHI   RF,8                                                             
         STC   RF,WORK+00                                                       
         MVC   WORK+01(19),8(RE)                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),=C'&&&&DST&&&&&&&&&&&&&&'                                
         AHI   RF,1                                                             
         STC   RF,5(RE)                                                         
*                                                                               
REP13    ICM   RE,15,EFHOUT        PRE-FILL OUTPUT FIELD                        
         BZ    REP14                                                            
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         SHI   RF,9                                                             
         TM    1(RE),X'02'                                                      
         BZ    *+8                                                              
         SHI   RF,8                                                             
         STC   RF,WORK+20                                                       
         MVC   WORK+21(19),8(RE)                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),=C'&&&&OUT&&&&&&&&&&&&&&'                                
         AHI   RF,1                                                             
         STC   RF,5(RE)                                                         
*                                                                               
REP14    GOTO1 REQTWA,DMCB,(RA),IO,ARFPBLK,(X'80',ACOMFACS),BLOCK,     X        
               EFHWHEN             ADD TO FILE                                  
         ICM   R2,15,EFHDEST                                                    
         BZ    REP15                                                            
         GOTOR CLEARFLD                                                         
         IC    RF,WORK+00                                                       
         EX    RF,*+8                                                           
         B     REP15                                                            
         MVC   8(0,R2),WORK+01                                                  
                                                                                
REP15    ICM   R2,15,EFHOUT                                                     
         BZ    REP16                                                            
         GOTOR CLEARFLD                                                         
         IC    RF,WORK+20                                                       
         EX    RF,*+8                                                           
         B     REP16                                                            
         MVC   8(0,R2),WORK+21                                                  
*                                  CHECK FOR RFP ERRORS                         
REP16    L     RE,ARFPBLK          POINT TO RFP CONTROL BLOCK                   
         CLI   RFPERROR-RFPBLK(RE),RFPNOERR OKAY IF NO ERRORS                   
         BNE   *+12                                                             
         MVI   OKNO,131            GROUP UPDATED WITH REQUEST                   
         B     REPEXIT             DONE                                         
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSYS,X'FF'        GENERAL MSG SYSTEM                           
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
         CLI   RFPERROR-RFPBLK(RE),RFPNOROO   MAX REQUESTS                      
         BNE   *+12                                                             
         MVI   GTMSGNO1,251        NO ROOM IN GROUP RECORD                      
         B     ERRXIT                                                           
*                                                                               
         CLI   RFPERROR-RFPBLK(RE),RFPNOADD   GROUP SUBMITTED?                  
         BNE   *+12                                                             
         MVI   GTMSGNO1,254        CAN'T ADD REQS TO SUBMITTED GROUP            
         B     ERRXIT                                                           
         DROP  R1                                                               
*                                                                               
*                                                                               
REP19    DS    0H                                                               
*                                                                               
         GOTO1 REQTWA,DMCB,(RA),IO,DATAMGR,ACOMFACS,BLOCK                       
         MVI   OKNO,20             REP WILL BE PROCESSED O/NIGHT                
         CLI   TWAWHEN,5           UPDATIVE SOON?                               
         BE    *+12                                                             
         CLI   TWAWHEN,2                                                        
         BNE   REPEXIT                                                          
         CLI   8(R1),X'FE'                                                      
         BNE   *+12                                                             
         MVI   ERROR,TQUEFULL      TERMINAL QUEUE IS FULL                       
         B     ERRXIT                                                           
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,PQUEFULL PRINT QUEUE FULL                                  
         B     ERRXIT                                                           
*                                                                               
         MVI   OKNO,21             REPORT &T WILL BE PROCESSED SOON             
*                                                                               
         L     RE,8(R1)            GET ADDRESS OF PRTQUE KEY                    
         MVC   BLOCK(3),2(RE)      BUILD XXX,9999 FOR &T SUBSTITUTION           
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         BNZ   *+12                                                             
         MVI   ERROR,NOJCL         JCL NOT FOUND                                
         B     ERRXIT                                                           
*                                                                               
         STH   R0,HALF             SAVE REPORT NUMBER IN HALF                   
         LA    R4,BLOCK                                                         
         MVI   3(R4),C','                                                       
         EDIT  (R0),(5,4(R4)),ALIGN=LEFT                                        
         LA    R1,GETTXTCB         SET A(TEXT) AND L'TEXT (XXX,99999)           
         STCM  R4,7,GTATXT-GETTXTD(R1)                                          
         MVI   GTLTXT-GETTXTD(R1),9                                             
         B     REPEXIT                                                          
REQSEQTAB DC   C'  ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'                        
         SPACE 1                                                                
REP20    MVC   AOVERLAY,AGO                                                     
         CLI   PHSPECS,0           DO WE NEED TO LOAD A SPEC PHASE              
         BE    REP40                                                            
         MVC   OVERLAY,PHSPECS                                                  
         GOTO1 LOADEM,DMCB,0       YES                                          
         ST    R3,SPECS                                                         
         SPACE 1                                                                
REP40    BAS   RE,REPLOAD          LOAD REPORT PHASE IF NEEDED                  
         MVC   BUFFALO,TWAVBUFF    XTRNS FOR OFF-LINE VERSIONS                  
         MVC   SORTER,TWAVSORT                                                  
         MVC   WORKER,TWAVWORK                                                  
         ICM   RE,15,TWAVBOX       REQUIRE DICTIONARY SUPPORT                   
         BZ    REP43                                                            
*&&UK*&& OI    BOXDDCTL-BOXD(RE),BOXDDREQ+BOXDDLC                               
*&&US*&& OI    BOXDDCTL-BOXD(RE),BOXDDREQ                                       
         ICM   R1,15,TWAMASTC                                                   
         BZ    REP43                                                            
         MVC   BOXSYS-BOXD(,RE),MCOVSYS-MASTD(R1)                               
         MVC   BOXLANG-BOXD(,RE),MCLANG-MASTD(R1)                               
         MVC   BOXCTRY-BOXD(,RE),MCCTRY-MASTD(R1)                               
         SPACE 1                                                                
REP43    CLI   TWAFIRST,0          FIRST TIME HOOK                              
         BNE   REP50                                                            
         ICM   RF,15,TWAMASTC      IF A(MASTER) RESOLVED (OFFLINE)              
         BZ    REP45                                                            
         USING MASTD,RF                                                         
         TM    MCPRTIND,MCPRTINL   AND LOGOS DEFERRED/DISABLED                  
         BZ    REP45                                                            
         GOTO1 MCVLOGO,DMCB,MCVLOGOC  ATTEMPT TO DO THEM NOW                    
         DROP  RF                                                               
REP45    CLI   FRSTLAST,C'N'       UNLESS SUPPRESSED                            
         BE    REP50                                                            
         MVI   TWAFIRST,1                                                       
         MVI   MODE,RUNFRST                                                     
         BAS   RE,GO                                                            
         B     REP55                                                            
         SPACE 1                                                                
REP50    CLI   TWAFIRST,X'FF'       LAST TIME HOOK                              
         BNE   REP55                                                            
         MVI   MODE,RUNLAST                                                     
         BAS   RE,GO                                                            
         B     EXIT                                                             
         SPACE 1                                                                
REP55    CLI   PQSW,0              UNLESS USER OPTS TO OPEN PQ                  
         BNE   REP60                                                            
         TM    WHEN,X'80'          OR REPORT IS ON SCREEN                       
         BO    REP60                                                            
         GOTO1 OPENPQ              OPEN PQ                                      
         SPACE 1                                                                
REP60    OC    VPRINT,VPRINT                                                    
         BZ    REP80                                                            
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   SPOOLRPN,MCREMPQK+5 SET REPORT NUMBER FOR SPECS                  
         DROP  R1                                                               
         BAS   RE,PRREQDET         TEST PRINTING REQUEST DETAILS                
         BNE   REP70                                                            
         MVC   DMCB+12(4),ABOX                                                  
         OC    ABOX,ABOX                                                        
         BZ    *+8                                                              
         MVI   DMCB+12,C'B'                                                     
         GOTO1 REQTWA,DMCB,(3,(RA)),(X'FF',ACOMFACS),VPRINT                     
         SPACE 1                                                                
REP70    GOTO1 TWAPTCHR,DMCB,(RB),(R3)                                          
         SPACE 1                                                                
REP80    L     R2,AFRSTKEY                                                      
         XC    KEY,KEY                                                          
         BAS   RE,CONPRINT                                                      
         SPACE 1                                                                
VEREP    CLI   OFFLINE,C'Y'        ENTER HERE TO JUST WRAP REPORT               
         BE    XIT                                                              
         CLI   PQSW,2              MUST HAVE BEEN OPENED                        
         BNE   REP105                                                           
VEREPB   MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         OC    SPOOLPAG,SPOOLPAG                                                
         BZ    REP105                                                           
         MVI   OKNO,23             REPORT SPOOLED.ID=&1 PGS=&2 LNS=&3           
         MVC   BLOCK(21),SPACES    SET UP GETTXT SUBST PARAM BLOCK              
*                                  L'TXTA+1,TXTA,L'TXTB+1,TXTB ETC ,00          
         MVI   BLOCK,10            XXX,99999 - REP I.D.     PARAM &1            
         MVI   BLOCK+10,5          9999 - PAGE COUNT        PARAM &2            
         MVI   BLOCK+15,6          99999 - LINE COUNT       PARAM &3            
         MVI   BLOCK+21,0          EOT                                          
         MVC   BLOCK+1(3),SPOOLID                                               
         MVI   BLOCK+4,C','                                                     
         EDIT  (2,SPOOLRPN),(5,BLOCK+5),ALIGN=LEFT                              
         EDIT  (2,SPOOLPAG),(4,BLOCK+11),ALIGN=LEFT                             
         EDIT  (2,SPOOLLIN),(5,BLOCK+16),ALIGN=LEFT                             
         LA    R1,GETTXTCB         SET A(SUBST PARAM LIST)                      
         LA    RF,BLOCK                                                         
         STCM  RF,7,GTASUBST-GETTXTD(R1)                                        
*                                                                               
         CLI   ONLYSPUL,C'Y'       SPOOL ONLY SYSTEMS                           
         BNE   REP100                                                           
         CLC   SPOOLRPN,=H'9999'                                                
         BH    REP100                                                           
         L     R1,EFHREC                                                        
         MVC   8(8,R1),BLOCK+1     GET XXX,1234 IN FIRST FIELD                  
         OI    6(R1),X'80'         FOR QUICK $DIP                               
         SPACE 1                                                                
REP100   B     REPEXIT                                                          
         SPACE 1                                                                
REP105   MVI   OKNO,22             NO DATA GENERATED                            
         SPACE 1                                                                
REPEXIT  CLI   ACTNUM,ACTSEL       TEST WE CAME FROM SELECT                     
         BNE   REPEXIT1            NO                                           
         L     R2,EFHACT                                                        
         MVC   8(#SELPLQ,R2),LP@SELP  RESTORE ACTION TO SELECT                  
         MVI   5(R2),6                                                          
         B     XIT                 BACK TO LIST LOGIC                           
         SPACE 1                                                                
REPEXIT1 LA    R1,GETTXTCB         DEFINE MSG NO AND TYPE                       
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO1,OKNO                                                    
         MVI   GTMTYP,GTMINF       INFORMATION                                  
         MVI   GTMSYS,X'FF'        GENERAL MSG SYSTEM                           
         DROP  R1                                                               
         GOTO1 GETTXT              R1=A(CONTROL BLOCK)                          
         B     EXIT                                                             
         SPACE 1                                                                
REPLOAD  NTR1                                                                   
         CLI   TWAFIRST,X'FF'      NOT NEEDED FOR RUNLAST                       
         BE    XIT                                                              
         CLI   PHREPORT,0          LOAD IN REPORT PHASE IF NEEDED               
         BE    REPLOAD2                                                         
         CLC   PHREPORT,PHEDIT     UNLESS IT WAS SAME AS EDIT                   
         BE    REPLOAD2                                                         
         MVC   OVERLAY,PHREPORT                                                 
         GOTO1 LOADEM,DMCB,0                                                    
         ST    R3,AGO                                                           
REPLOAD2 DS    0H                                                               
         ICM   RF,15,TWADCONS                                                   
         BZ    *+10                                                             
         MVC   TAOVER-TWADCOND(,RF),AGO                                         
         B     XIT                                                              
         SPACE 3                                                                
*                                  ENTER HERE TO JUST OPEN REPORT               
         SPACE 2                                                                
VBREP    CLI   PQSW,0              UNLESS USER OPTS TO OPEN PQ                  
         BNE   VBREP1              DO IT NOW                                    
         GOTO1 OPENPQ                                                           
         SPACE 1                                                                
VBREP1   OC    VPRINT,VPRINT                                                    
         BZ    XIT                                                              
         BAS   RE,PRREQDET         TEST PRINTING REQUEST DETAILS                
         BNE   VBREP2                                                           
         MVC   DMCB+12(4),ABOX                                                  
         OC    ABOX,ABOX                                                        
         BZ    *+8                                                              
         MVI   DMCB+12,C'B'                                                     
         GOTO1 REQTWA,DMCB,(3,(RA)),(X'FF',ACOMFACS),VPRINT                     
         SPACE 1                                                                
VBREP2   GOTO1 TWAPTCHR,DMCB,(RB),(R3)                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL PRINTING OF REPORT                                       
         SPACE 2                                                                
CONPRINT NTR1                                                                   
         L     R3,TWADCONS                                                      
         USING TWADCOND,R3                                                      
         LTR   R3,R3                                                            
         BZ    CONP2                                                            
         L     R3,TISPRACT         IF A(PRINT ACTIVE SWITCH) RESOLVED           
         LTR   R3,R3                                                            
         BZ    CONP2                                                            
         MVI   0(R3),C'N'          INITIALIZE IT                                
         SPACE 1                                                                
CONP2    MVI   MODE,PRINTREP                                                    
         BAS   RE,GO               PROCESS REPORT                               
         SPACE 1                                                                
         XC    LASTLIST,LASTLIST                                                
         XC    LASTSELK,LASTSELK                                                
         SPACE 1                                                                
         LTR   R3,R3               TEST A(PRINT ACTIVE SWITCH) RESOLVED         
         BZ    CONPX                                                            
         CLI   0(R3),C'N'          IF NOTHING WAS PRINTED                       
         BNE   CONPX                                                            
         BRAS  RE,EMPTY            PROCESS REPORT WITH NO OUTPUT                
         SPACE 1                                                                
CONPX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DETERMINES IF REQUEST DETAILS SHOULD PRINT               
         SPACE 1                                                                
PRREQDET DS    0H                                                               
         L     R1,TWAMASTC         RETURN R1 = A(MASTER WORK AREA)              
         USING MASTD,R1                                                         
         CLI   MCREQREP,C'N'       TEST SUPPRESSED VIA CONTROL CARD             
         BE    PRDETNO                                                          
         CLI   MCREQREP,C'Y'       IF NOT FORCED VIA CONTROL CARD               
         BE    *+12                                                             
         TM    GENSTAT2,NOREQDET   TEST PROGRAM SWITCH TO SUPPRESS THEM         
         BO    PRDETNO                                                          
         CR    RE,RE               PRINT REQUEST DETAILS - RETURN CC EQ         
         BR    RE                                                               
PRDETNO  LTR   RE,RE               DON'T PRINT - RETURN CC NE                   
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* LOOK UP RECORD AND ACTION DIRECTORY FROM SELECT CODE               *          
*         ENTER WITH R3->START OF TABLE                              *          
*         * NOTE - GENSTAT6 BIT SETTING GESECOVR CONFLICTS WITH *    *          
*         *        SECMASKS AND THEY CANNOT BE USED TOGETHER    *    *          
**********************************************************************          
         SPACE 1                                                                
LLOOKUP  NTR1                                                                   
         LA    R4,1                SET INDICATOR - VALIDATING SEL.              
         LA    R5,3                L'EX COMPARE (TYPE+3 CHR TEXT)               
         TM    GENSTAT3,USEDICT                                                 
         BZ    LOOKUP2                                                          
         BCTR  R5,0                R5=L'EX COMPARE (3 CHR TEXT)                 
         B     LOOKUP2A                                                         
         SPACE 1                                                                
**********************************************************************          
* LOOK UP RECORD AND ACTION DIRECTORY                                *          
*         ENTER WITH R3->START OF TABLE                              *          
*         * NOTE - GENSTAT6 BIT SETTING GESECOVR CONFLICTS WITH *    *          
*         *        SECMASKS AND THEY CANNOT BE USED TOGETHER    *    *          
**********************************************************************          
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         XR    R4,R4                                                            
         GOTO1 ANY                                                              
         CLI   WORK,C'?'           QUESTION MARK = HELP                         
         BNE   LOOKUPB                                                          
         MVC   8(#HELPPLQ,R2),LP@HELPP                                          
         OI    6(R2),X'80'                                                      
         MVI   5(R2),#HELPPLQ                                                   
         MVC   WORK(#HELPPLQ),LP@HELPP                                          
         SPACE 1                                                                
LOOKUPB  MVC   BLOCK(1),DUB                                                     
         MVC   BLOCK+1(8),WORK                                                  
         ZIC   R5,5(R2)                                                         
         LR    R0,R5                                                            
         LA    RE,BLOCK+1                                                       
         CLI   0(RE),C' '          VALIDATE UP TO FIRST BLANK                   
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         SR    R5,R0                                                            
         TM    GENSTAT3,USEDICT    R5=EX'LEN OF KEYWORD IF DICT REF             
         BZ    *+6                                                              
         BCTR  R5,0                                                             
         SPACE 1                                                                
LOOKUP2  TM    GENSTAT3,USEDICT    REC/ACT MAY BE DICTIONARY REFERENCES         
         BNZ   LOOKUP2A                                                         
         EX    R5,*+8                                                           
         BNE   LOOKUP3                                                          
         CLC   0(0,R3),BLOCK       MATCH ON TYPE/DATA                           
         MVC   DUB(8),1(R3)                                                     
         B     LOOKUP4                                                          
*                                                                               
LOOKUP2A CLC   0(1,R3),BLOCK       MATCH ON TYPE FIRST                          
         BNE   LOOKUP3                                                          
         MVC   DUB(8),1(R3)        EXTRACT DICTIONARY REF                       
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
         EX    R5,*+8                                                           
         BE    LOOKUP4                                                          
         CLC   DUB(0),BLOCK+1                                                   
*                                                                               
LOOKUP3  ZIC   R0,LRECACT                                                       
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   LOOKUP2                                                          
         CLI   BLOCK,X'04'         IF LOOKING FOR PROG RECS                     
         BE    NO                  RETURN CC NE                                 
         B     ERRXIT              ELSE THIS IS AN ERROR                        
*                                  TEST SECURITY BLOCK                          
LOOKUP4  GOTO1 =A(TSECBLK),(R3),RR=GENCRELO                                     
         BNE   LOOKUP3                                                          
         CLI   LRECACT,12          IF L'RECACT ENTRY IS GT 12                   
         BNH   LOOKUP5                                                          
         OC    SECMASKS,SECMASKS   AND SECURITY BIT MASK(S) DEFINED             
         BZ    LOOKUP5                                                          
         OC    TALSECMK,TALSECMK                                                
         BZ    LOOKUP4J                                                         
*                                                                               
         MVC   WORK(L'SECMASKS),SECMASKS                                        
         NC    WORK(L'SECMASKS),TALSECMK                                        
         BZ    LOOKUP3                                                          
         B     LOOKUP5                                                          
*                                                                               
LOOKUP4J ICM   RE,15,12(R3)        AND VALID BITS DEFINED                       
*                                  (NOTE THIS ASSUMES L'SECMASKS=4)             
*                                                                               
         BZ    LOOKUP5                                                          
         MVC   WORK(L'SECMASKS),SECMASKS DETERMINE IF VALID FOR MASK            
         NC    WORK(L'SECMASKS),12(R3)                                          
         BZ    LOOKUP3                                                          
         SPACE 1                                                                
LOOKUP5  LTR   R4,R4               IF THIS IS NOT SELECT FIELD                  
         BNZ   LOOKUP6                                                          
         CLC   8(8,R2),DUB         EXPAND FIELD ON SCREEN                       
         BE    LOOKUP6             IF IT'S NOT THERE ALREADY                    
         MVC   8(8,R2),DUB                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         CLI   5(R2),3                                                          
         BNL   *+8                                                              
         MVI   5(R2),3             SET MIN L'INPUT FOR REQTWA                   
         SPACE 1                                                                
LOOKUP6  ST    R3,WORK                                                          
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE X'03' REC/ACT COMBINATIONS                   
         SPACE 1                                                                
*                                  R3 MAY BE PRESET WITH A(START)               
*                                  WORK+2 HAS ACTION EQUATE                     
LKRECACT NTR1                                                                   
         MVI   WORK,X'03'          FINISH BUILDING SEARCH ARGUMENT              
         MVC   WORK+1(1),RECNUM                                                 
         CLI   GCMODE,C'4'         TEST PROG RECS                               
         BNE   LKR1                                                             
         OC    ASECBLK,ASECBLK     IF NEW SECURITY IN USE                       
         BNZ   LKR1                THEN VALIDATE REAL RECORD/ACTION             
         TM    GENSTAT4,LEAVEACT   SHOULD WE CHANGE THE ACTION ?                
         BO    LKR1                YES, LEAVE IT AS IS                          
         CLI   ACTEQU,ACTREST      MAINTENANCE ACTION?                          
         BNH   *+12                                                             
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BNE   LKR1                                                             
         MVI   WORK+2,ACTREP       ACTION IS ALWAYS REPORT                      
         SPACE 1                                                                
LKR1     LTR   R3,R3               TEST R3 PRESET                               
         BNZ   LKR4                BUMP TO NEXT AND KEEP TRYING                 
         SPACE 1                                                                
         ICM   R3,15,ARECACT3      ALTERNATIVE START FOR 03 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         SPACE 1                                                                
*                                                                               
LKR2     CLC   0(3,R3),WORK        NOW LOOK UP X'03' ENTRY                      
         BNE   LKR4                                                             
         ST    R3,WORK+4           SAVE A(ENTRY)                                
         BAS   RE,SUBACT           PASS SECRET SUB-ACTION IF CHA/DEL            
         GOTO1 =A(TSECBLK),(R3),RR=GENCRELO                                     
         BNE   LKR3                                                             
         L     R3,WORK+4           PASS BACK READ REC-ACT ENTRY                 
         CLI   LRECACT,12          IF L'RECACT ENTRY IS GT 12                   
         BNH   LKRX                                                             
         OC    SECMASKS,SECMASKS   AND SECURITY BIT MASK(S) DEFINED             
         BZ    LKRX                                                             
         OC    TALSECMK,TALSECMK                                                
         BZ    LKR2J                                                            
*                                                                               
         MVC   WORK(L'SECMASKS),SECMASKS                                        
         NC    WORK(L'SECMASKS),TALSECMK                                        
         BZ    LKR3                                                             
         B     LKRX                                                             
*                                                                               
LKR2J    ICM   RE,15,12(R3)        AND VALID BITS DEFINED                       
*                                  (NOTE THIS ASSUMES L'SECMASKS=4)             
*                                                                               
         BZ    LKRX                                                             
         MVC   DUB(L'SECMASKS),SECMASKS DETERMINE IF VALID FOR MASK             
         NC    DUB(L'SECMASKS),12(R3)                                           
         BNZ   LKRX                                                             
LKR3     MVI   ERROR,SECLOCK       NO - SECURITY LOCKOUT                        
         B     ERRXIT                                                           
*                                                                               
LKR4     ZIC   R0,LRECACT          BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   LKR2                                                             
         B     NO                  NOT FOUND - RETURN CC NE                     
         SPACE 1                                                                
LKRX     ST    R3,WORK+4           RETURN A(ENTRY)                              
         B     YES                 AND CC EQUAL                                 
         EJECT                                                                  
*                                                                               
SUBACT   DS    0H                                                               
         CLI   ACTEQU,ACTSEL       ACTION SELECT?                               
         BNE   SUBACTX             NO                                           
         MVI   WORK+2,ACTDEL                                                    
         CLI   THISLSEL,C'D'       TRYING TO DELETE FROM A LIST?                
         BE    SUBACT1             NO                                           
         MVI   WORK+2,ACTCHA                                                    
         CLI   THISLSEL,C'C'       TRYING TO CHANGE FROM A LIST?                
         BNE   SUBACTX             NO                                           
*                                                                               
SUBACT1  ICM   R3,15,ARECACT3      ALTERNATIVE START FOR 03 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
*                                                                               
SUBACT2  CLC   0(3,R3),WORK        NOW LOOK UP X'03' ENTRY                      
         BE    SUBACTX                                                          
         ZIC   R0,LRECACT          BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   SUBACT2                                                          
         B     NO                  NOT FOUND - RETURN CC NE                     
*                                                                               
SUBACTX  BR    RE                                                               
*                                  CHECK FOR HELP EXIT                          
         SPACE 1                                                                
ANYHELP  TM    1(R2),X'02'         NO HELP FOR EXTENDED HEADERS                 
         BNZR  RE                                                               
         CLI   8(R2),C'?'          QUESTION MARK=HELP                           
         BE    ANYHELP2                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LP@HELPP    DOES FIELD CONTAIN HELP                      
         BNER  RE                                                               
         SPACE 1                                                                
ANYHELP2 MVI   ERROR,NOHELP        YES - IS HELP AVAILABLE                      
         CLI   PHHELP,0                  NO - TOUGH                             
         BE    ERRXIT                                                           
         MVC   8(#HELPPLQ,R2),LP@HELPP                                          
         OI    6(R2),X'80'                                                      
         MVC   OVERLAY,PHHELP                                                   
         L     R3,EFHTAG                                                        
         GOTO1 LOADEM,DMCB,1       LOAD IN HELP SCREEN                          
         BAS   RE,TRANSBTS                                                      
         MVC   TWASCR,PHHELP                                                    
         MVI   OKNO,1                                                           
         B     OKEX                                                             
         SPACE 3                                                                
SWTOCON  NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   SYSDIR,=CL8'CTFILE'                                              
         MVC   SYSFIL,=CL8'CTFILE'                                              
         MVC   DATADISP,=H'28'                                                  
         MVC   LKEY,=H'25'                                                      
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     XIT                                                              
         SPACE 3                                                                
READUP   DS    0H                                                               
         TM    GENSTAT1,RDUPAPPL   IF APPLIC. CONTROLLING READ UPDATES          
         BZ    *+8                                                              
         MVI   RDUPDATE,C'Y'       INSURE IT'S ALWAYS SET HERE                  
         BR    RE                                                               
         EJECT                                                                  
*              UTILITY ROUTINES                                                 
         SPACE 1                                                                
LOADEM   NTR1                                                                   
LOADEM0  MVC   BYTE,3(R1)          DON'T USE APPLICATION CALLS                  
         CLI   BYTE,1                  FOR SCREENS (1)                          
         BE    LOADEM1                                                          
         L     R3,AOVERLAY                                                      
         SR    R0,R0               R0=0 IF REGULAR LOAD                         
         C     R3,SYSDUMMY                                                      
         BNE   *+8                                                              
         LHI   R0,1                R0=1 IF LOADING INTO SYSDUMMY                
         TM    GENSTAT1,APPLIC          USE APPLICATION CALLS?                  
         BO    LOADEMC                                                          
         SPACE 1                                                                
LOADEM1  MVC   DMCB+4(4),SYSPHASE       NORMAL CALL TO LOADEM                   
         ST    R3,DMCB                                                          
         SPACE 1                                                                
         CLI   BYTE,1              IF WE'RE LOADING A SCREEN                    
         BNE   LOADEMA                                                          
         LM    R4,R5,X'E00'(RA)    SAVE 1ST 8 BYTES OF MY SAVED STORAGE         
         CLI   ALTPROG,0           IF ALTERNATE PROGRAM DEFINED                 
         BE    LOADEMA                                                          
         MVC   DMCB+6(1),ALTPROG   USE IT                                       
         SPACE 1                                                                
LOADEMA  GOTO1 CALLOV,DMCB,,,0                                                  
         OC    DMCB+9(3),DMCB+9                                                 
         BZ    LOADEM2                  PHASE NOT FOUND                         
         SPACE 1                                                                
         CLI   BYTE,1              IF THIS ISN'T A SCREEN                       
         BNE   LOADEMB                 GO UPDATE AOVERLAY                       
         SPACE 1                                                                
         STM   R4,R5,X'E00'(RA)    RESTORE 1ST 8 BYTES OF SAVED STORAGE         
         LA    RF,X'E07'(RA)       RF=A(END OF USABLE TWA0 FOR SCREEN)          
         LA    R3,0(R3)                                                         
         C     R3,ATWA             IF A(LOAD) IS ON/AFTER A(TWA)                
         BL    XIT                                                              
         CR    R3,RF               AND BEFORE END OF USABLE AREA                
         BH    XIT                                                              
         AH    R3,DMCB+10          THEN INSURE NOT LOADED PAST END              
         CR    R3,RF                                                            
         BNH   XIT                                                              
         DC    H'0'                SCREEN EXCEEDS END OF USABLE AREA            
         SPACE 1                                                                
LOADEMB  L     R1,DMCB                                                          
         CLI   OFFLINE,C'Y'        ALLOW LOAD ANYWHERE                          
         BNE   *+10                IF RUNNING OFFLINE                           
         LR    R3,R1                                                            
         B     LOADEMBA                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         LR    R3,R1                                                            
         CR    R1,R3                                                            
         BE    *+6                                                              
         DC    H'0'             LOADS OUT OF PHASE???                           
         CLC   DMCB+10(2),=H'0'                                                 
         BNE   *+6                                                              
         DC    H'0'             ZERO LENGTH PHASE???                            
         AH    R1,DMCB+10                                                       
         LA    R1,8(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
LOADEMBA ST    R1,AOVERLAY         GET A(NEXT DOUBLE WORD)                      
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   LOADEMX                                                          
         LTR   R0,R0               AND LOADING TO SYSDUMMY                      
         BZ    LOADEMX                                                          
         ST    R3,SYSDUMMY         SET SYSDUMMY TO ACTUAL LOAD ADDRESS          
         B     LOADEMX                                                          
         SPACE 1                                                                
LOADEMC  XC    DMCB(12),DMCB       APPLICATION CALLS                            
         MVC   DMCB(1),OVERLAY                                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        CHECK ERROR,                                 
         BE    LOADEM2                 PHASE NOT FOUND                          
         L     R3,DMCB             RETURN A(PHASE)                              
LOADEMX  XIT1  REGS=(R3)                                                        
         ORG   *-2                                                              
         BR    RE                                                               
         SPACE 1                                                                
LOADEM2  LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,NOPHASE    CANT FIND OVERLAY                            
         MVI   GTMSYS,X'FF'        GENERAL SYSTEM MESSAGE                       
         MVI   GTLTXT,6            L'PHASE NAME                                 
         LA    R3,BLOCK            BUILD PHASE NAME TO APPEND                   
         STCM  R3,7,GTATXT                                                      
         DROP  R1                                                               
         MVC   WORK(4),SYSPHASE                                                 
         CLI   BYTE,1              IF WE'RE LOADING A SCREEN                    
         BNE   LOADEM4                                                          
         CLI   ALTPROG,0           AND ALTERNATE PROGRAM DEFINED                
         BE    LOADEM4                                                          
         MVC   WORK+2(1),ALTPROG   USE IT                                       
LOADEM4  GOTO1 HEXOUT,DMCB,WORK+1,(R3),3,0                                      
         MVI   0(R3),C'T'                                                       
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
         SPACE 1                                                                
VLOAD    EQU   LOADEM0                                                          
         EJECT                                                                  
*              PASS CONTROL TO OVERLAY                                          
         SPACE 2                                                                
GO       NTR1                                                                   
         CLI   GCMODE,C'4'         HANDLE PROG RECS INTERNALLY                  
         BNE   GO1                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    GO1                                                              
         CLI   ACTNUM,ACTSEL       TEST REPORT WAS SELECTED                     
         BNE   *+12                                                             
         CLI   THISLSEL,REPSELQ                                                 
         BE    GO1                                                              
*                                                                               
         GOTO1 GOPROGS,DMCB,(RC)                                                
         MVC   USEIO,SVUSEIO                                                    
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI                                                
         MVC   LKEY,SVLKEY                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),SVSYS                                                    
         MVC   FILENAME,SVFILENM                                                
         GOTO1 SWITCH,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,GOGO             OFF TO APPLICATION                           
*                                                                               
         MVC   SVFILENM,FILENAME                                                
         XC    FILENAME,FILENAME                                                
         BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
GO1      BAS   RE,GOGO             OFF TO APPLICATION                           
*                                                                               
         B     XIT                                                              
*                                                                               
GOGO     NTR1                                                                   
         L     RF,AGO                                                           
         XR    RB,RB                                                            
         BCTR  RB,0                HIGH VALS IN GENCON'S BASE REGISTERS         
         LR    R7,RB                                                            
         LR    R9,RB                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO HANDLE TEMPSTR MAINTENANCE                           
         SPACE 1                                                                
SAVESTOR NTR1                                                                   
SAVESTO0 MVC   TWANSAVE,NTWA       SAVE UP TO 3 AREAS                           
         SPACE 1                                                                
         BAS   RE,ANYTWA0          ANYTHING TO SAVE AT BOTTOM OF TWA0           
         BZ    *+6                 RETURNS REGS R0,R1,R2,R3                     
         MVCL  R0,R2               SAVE IT                                      
         SPACE 1                                                                
         CLI   TWANSAVE,0          ANY ADDITIONAL TEMPSTR PAGES TO SAVE         
         BE    SAVESTX                                                          
         MVC   COMMAND(6),=C'DMWRT '                                            
         BAS   RE,SETP6            SET P6 OF DMGR CALL IN TEMPIO                
         BAS   RE,SAVEREST                                                      
SAVESTX  B     XIT                                                              
         SPACE 1                                                                
VSAVE    EQU   SAVESTO0                                                         
         SPACE 3                                                                
RESTSTOR NTR1                                                                   
         BAS   RE,ANYTWA0          TEST ANYTHING TO RESTORE FROM TWA0           
         BZ    *+6                 RETURNS REGS R0,R1,R2,R3                     
         MVCL  R2,R0               RESTORE IT                                   
         SPACE 1                                                                
         CLI   TWANSAVE,0                                                       
         BE    RESTSTX                                                          
         MVC   NTWA,TWANSAVE       RESTORE UP TO 3 AREAS                        
         MVC   COMMAND(6),=C'DMREAD'                                            
         BAS   RE,SETP6            SET P6 OF DMGR CALL IN TEMPIO                
         BAS   RE,SAVEREST                                                      
         L     RE,AENDSV           OPTIONAL END OF SAVE STORAGE                 
         LTR   RE,RE                                                            
         BZ    RESTSTX                                                          
         LA    RF,2304             CLEAR A BIT FROM HERE                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
RESTSTX  B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR SAVED STORAGE MAINTENANCE                
         SPACE 2                                                                
ANYTWA0  DS    0H                                                               
         OC    LSVTWA0,LSVTWA0     ANYTHING TO SAVE AT BOTTOM OF TWA0           
         BZR   RE                                                               
         LH    R1,LSVTWA0                                                       
         CH    R1,=AL2(TWA018K)    MAX WE CAN KEEP THERE                        
         BNH   *+6                                                              
         DC    H'0'                YOU NEED TO ASK FOR LESS                     
         LH    R0,=AL2(18432-2048) TWA SIZE IS 18K, SAVE 2K FOR CHKPT           
         SR    R0,R1               SET TO START AT BOTTOM OF TWA0               
         A     R0,ATWA                                                          
         L     R2,ASTARTSV         A(SAVED STORAGE)                             
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         L     R2,ASYSD                                                         
         LR    R3,R1               SET L'MOVE IN BOTH ODD REGISTERS             
         LTR   RE,RE                                                            
         BR    RE                  OK - RETURN CC NZ                            
         SPACE 3                                                                
SETP6    DS    0H                                                               
         MVC   DMCB+20(2),=C'L='   SET P6 OF DMGR CALL IN TEMPIO                
         MVC   DMCB+22(2),=X'0900' TO 2304 (OLD)                                
*                                                                               
         TM    NTWA,NTWA06K+NTWA14K+NTWA18K                                     
         BZ    SETP6X                                                           
*                                                                               
         TM    NTWA,NTWA06K        OPTION TO RESTORE 06K AREA                   
         BZ    SETP6A                                                           
         MVC   DMCB+22(2),=X'1800' ASSUME 6K AREA                               
         LA    RF,NTWA14K+NTWA18K  ENSURE 14/18 NOT ALSO SET                    
         B     SETP6C                                                           
*                                                                               
SETP6A   TM    NTWA,NTWA14K        OPTION TO RESTORE 14K AREA                   
         BZ    SETP6B                                                           
         MVC   DMCB+22(2),=X'3800'                                              
         LA    RF,NTWA06K+NTWA18K  ENSURE 06/18 NOT ALSO SET                    
         B     SETP6C                                                           
*                                                                               
SETP6B   MVC   DMCB+22(2),=X'4800' OPTION TO RESTORE 18K AREA                   
         LA    RF,NTWA06K+NTWA14K  ENSURE 06/14 NOT ALSO SET                    
*                                                                               
SETP6C   EX    RF,*+10                                                          
         BZ    SETP6X                                                           
         DC    H'0'                MIX OF 6/14/18 K REQUESTED                   
         TM    NTWA,0                                                           
*                                                                               
SETP6X   BR    RE                                                               
         EJECT                                                                  
*              ROUTINES TO CONTROL SAVE/RESTORE OF LIST SCREENS                 
         SPACE 2                                                                
SAVELIST NTR1                      SAVE A SCREEN                                
         GOTO1 SWITCH,DMCB,X'FFFFFFFF',0                                        
         L     R1,0(R1)                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(LSTSCMAP-TWATASK)                                          
         MVC   0(L'LSTSCMAP,RF),TSCRNE-UTLD(R1)                                 
         MVI   LSTONTWA,C'Y'                                                    
         MVC   COMMAND(6),=C'DMWRT '                                            
         B     SCRN2                                                            
         SPACE 3                                                                
RESTLIST NTR1                      RESTORE A SCREEN                             
         CLI   LSTONTWA,C'Y'       IS THERE ONE TO RESTORE                      
         BNE   XIT                                                              
*&&US*&& MVC   COMMAND(6),=C'DMRDIR'                                            
*&&UK*&& MVC   COMMAND(6),=C'DMREAD'                                            
         SPACE 1                                                                
         XC    DMCB+20(4),DMCB+20                                               
         TM    GENSTAT3,RESTXE00   RESTORE X'E00' TO TWA0                       
         BZ    SCRN2                                                            
         MVC   DMCB+20(2),=C'L='   SET P6 OF DMGR CALL IN TEMPIO                
         MVC   DMCB+22(2),=X'0E00'                                              
         MVC   COMMAND(6),=C'DMREAD'                                            
         SPACE 1                                                                
SCRN2    L     R2,ATWA                                                          
         LA    R3,4                                                             
         BAS   RE,TEMPIO                                                        
         XC    DMCB+20(4),DMCB+20  INSURE P6 IS CLEARED                         
         SPACE 1                                                                
         GOTO1 SWITCH,DMCB,X'FFFFFFFF',0                                        
         L     R1,0(R1)                                                         
         GOTO1 PROTOFF                                                          
         LR    RF,RA                                                            
         AH    RF,=Y(LSTSCMAP-TWATASK)                                          
         MVC   TSCRNE-UTLD(L'LSTSCMAP,R1),0(RF)                                 
         GOTO1 PROTON                                                           
         CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
         EJECT                                                                  
*              TEMPSTR I/O                                                      
         SPACE 1                                                                
SAVEREST NTR1                                                                   
         ZIC   R0,TWANSAVE                                                      
         SLL   R0,27               CRUNCH OFF X'E0' BITS                        
         SRL   R0,27                                                            
         LTR   R0,R0               ANY TO SAVE/RESTORE                          
         BZ    SRX                                                              
         CLI   OFFLINE,C'Y'        NOT APPLICABLE-OFF LINE                      
         BE    SRX                                                              
         CHI   R0,3                MAX 3                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R3,1                TWA NUMBER                                   
         L     R2,ASYSD            START OF STORAGE                             
         NC    ASTARTSV,ASTARTSV                                                
         BZ    *+8                                                              
         L     R2,ASTARTSV         USE OPTIONAL AREA IF SET                     
         AH    R2,LSVTWA0          SKIP ANYTHING SAVED IN TWA0                  
         SPACE 1                                                                
SR2      BAS   RE,TEMPIO                                                        
         AH    R2,DMCB+22          LENGTH IS IN P6+2 OF TEMPSTR CALL            
         LA    R3,1(R3)                                                         
         BCT   R0,SR2                                                           
         SPACE 1                                                                
SRX      XC    DMCB+20(4),DMCB+20                                               
         B     XIT                                                              
         SPACE 1                                                                
*                                  P6 OF DMCB MAY BE SET BY CALLING RTN         
TEMPIO   NTR1                                                                   
         L     RA,ATWA                                                          
         STC   R3,DMCB+8           PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              SET UP TWA ADDRESSES                                             
         SPACE 1                                                                
SETTWADD NTR1                                                                   
         XC    AFRSTREC,AFRSTREC                                                
         XC    AFRSTKEY,AFRSTKEY                                                
         L     R2,EFHTAG                                                        
         SR    R1,R1                                                            
         SPACE 1                                                                
SETT2    BAS   RE,BUMP                                                          
         BE    SETT8                                                            
         TM    1(R2),X'20'                                                      
         BO    SETT2                                                            
         ST    R2,AFRSTKEY                                                      
         ST    R2,AFRSTREC                                                      
         ST    R2,AFRSTRCH                                                      
         SPACE 1                                                                
SETT4    BAS   RE,BUMP             NOW LOOK FOR A                               
         BE    SETT8                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BNO   SETT4                                                            
         CLI   0(R2),9             1 BYTE FIELD                                 
         BNE   SETT4                                                            
         SPACE 1                                                                
SETT6    BAS   RE,BUMP                                                          
         BE    SETT8                                                            
         TM    1(R2),X'20'                                                      
         BNO   *+12                                                             
         ST    R2,AFRSTRCH         NOTE HEADER BEFOR FIRST RECORD               
         B     SETT6                                                            
         ST    R2,AFRSTREC         AND FIRST RECORD INPUT                       
         SPACE 1                                                                
SETT8    B     XIT                                                              
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
         SPACE 1                                                                
TRANSBTS NTR1                                                                   
         LA    R2,CONHEADH                                                      
         SPACE 1                                                                
TRANS2   OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
         BAS   RE,BUMP                                                          
         BNE   TRANS2                                                           
         MVC   1(2,R2),=X'0101'    POP IN INDICATORS                            
         B     XIT                                                              
         EJECT                                                                  
YES      SR    R0,R0               THEN THERE IS KEY DATA INPUT                 
         J     *+8                                                              
NO       LA    R0,1                                                             
         LTR   R0,R0                                                            
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK LIST SCREEN FOR CHANGES                         
         SPACE 1                                                                
ANYCHGS  NTR1                                                                   
         TM    GLSTSTAT,CHNGLIST   TEST CHANGES ALLOWED ON LIST SCREEN          
         BZ    NO                  DON'T BOTHER IF NOT                          
         SPACE 1                                                                
         ZIC   R0,LISTNUM          R0=N'LISTED RECORDS                          
         TM    GLSTSTAT,OKTOADD    IF OK TO ADD NEW RECORDS FROM LIST           
         BZ    *+8                                                              
         IC    R0,NLISTS           CHECK ENTIRE SCREEN                          
         LTR   R0,R0                                                            
         BZ    NO                                                               
         L     R2,AFRSTREC         R2=A(1ST DATA LINE)                          
         SPACE 1                                                                
ANYC10   BAS   RE,TESTSEL          UNLESS THERE'S NO SELECT FIELD               
         BNE   *+12                                                             
         BAS   RE,BUMP             BUMP PAST IT TO 1ST FIELD                    
         BE    NO                                                               
         LR    R3,R2                                                            
         AH    R3,LLIST            R3=A(1ST FIELD AFTER THIS DATA LINE)         
         SPACE 1                                                                
ANYC20   TM    1(R2),X'20'         IGNORE PROTECTED FIELDS                      
         BO    ANYC30                                                           
         TM    4(R2),X'20'         LOOK FOR FIELDS NOT PREV. VALIDATED          
         BZ    YES                                                              
         SPACE 1                                                                
ANYC30   BAS   RE,BUMP             BUMP TO NEXT FIELD IN RECORD                 
         BE    NO                                                               
         CR    R2,R3               TEST PAST END                                
         BL    ANYC20              NO                                           
         B     ANYC10              YES - BUMP TO NEXT RECORD                    
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO HANDLE VALIDATION OF LISTED RECORDS                   
***********************************************************************         
VALLIST  NTR1                                                                   
         ZIC   R0,LISTNUM          R0=N'LISTED RECORDS                          
         TM    GLSTSTAT,OKTOADD    IF OK TO ADD NEW RECORDS FROM LIST           
         BZ    *+8                                                              
         IC    R0,NLISTS           CHECK ENTIRE SCREEN                          
         LA    R3,LISTDIR          R3=A(DISK ADDRESS TABLE)                     
         L     R2,AFRSTREC         R2=A(1ST DATA LINE)                          
*                                                                               
VALL10   BAS   RE,TESTSEL          UNLESS THERE'S NO SELECT FIELD               
         BNE   *+12                                                             
         BAS   RE,BUMP             BUMP PAST IT TO 1ST FIELD                    
         BE    NO                                                               
         ST    R2,ATHISLST         SAVE A(THIS LIST LINE) FOR APPLIC            
         LR    R4,R2                                                            
         AH    R4,LLIST            R4=A(1ST FIELD AFTER THIS DATA LINE)         
         MVI   BYTE,0              CLEAR LINE STATUS BYTE                       
*                                                                               
VALL20   TM    1(R2),X'20'         IGNORE PROTECTED FIELDS                      
         BO    VALL30                                                           
         TM    4(R2),X'20'         LOOK FOR FIELDS NOT PREV. VALIDATED          
         BO    VALL21                                                           
         OI    BYTE,X'80'          SET FOUND UN-VALIDATED FIELD                 
*****                                                                           
         TM    GLSTSTAT,EMPTYLIN   NEED TO LOOK FOR FIELDS WITH INPUT?          
         BNZ   VALL23              NO, WE HAVE A CHANGED LINE                   
*****                                                                           
VALL21   CLI   5(R2),0             ALSO LOOK FOR FIELDS WITH INPUT              
         BE    *+8                                                              
         OI    BYTE,X'40'          SET FOUND A FIELD WITH INPUT                 
         TM    BYTE,X'C0'          IF WE'VE HAVEN'T SEEN BOTH YET               
         BNO   VALL30              KEEP ON LOOKING                              
*                                                                               
VALL23   ZIC   R1,LISTNUM          RECORD CHANGED - SET RELATIVE NUMBER         
         TM    GLSTSTAT,OKTOADD                                                 
         BZ    *+8                                                              
         IC    R1,NLISTS                                                        
         SR    R1,R0                                                            
         CLM   R1,1,LISTNUM        IF RELATIVE NUMBER >= LISTNUM                
         BNL   VALL25              THEN THIS MUST BE ADD                        
*                                                                               
         STC   R1,SELLISTN         SAVE RELATIVE NUMBER                         
         MVI   0(R3),CHASELQ       MAKE IT APPEAR SELECTED FOR CHANGE           
         BAS   RE,SELREAD          READ RECORD                                  
         MVI   0(R3),0                                                          
         MVI   MODE,LVALREC        SET VALIDATE LISTED RECORD MODE              
         BAS   RE,CHANGE2          HANDLE CHANGES                               
         MVI   SELLISTN,0                                                       
         B     VALL28                                                           
                                                                                
VALL25   MVI   MODE,LVALKEY        SET VALIDATE KEY FROM LIST MODE              
         BAS   RE,GO               LET APPLICATION VALIDATE/BUILD KEY           
         BAS   RE,ADDVKEY          VALIDATE KEY FOR ADD                         
         BNE   ERRXIT              RETURNS CC NE IF THERE'S A PROBLEM           
         MVI   MODE,LVALREC        SET VALIDATE RECORD FROM LIST MODE           
         BAS   RE,ADDIT            HANDLE ADD                                   
         GOTO1 LISTMON             RECOGNIZE NEW ITEM IN LIST                   
                                                                                
VALL28   LR    R2,R4               BUMP TO NEXT RECORD                          
         B     VALL40                                                           
                                                                                
VALL30   BAS   RE,BUMP             BUMP TO NEXT FIELD IN RECORD                 
         BE    NO                                                               
         CR    R2,R4               TEST PAST END                                
         BL    VALL20              NO                                           
                                                                                
VALL40   LA    R3,6(R3)            YES - BUMP TO ENTRY IN D/A TABLE             
         BCT   R0,VALL10           BUMP TO NEXT RECORD                          
         B     XIT                                                              
         EJECT                                                                  
*              EXITS                                                            
MODEX    L     R1,EFHACT                                                        
         TM    1(R1),X'20'                                                      
         BO    *+8                 DON'T MODIFY IF PROTECTED                    
         OI    6(R1),X'80'+X'01'   MODIFY AND TRANSMIT                          
                                                                                
OKEX     TM    GENSTAT2,USMYOK+USGETTXT      TEST IF APPL SET GETTXTCB          
         BO    OKEX1                                                            
         TM    GENSTAT2,USMYOK               TEST IF APPL SET OWN MSG           
         BO    OKEX3                                                            
         XC    PARAS(L'GTBLOCK),PARAS        DEFINE GETTXT PARAMS               
         LA    R1,PARAS                                                         
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO1,OKNO       MESSAGE NUMBER (1 BYTE)                      
         MVI   GTMSYS,X'FF'        SYSTEM ZERO (GENERAL) MESSAGES               
         B     OKEX2                                                            
*                                                                               
OKEX1    LA    R1,GETTXTCB         USER HAS DEFINED CONTROL BLOCK               
         CLI   GTMTYP,0            CHECK THAT MESSAGE TYPE DEFINED              
         BNE    *+8                                                             
*                                                                               
OKEX2    MVI   GTMTYP,GTMINF       SET INFORMATION MESSAGE                      
         DROP  R1                                                               
                                                                                
         GOTO1 GETTXT              R1=A(PARAS)-GENCON. A(GETTXTCB)-APPL         
         B     OKEX4                                                            
                                                                                
OKEX3    TM    GENSTAT2,STLCOK     APPL SET MSG - MAY WANT LOWER CASE           
         BZ    OKEX4                                                            
         L     R1,=A(UPRLWR)                                                    
         A     R1,GENCRELO                                                      
         TR    CONHEAD+1(59),0(R1)                                              
OKEX4    NI    GENSTAT2,255-USMYOK-STLCOK-USGETTXT                              
         CLI   PQSW,2              IF PQ HAS BEEN OPENED                        
         BE    VEREPB                  CLOSE IT                                 
*                                                                               
EXIT     CLI   GCMODE,C'S'         EXIT HERE IF SLAVED MODE                     
         BE    XIT                                                              
         MVI   GENSTAT2,0                                                       
         ICM   R0,15,ACURFORC      FORCE CURSOR TO THIS ADDRESS?                
         BZ    *+6                                                              
         LR    R2,R0                                                            
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         L     R2,EFHREC                                                        
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    EXIT10                                                           
         LA    R2,0(,R2)           CLEAR HOB                                    
         GOTO1 GETFACT,DMCB,(X'80',0),F#CKITSK,(R2)   VALIDATE ADDRESS          
         BE    *+6                                                              
         DC    H'00'               I DON'T TRUST R2                             
*                                                                               
* THE NEXT INSTRUCTION CAUSES CRAPPERS IF R2 IS BAD                             
*                                                                               
EXIT10   OI    6(R2),X'40'         INSERT CURSOR                                
         SR    R2,RA                 AND SAVE LOCATION                          
         STH   R2,TWADISP                                                       
         L     R2,AERRAREA                                                      
         MVC   TWALACT,ACTNUM                                                   
         MVC   TWALREC,RECNUM                                                   
         OI    6(R2),X'80'         TRANSMIT HEADER                              
*                                                                               
         TM    GENSTAT4,SVTWA0FF   SAVE TWA0 ON 'FF' SCREEN?                    
         BO    SVSTOR              YES, PER MEL                                 
         CLI   TWASCR,X'FF'        IF REQUEST DISPLAY NOT ACTIVE                
         BE    *+8                                                              
SVSTOR   BAS   RE,SAVESTOR         SAVE SAVED STORAGE                           
*                                                                               
         TM    GENSTAT7,GES7BACK   TEST TO RETURN TO APP                        
         BO    *+8                                                              
         L     RD,SYSRD                                                         
*                                                                               
XIT      XIT1  ,                                                                
         ORG   *-2                                                              
         BR    RE                                                               
                                                                                
ERRINV   MVI   ERROR,INVALID                                                    
ERRXIT   GOTO1 ERREX                                                            
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
LARF00   LA    RF,0(0)                                                          
         SPACE 2                                                                
SELTAB   DS    0XL(SELTABLQ)       SELECT CODE ACTION EQUIVALENCE TABLE         
         DC    S(LP@CHAS),S(LP@CHA) CHANGE                                      
         DC    S(LP@SELS),S(LP@DIS) DISPLAY                                     
         DC    S(LP@DELS),S(LP@DEL) DELETE                                      
         DC    S(LP@RESS),S(LP@RES) RESTORE                                     
         DC    S(LP@REPS),S(LP@REP) REPORT                                      
         DC    X'FFFF'                                                          
         SPACE 2                                                                
LISTLIST DS    0XL(LISTLLQ)                                                     
         DC    S(LP@FRSTP),C'F',X'0'                                            
         DC    S(LP@LASTP),C'L',X'0'                                            
         DC    S(LP@NEXTP),C'N',X'0'                                            
         DC    S(LP@THISP),C'T',X'0'                                            
         DC    S(LP@XP),C'T',X'0'                                               
         DC    S(LP@EXITP),C'T',X'0'                                            
         DC    X'FFFF'                                                          
         SPACE 2                                                                
* VALID PRINT OPTIONS -  EXTRA FOR PROGRAM RECS                                 
*                                                                               
         DS    0S                  HW ALIGN                                     
PRGOPTS  DS    0XL(POPTLQ)                                                      
         DC    S(LP@NO),XL1'00',XL1'00'                                         
         DC    AL4(VOUT),AL4(0)                                                 
         DC    S(LP@NEXT),XL1'00',XL1'00'                                       
         DC    AL4(VOUT),AL4(0)                                                 
         SPACE 1                                                                
* VALID PRINT OPTIONS                                                           
*                                                                               
PRTOPTS  DS    0XL(POPTLQ)                                                      
         DC    S(LP@NOW),XL1'40',XL1'00'                                        
         DC    AL4(VPREMU),AL4(0)                                               
*&&UK                                                                           
         DC    S(LP@SOON),XL1'20',XL1'02'                                       
         DC    AL4(VPREMU),AL4(0)                                               
         DC    S(LP@ASAP),XL1'20',XL1'02'                                       
         DC    AL4(VPREMU),AL4(0)                                               
*&&                                                                             
*&&US                                                                           
         DC    S(LP@SOON),XL1'20',XL1'02'                                       
         DC    AL4(VPREMU),AL4(VPOX)                                            
         DC    S(LP@ASAP),XL1'20',XL1'02'                                       
         DC    AL4(VPREMU),AL4(VPOX)                                            
*&&                                                                             
         DC    S(LP@OV),XL1'10',XL1'04'                                         
         DC    AL4(VPREMU),AL4(VPOX)                                            
         DC    S(LP@ONT),XL1'10',XL1'04'                                        
         DC    AL4(VPREMU),AL4(VPOX)                                            
         DC    S(LP@DDS),XL1'08',XL1'04'                                        
         DC    AL4(VPREMU),AL4(VPOX)                                            
         DC    XL1'FF'                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST SECURTIY ACCESS BLOCK                               *         
*                                                                     *         
* NTRY - R1=A(RECACT ENTRY)                                           *         
* EXIT - CC=EQUAL IF AUTHORIZED, NOT EQUAL IF NOT AUTHORIZED          *         
***********************************************************************         
TSECBLK  NTR1  BASE=*,LABEL=*                                                   
         OC    ASECBLK,ASECBLK     TEST SECURITY BLOCK DEFINED                  
         JZ    YES                                                              
         CLI   OFFLINE,C'Y'        SUPPRESS TESTING OFFLINE                     
         JE    YES                                                              
*                                                                               
         CLI   0(R1),X'03'         TEST SECURITY ON RECORD/ACTION PAIR          
         BNE   TBLK02                                                           
         LA    RF,2(R1)                                                         
         ICM   RF,8,1(R1)          RF=(RECORD, A(ACTION))                       
         LA    R4,SECPRACT         R4=ACTION (TESTING RECORD/ACTION)            
         TM    GENSTAT6,GESECOVR   ARE WE USING A RECORD OVERRIDE?              
         BNO   TBLK10              (GESECOVR CONFLICTS WITH SECMASKS)           
         CLI   12(R1),0            ANY OVERRIDE PROVIDED?                       
         BE    TBLK10              NO OVERRIDE-GO AS IS                         
         ICM   RF,8,12(R1)         STICK IN OVERRIDE INSTEAD OF RECORD          
         B     TBLK10                                                           
*                                                                               
*                                  ELSE MUST BE RECORD OR ACTION                
TBLK02   LA    RF,9(R1)            RF=A(RECORD OR ACTION)                       
         TM    GENSTAT6,GESECOVR   ARE WE USING A RECORD OVERRIDE?              
         BNO   *+16                (GESECOVR CONFLICTS WITH SECMASKS)           
         CLI   12(R1),0            ANY OVERRIDE PROVIDED?                       
         BE    *+8                 NO OVERRIDE-GO AS IS                         
         LA    RF,12(R1)           POINT TO OVERRIDE INSTEAD OF RECORD          
*                                                                               
         LA    R4,SECPRCD          R4=ACTION (TESTING RECORD)                   
         CLI   0(R1),X'02'         IF TESTING ACTION                            
         BNE   *+8                                                              
         LA    R4,SECPACT          R4=ACTION (TESTING ACTION)                   
*                                                                               
TBLK10   TM    GENSTAT7,GES7DDS    TEST FOR DDS ONLY RECORDS                    
         BZ    TBLK20                                                           
         CLI   0(R1),X'02'         DON'T BOTHER WITH ACTION                     
         BE    TBLK20              NO DDS ONLY ACTIONS                          
         CLI   LRECACT,12          MUST BE MORE THAN 12                         
         BNH   TBLK20                                                           
         LA    R2,12(R1)           DEFAULT IS +12                               
         CLI   LDDSDISP,0                                                       
         BE    TBLK12                                                           
         LA    R2,0(R1)                                                         
         LLC   R1,LDDSDISP                                                      
         AR    R2,R1                                                            
*                                                                               
TBLK12   TM    0(R2),X'80'         DDS BIT +12 IN TABLE                         
         BZ    TBLK20                                                           
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         JE    YES                                                              
*                                                                               
TBLK20   GOTO1 SECRET,DMCB,((R4),ASECBLK),(RF)                                  
*                                                                               
         J     XIT                 SECRET RETURNS CC                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROCESS A REPORT WITH NO OUTPUT                       
         SPACE 1                                                                
EMPTY    NTR1  BASE=*,LABEL=*                                                   
*&&US                                                                           
EMPTY1   CLI   TWAWHEN,5           ALL SOONS MUST HAVE SOME OUTPUT              
         BE    EMPTY1X                                                          
         CLI   TWAWHEN,2                                                        
         BE    EMPTY1X                                                          
         BAS   RE,PRREQDET         PRINT REQUEST DETAILS?                       
         BNE   EMPTYX              NO                                           
EMPTY1X  DS    0H                                                               
*&&                                                                             
EMPTY3   L     R1,TWAMASTC         R1 = A(MASTER WORK AREA)                     
         USING MASTD,R1                                                         
         TM    MCPRTIND,MCPRTXFI   TEST EMPTY XFILE REQUEST                     
         BZ    EMPTY4                                                           
         MVC   P,SPACES            SET ONE PAGE ONLY REQUEST DETAILS            
         GOTO1 TWAVPRNT,DUB,P,=C'BC01'                                          
         B     EMPTYX                                                           
         SPACE 1                                                                
*                                                                               
EMPTY4   TM    WHEN,X'20'          TEST SOON                                    
         BZ    EMPTY6              NO                                           
         TM    GENSTAT7,GES7PDF    TEST USER NEED PDF HOOK                      
         BZ    EMPTY6                                                           
         MVI   MODE,EMPTYHK        YES - DO ERRHOOK CALL NOW                    
         BAS   RE,GO                                                            
         B     EMPTYX              AND THAT'S ALL WE DO FOR HIM!                
*                                                                               
EMPTY6   MVC   P,SPACES            SET TO DISPLAY NOTHING TO REPORT MSG         
         GOTO1 TWAVPRNT,DUB,P,=C'BL01'                                          
         BASR  RE,RF                                                            
         MVCDD P+3(#DSK02LQ),GE#DSK02 JOB RAN TO NORMAL COMPLETION              
         BASR  RE,RF                                                            
         MVCDD P+3(#DSK01LQ),GE#DSK01 THERE WAS NOTHING ACTIVE TO REP           
         BASR  RE,RF                                                            
*                                                                               
EMPTYX   XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK SCREEN FOR SELECTIONS                           
         SPACE 1                                                                
ANYSELS  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R0,LISTNUM                                                       
         LTR   R0,R0               DON'T BOTHER IF NO LIST                      
         BZ    NO                                                               
         L     R2,AFRSTREC         FIRST SIMPLY VALIDATE SELECT CODES           
         TM    GENSTAT5,SEL1BYTE   TEST APPL SUPPORTS 1 BYTE SELECT             
         BNZ   AS2                                                              
         CLI   0(R2),9             1 BYTE SELECT FIELDS ARE PHONEY              
         BE    NO                                                               
         TM    GLSTSTAT,NOSELFLD   DON'T BOTHER IF NO SELECT FIELD              
         BO    NO                                                               
         SPACE 1                                                                
AS2      TM    1(R2),X'20'         IGNORE PROTECTED FIELDS                      
         BO    AS4                                                              
         TM    1(R2),X'0C'         AND ZERO INTENSITY FIELDS                    
         BO    AS4                                                              
         CLI   5(R2),0             WAS ANYTHING INPUT                           
         BE    AS4                                                              
         LA    R4,SELTAB           LOOK UP SELECT CODE IN TABLE                 
         USING SELTABD,R4                                                       
         MVC   DUB(2),LARF00       LA  RF,0(0)                                  
*                                                                               
AS3      CLI   0(R4),X'FF'                                                      
         BE    ERRINV              NOT A VALID SELECT CODE                      
         MVC   DUB+2(2),SELSCHR    GENERATE BASE DISP FOR LA RF,0(0)            
         EX    0,DUB               RF=A(SINGLE CHR SELECT CODE)                 
         CLC   8(1,R2),0(RF)                                                    
         BE    *+12                                                             
         LA    R4,SELTABLQ(R4)                                                  
         B     AS3                                                              
         SPACE 1                                                                
         CLC   8(1,R2),LP@CHAS     IF SELECTING FOR CHANGE                      
         BNE   *+12                                                             
         TM    GENSTAT5,NOCHGLST   AND IF CHANGE FROM LIST NOT ALLOWED          
         BO    ERRINV              THEN GIVE ERROR                              
         SPACE 1                                                                
         CLC   8(1,R2),LP@DELS     IF SELECTING FOR DELETE                      
         BNE   AS3B                                                             
         TM    GENSTAT4,NODELLST   AND IF DELETE FROM LIST NOT ALLOWED          
         BO    ERRINV              THEN GIVE ERROR                              
         TM    GENSTAT5,NODLST     OR IF 'D' FROM LIST NOT ALLOWED              
         BZ    AS3B                ONLY 'DE'/'DEL' ALLOWED                      
         CLC   8(2,R2),LP@DEL                                                   
         BE    AS3B                                                             
         CLC   8(3,R2),LP@DEL                                                   
         BNE   ERRINV              THEN GIVE ERROR                              
         SPACE 1                                                                
AS3B     TM    GENSTAT3,OKVALSEL   TEST OK TO VALIDATE SELECT CODES             
         BZ    AS4                                                              
         MVI   BLOCK,X'02'         LOOK UP X'02' (ACTION) ENTRY                 
         MVC   DUB+2(2),SELSNAM    GENERATE BASE DISP FOR LA RF,0(0)            
         EX    0,DUB               RF=A(SELECT ACTION NAME)                     
         MVC   BLOCK+1(3),0(RF)    USE CORRESPONDING ACTION FROM TABLE          
*                                                                               
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   AS3D                                                             
*                                                                               
         CLI   8(R2),C'R'          IF SELECT CODE IS 'R' (IE RESTORE)           
         BNE   AS3D                                                             
         CLI   GCMODE,C'4'         AND THIS IS PROG RECS                        
         BNE   AS3D                                                             
         MVC   BLOCK+1(#REPLQ),LP@REP   FORCE TO ACTION REPORT                  
         DROP  R4                                                               
         SPACE 1                                                                
AS3D     ICM   R3,15,ARECACT2      ALTERNATIVE START FOR 02 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         MVI   ERROR,INVACT                                                     
         BAS   RE,LLOOKUP          LOOK UP ACTION                               
         SPACE 1                                                                
         L     R3,WORK             RETURNS A(ACTION ENTRY) IN WORK              
         MVC   WORK+2(1),10(R3)    NOW HAVE ACTION EQUATE                       
         XR    R3,R3                                                            
         BAS   RE,LKRECACT         LOOKUP RECORD/ACTION ENTRY                   
         BE    AS4                                                              
         MVI   ERROR,INVRCACT      ERROR IF RETURNS CC NE                       
         B     ERRXIT                                                           
         SPACE 1                                                                
AS4      BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         AH    R2,LLIST            USE L'LINE IF DEFINED TO GET TO NEXT         
         CLI   0(R2),0                                                          
         BNE   AS2                                                              
         SPACE 1                                                                
         L     R2,AFRSTREC         ALL OK - NOW DO IT FOR REAL                  
         LA    R3,LISTDIR                                                       
         SR    R5,R5               COUNT SELECTIONS                             
AS6      TM    1(R2),X'20'                                                      
         BO    AS10                                                             
         TM    1(R2),X'0C'                                                      
         BO    AS10                                                             
         CLI   5(R2),0                                                          
         BE    AS8                                                              
         MVC   0(2,R3),8(R2)       SAVE ACTION AND ARGUMENT                     
         LA    R5,1(R5)            ADD TO SELECTIONS                            
*                                  CLEAR FIELD                                  
         ZIC   R1,0(R2)            CALCULATE L'FIELD                            
         AHI   R1,-9                                                            
         CLI   1(R2),X'FF'         NOP FIELD - CAN'T HAVE EXTENDED HDR          
         BE    *+16                                                             
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)       TEST IF ANYTHING IN FIELD                    
         BZ    AS8                 DON'T BOTHER IF NOTHING THERE                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'         TRANSMIT IT                                  
         MVI   5(R2),0             AND SIMULATE NO INPUT LENGTH                 
         SPACE 1                                                                
AS8      LA    R3,6(R3)                                                         
         BCT   R0,AS10                                                          
         LTR   R5,R5                                                            
         BZ    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
AS10     BAS   RE,BUMP                                                          
         AH    R2,LLIST            USE L'LINE IF DEFINED TO GET TO NEXT         
         CLI   0(R2),0                                                          
         BNE   AS6                                                              
         B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SEE IF THERE WAS INPUT IN A KEY FIELD                 
         SPACE 1                                                                
ANYKEY   NTR1  BASE=*,LABEL=*                                                   
*&&UK                                                                           
         L     R1,EFHOTH                                                        
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         TM    4(R1),X'80'         WAS 'OTHER' DATA ENTERED                     
         BO    YES2                YES - TREAT AS KEY DATA INPUT                
*&&                                                                             
         L     R2,ATWA                                                          
         LA    R2,64(R2)                                                        
         SPACE 1                                                                
AK2      LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    NO2                                                              
         TM    1(R2),X'20'         LOOK FOR UNPROTECTED FIELD                   
         BO    AK2                                                              
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         BZ    *+12                                                             
         TM    1(R2),X'02'                                                      
         BZ    AK2                                                              
*                                                                               
         TM    4(R2),X'80'         THAT WAS INPUT THIS TIME                     
         BNO   AK2                                                              
         C     R2,AFRSTKEY         DOES IF FALL BETWEEN FIRST KEY               
         BL    AK2                                                              
*                                                                               
         TM    GENSTAT4,USEAFRCH   USE AFRSTRCH NOT AFRSTREC                    
         BNO   AK4                                                              
         C     R2,AFRSTRCH         AND FIRST FIELD AFTER KEY                    
         BNL   NO2                                                              
         B     YES2                                                             
*                                                                               
AK4      C     R2,AFRSTREC         AND FIRST RECORD FIELD                       
         BNL   NO2                                                              
*                                                                               
YES2     SR    R0,R0               THEN THERE IS KEY DATA INPUT                 
         J     *+8                                                              
NO2      LA    R0,1                                                             
         LTR   R0,R0                                                            
AKXIT    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES ENTERABLE FROM BASE OR OVERLAY                          
         SPACE 1                                                                
         DS    CL((4*4096)-(*-T00A30))  FORCE OFFSET TO A NICE NUMBER           
         SPACE 1                                                                
         USING *,RF                                                             
VCOMMON  NTR1  BASE=BASERB                                                      
         LA    R7,2048(RB)         SECOND BASE                                  
         LA    R7,2048(R7)                                                      
         LA    R9,2048(R7)         THIRD BASE                                   
         LA    R9,2048(R9)                                                      
         LR    RB,RF               FIRST BASE OVERRIDE                          
         DROP  RF                                                               
         USING VCOMMON,RB                                                       
*                                                                               
         L     R8,ASPOOLD          RESTORE OTHER REGISTERS                      
         L     RA,ATWA             USERS MAY HAVE CHANGED                       
*                                                                               
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 2                                                                
VBRANCH  B     VANY                                                             
         B     VHIGH                                                            
         B     VSEQ                                                             
         B     VREAD                                                            
         B     VWRITE                                                           
         B     VADD                                                             
         B     VGETREC                                                          
         B     VPUTREC                                                          
         B     VADDREC                                                          
         B     VVALDATE                                                         
         B     VVALPER                                                          
         B     VVALNUM                                                          
         B     VVALDEC                                                          
         B     VERREX                                                           
         B     VERREX2                                                          
         B     VADDELEM                                                         
         B     VREMELEM                                                         
         B     VLISTMON                                                         
         B     VDSPSEC                                                          
         B     VVALSEC                                                          
         B     VTSTSEC                                                          
         B     VOPNPQ                                                           
         B     VLOAD                                                            
         B     VSAVE                                                            
         B     VREQ                                                             
         B     VBREP                                                            
         B     VEREP                                                            
         B     VCATCHIO                                                         
         SPACE 2                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
VBRANCH2 B     VRFP                                                             
         B     XIT                 SPARE ROUTINES                               
         B     XIT                                                              
         B     XIT                                                              
         B     XIT                                                              
         B     XIT                                                              
VCOUNT2  EQU   (*-VBRANCH2)/4                                                   
         EJECT                                                                  
*              ENTERABLE ROUTINES - ANY                                         
         SPACE 1                                                                
VANY     CLI   5(R2),0                                                          
         BNE   VANY2                                                            
         MVI   ERROR,MISSING                                                    
         B     VERREX                                                           
         SPACE 2                                                                
VANY2    MVC   WORK,SPACES                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*              I/O HANDLING ROUTINES - DIRECTORY                                
         SPACE 1                                                                
VHIGH    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    VDIR                                                             
         MVC   KEYSAVE,BIGKEY                                                   
         B     VDIR                                                             
         SPACE 1                                                                
VSEQ     MVC   COMMAND,=C'DMRSEQ'                                               
         B     VDIR                                                             
         SPACE 1                                                                
VREAD    MVC   COMMAND(6),=C'DMREAD'                                            
         MVC   KEYSAVE,KEY                                                      
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    VDIR                                                             
         MVC   KEYSAVE,BIGKEY                                                   
         B     VDIR                                                             
         SPACE 1                                                                
VWRITE   MVC   COMMAND(6),=C'DMWRT '                                            
         L     R1,AIO                                                           
         CLI   USEIO,C'Y'                                                       
         BNE   *+12                                                             
         GOTOR CHAACTIV                                                         
         B     VWR10                                                            
         LH    RE,LKEY                                                          
         BCTR  RE,0                                                             
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BO    VWR05                                                            
         EX    RE,MVCKEY                                                        
         B     VWR10                                                            
VWR05    EX    RE,MVCBKEY                                                       
VWR10    CLI   TWAWRITE,C'N'                                                    
         BE    XIT                                                              
         B     VDIR                                                             
MVCKEY   MVC   0(0,R1),KEY                                                      
MVCBKEY  MVC   0(0,R1),BIGKEY                                                   
         SPACE 1                                                                
VADD     MVC   COMMAND,=C'DMADD '                                               
         CLI   USEIO,C'Y'                                                       
         BNE   *+8                                                              
         GOTOR ADDACTIV                                                         
         CLI   TWAWRITE,C'N'                                                    
         BE    XIT                                                              
         SPACE 1                                                                
VDIR     MVC   DMFILE,FILENAME                                                  
         CLI   DMFILE,0            IF USER DID NOT PROVIDE FILE NAME            
         BNE   *+10                                                             
         MVC   DMFILE,SYSDIR       USE SYSTEM DIRECTORY NAME                    
         LA    R3,KEY              READ INTO KEY                                
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R3,BIGKEY                                                        
         CLI   USEIO,C'Y'          OR, OPTIONALLY I/O AREA                      
         BNE   *+8                                                              
         L     R3,AIO                                                           
VDIR2    BAS   RE,SETRDUP          SET READ FOR UPDATE                          
*                                                                               
*INTEREP GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,(R3),0                 
         SPACE                                                                  
         LA    RE,KEY              DEFAULT DATAMGR P3                           
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    RE,BIGKEY                                                        
         TM    GENSTAT3,USEKEYSV                                                
         BZ    VDIR20                                                           
*                                                                               
         MVC   KEYSAVE,0(RE)       PASS CURRENT KEY                             
         LA    RE,KEYSAVE          NEW DATAMGR P3                               
*                                                                               
VDIR20   ST    RE,DMCB+8           DATAMGR P3.                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,,(R3),0                    
         SPACE                                                                  
         CLI   USEIO,C'Y'                                                       
         BNE   DMCHECK                                                          
         L     R1,AIO                                                           
         MVC   KEY,0(R1)                                                        
         B     DMCHECK                                                          
         EJECT                                                                  
*              I/O HANDLING ROUTINES - FILE                                     
         SPACE 1                                                                
VGETREC  MVC   COMMAND(6),=C'GETREC'                                            
         LA    R3,KEY                                                           
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R3,BIGKEY                                                        
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS          R3=A(D/A)                                    
         B     ALLREC                                                           
         SPACE 1                                                                
VPUTREC  MVC   COMMAND(6),=C'PUTREC'                                            
         GOTOR CHAACTIV            UPDATE ACTIVITY                              
         CLI   TWAWRITE,C'N'                                                    
         BE    XIT                                                              
         LA    R3,DMWORK+4         R3=A(D/A)                                    
         B     ALLREC                                                           
         SPACE 1                                                                
VADDREC  MVC   COMMAND(6),=C'ADDREC'                                            
         GOTOR ADDACTIV            ADD AN ACTIVITY ELEMENT                      
         CLI   TWAWRITE,C'N'                                                    
         BE    XIT                                                              
         LA    R3,KEY              R3=A(WHERE DM WILL RETURN D/A)               
         SPACE 1                                                                
ALLREC   MVC   DMFILE,FILENAME                                                  
         CLI   DMFILE,0            IF USER DID NOT PROVIDE FILENAME             
         BNE   *+10                                                             
         MVC   DMFILE,SYSFIL       USE STANDARD SYSTEM FILE NAME                
         BAS   RE,SETRDUP          SET READ FOR UPDATE                          
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),                         X        
               DMFILE,(R3),AIO,DMWORK                                           
         MVC   DMDSKADD,0(R3)                                                   
         SPACE 1                                                                
DMCHECK  MVI   RDUPDATE,C'Y'       RESET READ FOR UPDATE                        
         OI    DMINBTS,X'80'                                                    
         TM    GENSTAT1,RDUPAPPL                                                
         BZ    *+12                                                             
         MVI   RDUPDATE,C'N'       UNLESS CONTROLLED BY APPLICATION             
         NI    DMINBTS,X'7F'                                                    
         SPACE 1                                                                
         TM    GENSTAT4,NODUPDIE   DON'T DIE ON DUPLICATE KEY ON ADD            
         BO    DMCHK20                                                          
         CLC   =C'ADDREC',COMMAND  ON ALL UPDATIVE DATAMGR CALLS,               
         BE    DMCHK10                                                          
         CLC   =C'PUTREC',COMMAND                                               
         BE    DMCHK10                                                          
         CLC   =C'DMADD',COMMAND                                                
         BE    DMCHK10                                                          
         CLC   =C'DMWRT',COMMAND                                                
         BNE   *+14                                                             
DMCHK10  CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON NON-ZERO RETURN CODE                  
         SPACE 1                                                                
DMCHK20  MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         BZ    XIT                                                              
         MVI   ERROR,0                                                          
         B     VERREX                                                           
         SPACE 1                                                                
SETRDUP  CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BE    *+12                                                             
         CLI   RDUPDATE,C'N'       OR READ FOR UPDATE IS OFF                    
         BNE   *+8                                                              
         NI    DMINBTS,X'7F'       TURN IT OFF                                  
         TM    GENSTAT1,RDUPAPPL   IF APPLIC. CONTROLLING READ UPDATES          
         BZR   RE                                                               
         CLI   RDUPDATE,C'Y'       AND READ FOR UPDATE IS REQUESTED             
         BNER  RE                                                               
         OI    DMINBTS,X'80'       DO IT                                        
         BR    RE                                                               
         EJECT                                                                  
*              VALIDATE DATE, PERIOD                                            
         SPACE 1                                                                
VVALDATE L     R3,0(R1)                                                         
         GOTO1 ANY                                                              
         CLC   8(#TODAYLQ,R2),LP@TODAY    KEYWORD OF TODAY ALLOWED              
         BNE   VVALD2                                                           
         GOTO1 DATCON,DMCB,(5,0),(0,0(R3))                                      
         MVI   ACTUAL,5                                                         
         B     XIT                                                              
         SPACE 1                                                                
VVALD2   GOTO1 DATVAL,DMCB,(0,8(R2)),(R3)                                       
         MVC   ACTUAL,DMCB+3                                                    
         CLI   ACTUAL,0                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
*                                  YEAR IS OPTIONAL                             
         GOTO1 DATCON,DMCB,(5,0),(0,0(R3))                                      
         LH    R0,0(R3)            SAVE THIS YEAR                               
         GOTO1 DATVAL,DMCB,(1,8(R2)),(R3)                                       
         STH   R0,0(R3)            AND INSERT INTO OUTPUT                       
         MVC   ACTUAL,DMCB+3                                                    
         CLI   ACTUAL,0                                                         
         BNE   XIT                                                              
VVALDERR MVI   ERROR,INVDATE                                                    
         B     VERREX                                                           
         SPACE 1                                                                
*&&US                                                                           
VVALPER  DC    H'0'                NO LONGER VALID: DEIS OCT13/98               
*                                                                               
         L     R3,0(R1)            PERIOD                                       
         ZIC   R5,0(R1)            POSSIBLE MONTH YEAR OPTION                   
         LA    R4,8(R2)                                                         
         GOTO1 DATVAL,DMCB,((R5),(R4)),(R3)                                     
         ZIC   R0,DMCB+3                                                        
         LTR   R0,R0                                                            
         BZ    VVALDERR                                                         
         AR    R4,R0                                                            
         MVC   6(6,R3),0(R3)       ASSUME END = START                           
         CLI   0(R4),C' '                                                       
         BE    XIT                                                              
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),C'-'          - OR , ACCEPTABLE DELIMITERS                 
         BE    *+12                                                             
         CLI   0(R4),C','                                                       
         BNE   VVALDERR                                                         
         LA    R4,1(R4)                                                         
         LA    R3,6(R3)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),(R3)                                       
         CLI   DMCB+3,0                                                         
         BE    VVALDERR                                                         
         AHI   R3,-6                                                            
         CLC   0(6,R3),6(R3)                                                    
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     VERREX                                                           
*&&                                                                             
         EJECT                                                                  
*&&UK                                                                           
*        THIS ROUTINE VALIDATES A DATE PAIR AND RETURNS A 12 BYTE               
*        FIELD CONTAINING THE TWO DATES IN YYMMDD EBCDIC FORM. IF ONLY          
*        ONE DATE ENTERED THIS WILL APPEAR IN BOTH SIDES                        
*              DDMMMYY-DDMMMYY      DDMMM-DDMMMYY                               
*              MMMYY-MMMYY          MMM-MMMYY                                   
*              DDMMM-DDMMM          MMMYY                                       
*              DDMMM                                                            
*              MMM-MMM              MMM                                         
*        EITHER THE TO OR FROM DATE MAY BE OMITTED BY ENTERING THE DASH         
*        TO INDICATE AN UPTO OR FROM. ANY OF THE ABOVE FORMATS IS VALID         
*        E.G.  DDMMMYY-             -DDMMMYY                                    
*              MMMYY-               -MMMYY    ETC.                              
*        ON ENTRY R2 ADDRESSES THE DATE FLD HDR AND R1 POINTS TO THE            
*        ADDRESS OF THE 12 BYTE OUTPUT AREA.                                    
*                                                                               
VVALPER  L     R3,0(R1)            0(R1)=A(OUTPUT AREA)                         
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    VERREX                                                           
         ZIC   R1,BTODAY                                                        
         CVD   R1,DUB                                                           
         UNPK  HALF,DUB+6(2)                                                    
         OI    HALF+1,C'0'         SAVE CURRENT YR IN HALF                      
         XC    BLOCK(32*3),BLOCK                                                
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=-='                              
         MVI   ERROR,INVDATE                                                    
         LA    R4,BLOCK                                                         
         CLI   4(R1),0                                                          
         BE    VERREX                                                           
         CLI   4(R1),2                                                          
         BH    VERREX                                                           
         BNE   VVALP10                                                          
         ZIC   RF,32(R4)           CHECK 2ND DATE FOR YEAR INPUT                
         LA    RF,32+10(R4,RF)                                                  
         TM    0(RF),X'F0'         LAST 2 CHARS NUMERIC                         
         BNO   VVALP10                                                          
         TM    1(RF),X'F0'                                                      
         BNO   VVALP10                                                          
         MVC   HALF,0(RF)          IF FOUND OVERWRITE CURRENT YEAR              
*                                                                               
VVALP10  NC    0(2,R4),0(R4)       ANY DATA LEFT OF -                           
         BNZ   VVALP15             YES - VALIDATE IT                            
         MVC   4(6,R4),=C'000000'                                               
         NC    32(2,R4),32(R4)                                                  
         BZ    VERREX              MUST BE A RIGHT IF NO LEFT                   
         B     VVALP25                                                          
VVALP15  BAS   RE,DVAL                                                          
         BNE   VERREX                                                           
         TM    2(R4),2             DAY NOT GIVEN                                
         BNO   VVALP20                                                          
         MVC   8(2,R4),=C'01'      SET TO 1ST OF MONTH                          
*                                                                               
VVALP20  NC    32(2,R4),32(R4)     NO 2ND DATE                                  
         BNZ   VVALP25                                                          
         ZIC   R1,0(R4)                                                         
         LA    R1,8(R2,R1)                                                      
         CLI   0(R1),C'-'          SEE IF IT WAS DDMMMYY-                       
         BNE   VVALP22                                                          
         MVC   32+12(7,R4),=C'31DEC99'                                          
         MVI   32(R4),7            IF SO SET MAX DATE                           
         B     VVALP25                                                          
VVALP22  MVC   32(32,R4),0(R4)     OTHERWISE PROPAGATE 1ST ENTRY                
         TM    2(R4),2             IF NONE AND 1ST DATE INCLUDED DAY            
         BNO   VVALP30             THEN LEAVE START AND END SAME                
*                                                                               
VVALP25  LA    R4,32(R4)           VALIDATE 2ND DATE                            
         BAS   RE,DVAL                                                          
         BNE   VERREX                                                           
         TM    2(R4),2             DAY NOT GIVEN                                
         BNO   VVALP30             SET TO LAST OF MONTH                         
         MVC   8(2,R4),=C'01'                                                   
         GOTO1 ADDAY,DMCB,4(R4),32(R4),F'35'                                    
         MVC   36(2,R4),=C'01'                                                  
         GOTO1 (RF),(R1),32(R4),4(R4),X'FFFFFFFF'                               
*                                                                               
VVALP30  MVI   ERROR,INVEBFRS      CHECK FOR START GTR THAN END                 
         LA    R4,BLOCK                                                         
         CLC   4(6,R4),32+4(R4)                                                 
         BH    VERREX                                                           
         MVC   0(6,R3),4(R4)       MOVE TO OUTPUT                               
         MVC   6(6,R3),32+4(R4)                                                 
         B     XIT                                                              
*                                                                               
*              SUBROUTINE TO VALIDATE A SCANNER ENTRY AT R2                     
*                                                                               
DVAL     NTR1                                                                   
         LR    R2,R4                                                            
         CLI   1(R4),0                                                          
         BNE   DVALERR             '=' PRESENT                                  
         XC    1(11,R2),1(R2)                                                   
         SR    R0,R0                                                            
         CLC   12(#TODAYLQ,R2),LP@TODAY                                         
         BNE   DVAL0                                                            
         GOTO1 DATCON,DMCB,(3,BTODAY),4(R2)                                     
         B     DVAL2                                                            
DVAL0    LA    R4,1                CHECK FOR MY,DM AND DMY                      
         LA    R5,2                                                             
DVAL1    GOTO1 DATVAL,DMCB,((R0),12(R2)),4(R2)                                  
         OC    DMCB,DMCB                                                        
         BNZ   DVAL2                                                            
         BXLE  R0,R4,DVAL1                                                      
         CLI   0(R2),3             CHECK FOR JUST MONTH                         
         BNE   DVALERR                                                          
         MVC   15(2,R2),HALF                                                    
         GOTO1 (RF),(R1),(2,12(R2)),4(R2)                                       
         OC    DMCB,DMCB                                                        
         BZ    DVALERR                                                          
         LA    R0,2                                                             
DVAL2    STC   R0,2(R2)            SAVE INDICATORS                              
         TM    2(R2),1             IF YEAR MISSING SET TO CURRENT OR            
         BNO   *+10                YEAR OF 2ND DATE                             
         MVC   4(2,R2),HALF                                                     
         CR    RB,RB                                                            
         B     XIT                                                              
DVALERR  LTR   RB,RB                                                            
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              VALIDATE NUMERIC                                                 
         SPACE 1                                                                
VVALNUM  GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BNE   VALNM2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    VALNM2                                                           
         CHI   R1,255                                                           
         BH    VALNM2                                                           
         STC   R1,ACTUAL                                                        
         B     XIT                                                              
         SPACE 2                                                                
VVALDEC  GOTO1 ANY                 VALIDATE DECIMAL                             
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(MAX,8(R2)),(R3)                                    
         MVC   FULL,DMCB+4                                                      
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         SPACE 2                                                                
VALNM2   MVI   ERROR,NOTNUM                                                     
         B     VERREX                                                           
         EJECT                                                                  
*              ELEMENT MANIPULATION                                             
         SPACE 1                                                                
VADDELEM CLI   ELEMENT,0                                                        
         BE    XIT                                                              
         SR    RF,RF                                                            
         TM    GENSTAT5,ADDEQCOD                                                
         BZ    *+8                                                              
         LA    RF,=C'ADD=CODE'                                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,(RF)                        
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         MVI   ERROR,TOOLONG       DID RECORD GET TOO LONG                      
         CLI   DMCB+12,5                                                        
         BE    VERREX                                                           
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
         SPACE 1                                                                
VREMELEM L     R6,AIO                                                           
         XC    ELEMENT,ELEMENT                                                  
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),0                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MONITOR LIST ACTIVITY                                 
         SPACE 1                                                                
VLISTMON TM    WHEN,X'80'          ONLY OF INTEREST FOR IMMEDIATE               
         BNO   YES                                                              
         CLC   LISTNUM,NLISTS      IF MAX HAVE ALREADY BEEN INPUT               
         BL    VLM2                                                             
         MVI   OKNO,15             SET MESSAGE ASSUMING NO SELECT FIELD         
         BAS   RE,TESTSEL          IF THIS IS SELECT FIELD                      
         BNE   *+8                                                              
         MVI   OKNO,9              SET MESSAGE WITH SELECT PROMPT               
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,32             GIVE SPECIAL MESSAGE                         
         B     VLMX                                                             
         SPACE 1                                                                
VLM2     ZIC   R1,LISTNUM          UPDATE NUMBER IN LIST                        
         LA    R1,1(R1)                                                         
         STC   R1,LISTNUM                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'6'                                                         
         LA    R1,LISTDIR(R1)                                                   
         XC    0(6,R1),0(R1)                                                    
         CLI   USEIO,C'Y'                                                       
         BE    VLM4                                                             
         MVC   2(4,R1),DMDSKADD    SAVE DISK ADDRESS                            
         MVC   LASTLIST,DMDSKADD                                                
         OC    PAGEDAS+60(4),PAGEDAS+60                                         
         BNZ   *+10                                                             
         MVC   PAGEDAS+60(4),DMDSKADD                                           
         OC    VERYFRST,VERYFRST                                                
         BNZ   *+10                                                             
         MVC   VERYFRST,DMDSKADD                                                
         B     VLM6                                                             
         SPACE 1                                                                
VLM4     LA    RF,LISTKEYS         FOR IS FILES SAVE KEY                        
         ZIC   RE,LISTNUM                                                       
         BCTR  RE,0                                                             
         MH    RE,LKEY                                                          
         AR    RF,RE                                                            
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),KEY                                                      
         MVC   LASTSELK,KEY                                                     
         SPACE 1                                                                
VLM6     L     R2,ATHISLST                                                      
         TM    GLSTSTAT,APPLCDSP   TEST APPLIC ALREADY DISPLAYED DATA           
         BO    VLM8                                                             
         ZIC   R1,0(R2)            MOVE LISTAR INTO SCREEN                      
         AHI   R1,-9                                                            
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),LISTAR      IF IT HAS CHANGED                            
         BE    VLM8                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
         OI    6(R2),X'80'         AND TRANSMIT                                 
         SPACE 1                                                                
VLM8     OC    LLIST,LLIST         TEST HAVE L'LIST LINE                        
         BZ    *+12                                                             
         AH    R2,LLIST            USE IT TO BUMP TO NEXT                       
         B     *+8                                                              
         BAS   RE,BUMP             ELSE SIMPLY BUMP TO NEXT FIELD               
         SPACE 1                                                                
         MVI   OKNO,15             SET MESSAGE ASSUMING NO SELECT FIELD         
         BAS   RE,TESTSEL          IF THIS IS SELECT FIELD                      
         BNE   *+12                                                             
         MVI   OKNO,9              SET MESSAGE WITH SELECT PROMPT               
         BAS   RE,BUMP             AND BUMP PAST IT                             
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,32             GIVE SPECIAL MESSAGE                         
         SPACE 1                                                                
         ST    R2,ATHISLST         SAVE A(NEXT LIST LINE)                       
         CLC   LISTNUM,NLISTS      IF HAVEN'T DISPLAYED ALL YET                 
         BL    YES                 RETURN TO CALLER WITH CC EQ                  
         TM    GLSTSTAT,RETEXTRA   IF APPLIC WANTS EXTRA CALL                   
         BO    NO                  RETURN TO CALLER WITH CC NE                  
         SPACE 1                                                                
VLMX     L     R2,AFRSTREC         SET TO PLACE CURSOR AT FIRST FIELD           
         B     MODEX               AND EXIT TO USER                             
         EJECT                                                                  
*              SECURITY VALIDATE                                                
         SPACE 1                                                                
VVALSEC  MVI   ELCODE,X'F3'                                                     
         GOTO1 REMELEM                                                          
         CLI   5(R2),0             DEFAULT IS NONE                              
         BE    XIT                                                              
         XC    ELEM,ELEM                                                        
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),0                                    
         LA    R6,ELEM                                                          
         USING SECURED,R6                                                       
         MVI   ERROR,INVALID                                                    
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    VERREX                                                           
         SPACE 1                                                                
VSEC2    CLC   12(#IDLQ,R3),LP@ID      STAMP WITH ID                            
         BNE   VSEC4                                                            
         MVC   SECURID,TWAORIG                                                  
         B     VSECEND                                                          
         SPACE 1                                                                
VSEC4    CLC   12(#MELQ,R3),LP@ME  PASSWORD PROTECTION                          
         BE    VSEC6                                                            
         CLC   12(#PASSLQ,R3),LP@PASS                                           
         BE    VSEC6                                                            
         OC    4(4,R3),4(R3)       ELSE NUMERIC LEVEL                           
         BZ    VERREX                                                           
         MVC   SECURLEV,7(R3)                                                   
         B     VSECEND                                                          
         SPACE 1                                                                
VSEC6    GOTO1 GETFACT,DMCB,0      FROM GETFACT                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SECURPAS,FAPASSWD   PICK UP PASSWORD                             
         MVC   SECURFLG,FATFLAG            AND FLAG                             
         SPACE 1                                                                
VSECEND  LA    R3,32(R3)                                                        
         BCT   R4,VSEC2                                                         
         MVI   SECUREL,X'F3'                                                    
         MVI   SECURLEN,12                                                      
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*              SECURITY DISPLAY AND TEST                                        
         SPACE 1                                                                
VDSPSEC  MVC   BLOCK(60),SPACES    DISPLAY                                      
         L     R6,AIO                                                           
         XR    R4,R4               NUMBER OF FIELDS                             
         MVI   ELCODE,X'F3'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSECEND                                                          
         USING SECURED,R6                                                       
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
         OC    SECURID,SECURID     ID                                           
         BZ    *+18                                                             
         MVC   0(#IDLQ,R3),LP@ID                                                
         LA    R3,20(R3)                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
         OC    SECURPAS,SECURPAS   PASSWORD                                     
         BZ    *+18                                                             
         MVC   0(#PASSWLQ,R3),LP@PASSW                                          
         LA    R3,20(R3)                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
         CLI   SECURLEV,0          LEVEL                                        
         BZ    DSECEND                                                          
         EDIT  (1,SECURLEV),(3,(R3)),ALIGN=LEFT                                 
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
DSECEND  GOTO1 UNSCAN,DMCB,((R4),BLOCK),(R2),0                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         SPACE 1                                                                
VTSTSEC  L     R6,AIO              TEST SECURITY                                
         MVI   ELCODE,X'F3'                                                     
         BAS   RE,GETEL                                                         
         BNE   TSECOK                                                           
         USING SECURED,R6                                                       
         OC    SECURID,SECURID     ID STAMPED                                   
         BZ    TSEC2                                                            
         CLC   SECURID,TWAORIG                                                  
         BNE   TSECNOT                                                          
         SPACE 1                                                                
TSEC2    OC    SECURPAS,SECURPAS   PASSWORD STAMPED                             
         BZ    TSEC4                                                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLC   SECURPAS,FAPASSWD                                                
         BNE   TSECNOT                                                          
         SPACE 1                                                                
TSEC4    CLI   SECURLEV,0                                                       
         BE    TSECOK                                                           
         CLC   SECURLEV,TWAACCS+2                                               
         BL    TSECNOT                                                          
         CLC   SECURLEV,TWAACCS+3                                               
         BH    TSECNOT                                                          
         SPACE 1                                                                
TSECOK   CR    R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
TSECNOT  MVI   ERROR,SECLOCK                                                    
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE SPOOL AND PRINT Q                                     
         SPACE 1                                                                
VOPNPQ   MVI   PQSW,2                                                           
         CLI   REMUSER+2,C' '      PAD USER                                     
         BNE   *+8                                                              
         MVI   REMUSER+2,C'*'                                                   
         CLI   REMUSER+1,C' '                                                   
         BNE   *+8                                                              
         MVI   REMUSER+1,C'*'                                                   
         CLI   REMUSER,C' '                                                     
         BNE   *+8                                                              
         MVI   REMUSER,C'*'                                                     
         MVC   SPOOLID,REMUSER                                                  
*                                                                               
         TM    GENSTAT3,NOCLRSPK   USER HAS INITIALIZED SPOOLKEY?               
         BO    *+10                                                             
         XC    SPOOLKEY,SPOOLKEY                                                
         LA    R2,SPOOLKEY         KEY FOR PRINT Q                              
         USING PQPLD,R2                                                         
*NOP*    CLC   PLDESC,SPACES       DO NOT ALLOW 'NO-NAME' TO GO IN KEY          
*NOP*    BH    *+16                                                             
*NOP*    MVC   PLDESC,SPACES                                                    
*NOP*    MVC   PLDESC(4),RCPROG    DDSPOOL SETS DEFAULT PLDESC NOW              
*&&US                                                                           
         CLC   TWAOUT(3),=C'MLR'   SPECIAL FOR MAILERS                          
         BNE   *+10                                                             
         MVC   PLDESC(6),=C'MAILER'                                             
         CLC   PLDESC(4),=C'TRSB'  SPECIAL FOR LABELS                           
         BNE   *+12                                                             
         MVI   PLLPP,X'80'         TO SUPPRESS PAGING                           
         OI    SPOOLIND,X'40'      AND INDICATE PARMS PRESENT                   
*&&                                                                             
         MVC   USERLANG,LANG                                                    
         MVC   PLSUBID,SPOOLID                                                  
         MVC   PLUSER,TWADEST      OPTIONAL DESTINATION                         
         OC    PLUSER,PLUSER                                                    
         BNZ   *+10                                                             
         MVC   PLUSER,TWAORIG                                                   
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         MVC   SPOOLBUF,ATIA                                                    
         MVI   SPMODE,0            INITIALIZE SPOOL DSECT                       
         TM    GENSTAT1,MLTPQOK    TEST MULTIPLE PQ INTERVALS ALLOWED           
         BZ    *+8                 NO                                           
         MVI   SPMODE,X'80'                                                     
         MVC   VPRINT,TWAVPRNT                                                  
         MVC   ABOX,TWAVBOX                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,PLREPNO                                                 
         MVC   SPCONSYS,CONNSYS    PASS CONNECT SYSTEM TO SPOOL                 
         B     XIT                                                              
         EJECT                                                                  
*              TEST APPLICATION IS ABOUT TO EXCEED MAX IO COUNT                 
         SPACE 1                                                                
VCATCHIO CLI   OFFLINE,C'Y'        DON'T BOTHER IF OFF-LINE                     
         BE    XIT                                                              
         GOTO1 GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    R3,=H'9'                                                         
         D     R2,=F'10'           90 PERCENT OF MAX IOS IN R3                  
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    XIT                 NO - STILL WITHIN 90 PERCENT                 
         MVI   ERROR,NOTONLIN      JOB CANNOT BE RUN ONLINE                     
         TM    GENSTAT1,CATCHIOR   TEST USER WANTS TO REGAIN CONTROL            
         BO    XIT                 YES                                          
         B     VERREX                                                           
         DROP  R1                                                               
         EJECT                                                                  
*              EXITS                                                            
         SPACE 1                                                                
VERREX   NI    GENSTAT7,X'FF'-GES7BACK  NEVER RETURN TO APP                     
         L     R4,AERRAREA         ERRORS EXIT HERE                             
*                                                                               
         TM    GENSTAT7,GES7PDF    IF PDF ERRHOOK ACTV,IGNORE ERROPT            
         BO    *+16                                                             
         CLI   ERROPT,C'Y'         OPTION TO GO BACK TO USER                    
         MVI   ERROPT,0            SWITCHING OFF TO SHOW ERROR                  
         BE    XIT                                                              
*                                                                               
         TM    GENSTAT2,USGETTXT   TEST GENERATE A GETTXT CALL                  
         BO    VERREXB                                                          
         ZIC   R3,GETMSYS          MESSAGES UNDER THIS SYSTEM                   
         CLI   ERROR,60            UNLESS ITS A GENCON MESSAGE                  
         BH    VERREXA                                                          
         TM    GENSTAT2,USMYERSY   OR USER OVERRIDES                            
         BO    VERREXA                                                          
         LA    R3,255              GENCON MESSAGES ON SYS0 (GENERAL)            
         SPACE 1                                                                
VERREXA  GOTO1 GETMSG,PARAS,(ERROR,8(R4)),(X'FF',DMCB),                X        
               ((R3),DATAMGR)                                                   
         B     VERREX2                                                          
*                                                                               
VERREXB  CLI   OFFLINE,C'Y'        ENSURE A(OUTPUT) SET OFFLINE                 
         BNE   VERREXB1                                                         
         LA    R1,GETTXTCB+(GTAOUT-GETTXTD)                                     
         OC    0(L'GTAOUT,R1),0(R1)                                             
         BNZ   *+12                                                             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,0(R1)                                                       
*                                                                               
VERREXB1 GOTO1 GETTXT,GETTXTCB     CTL BLOCK MUST BE DEFINED                    
*                                                                               
VERREX2  CLI   OFFLINE,C'Y'                                                     
         JNE   *+12                                                             
         TM    WHEN,X'20'          TEST SOON                                    
         BO    VERREX2A            YES - DO ERRHOOK CALL AFTER TWAPRT           
         TM    GENSTAT7,GES7ERHK+GES7PDF                                        
         BZ    VERREX2A                                                         
         MVI   MODE,ERRHOOK                                                     
         BAS   RE,GO                                                            
*                                                                               
VERREX2A NI    GENSTAT7,X'FF'-GES7BACK  NEVER RETURN TO APP                     
         CLI   ACTNUM,ACTCHA       IS THIS A CHANGE ?                           
         BNE   VERREXC             NO - SKIP                                    
         CLI   TWALACT,ACTSEL      DID CHANGE FOLLOW SELECT                     
         BNE   VERREXC             NO - SKIP                                    
         MVI   ACTNUM,ACTSEL       PRETEND THIS IS SELECT FOR NEXT TRY          
*                                                                               
VERREXC  CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         BE    VERREXD             YES                                          
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   EXIT                NO - EXIT                                    
         GOTO1 OPENPQ                                                           
*                                                                               
VERREXD  CLI   GCMODE,C'X'         TEST DELETE PARTIAL REPORT                   
         BNE   VERREXE                                                          
         MVI   SPMODE,X'FE'        DELETE PARTIALLY GENERATED REPORT            
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         GOTO1 OPENPQ              REOPEN PQ FOR ERROR PRINTING                 
         SPACE 1                                                                
VERREXE  OC    VPRINT,VPRINT                                                    
         BZ    VERREXF                                                          
         MVC   DMCB+12(4),ABOX                                                  
         OC    ABOX,ABOX                                                        
         BZ    *+8                                                              
         MVI   DMCB+12,C'B'                                                     
         GOTO1 REQTWA,DMCB,(3,(RA)),(X'FF',ACOMFACS),VPRINT                     
         SPACE 1                                                                
         L     R0,ACURFORC         FORCE CURSOR TO THIS ADDRESS?                
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         LR    R2,R0                                                            
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         L     R2,EFHREC                                                        
         SPACE 1                                                                
         MVCDD P(#CURADLQ),GE#CURAD                                             
         LH    R0,2(R2)            FIELD ADDRESS                                
         SRDL  R0,32                                                            
         D     R0,=F'80'                                                        
         AHI   R0,1                R0 = COL NUMBER  (01-80)                     
         LA    R1,1(R1)            R1 = ROW NUMBER  (01-24)                     
         CVD   R1,DUB              MOVE CURSOR ROW NUMBER                       
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVI   P+#CURADLQ+1,C'('                                                
         MVI   P+#CURADLQ+4,C','                                                
         MVI   P+#CURADLQ+7,C')'                                                
         MVC   P+#CURADLQ+2(2),DUB+1                                            
         CVD   R0,DUB              MOVE CURSOR COLUMN NUMBER                    
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   P+#CURADLQ+5(2),DUB+1                                            
         GOTO1 VPRINT,DMCB,SPACES-1,=C'BL01'                                    
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
         TM    WHEN,X'20'          TEST SOON                                    
         BZ    VERREXF             NO                                           
         TM    GENSTAT7,GES7PDF    TEST PDF ERRHOOK ACTIVE                      
         BZ    VERREXF                                                          
         MVI   MODE,ERRHOOK        YES - DO ERRHOOK CALL NOW                    
         BAS   RE,GO                                                            
*                                                                               
VERREXF  MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         SPACE 1                                                                
         TM    GENSTAT3,DIEONERR   IF REQUIRED TO DIE ON ERRORS                 
         BZ    VERREXX                                                          
         CLI   OFFLINE,C'Y'        INSURE WE'RE OFFLINE                         
         BNE   VERREXX                                                          
         DC    H'0'                NOW DIE (SO THAT WE GET DUMP)                
         SPACE 1                                                                
VERREXX  B     EXIT                                                             
         EJECT                                                                  
VRFP     DS    0H                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    XIT                                                              
*                                                                               
         L     R3,0(R1)                                                         
         USING QRFPD,R3                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP CONTROL BLOCK                  
         USING RFPBLK,R4                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'E3000A67',0  LOAD RFP MODULE                     
         L     RF,0(R1)                                                         
*                          MORE LITERALS                                        
         LA    R0,RFPTABN          LOCATE ROUTINE IN BRANCH TABLE               
         LA    R1,RFPTAB                                                        
         CLC   QRFPMODE,0(R1)                                                   
         BE    *+14                                                             
         LA    R1,L'RFPTAB(R1)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                INVALID MODE                                 
*                                                                               
         ICM   RE,15,1(R1)                                                      
         RELOC (R1)                                                             
         AR    RE,R1                                                            
         BR    RE                  BRANCH TO ROUTINE                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE $RFP INTERFACE                              *         
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         LR    R0,R4               CLEAR $RFP INTERFACE BLOCK                   
         SR    R1,R1                                                            
         ICM   R1,3,=Y(RFPBLKLN)                                                
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2                                                            
*                                                                               
* ARFPBLK = A(8K RFPBLK, 5K MINIO AREA, 1K MINIO REC TABLE)                     
*                                                                               
         MVI   RFPINIT,0           INITIALIZE RFP BLOCK                         
         MVC   RFPACOMF,ACOMFACS   - A(COMFACS)                                 
         LA    R1,2048(R4)         RFPBLK + 8K                                  
         LA    R1,2048(R1)                                                      
         LA    R1,2048(R1)                                                      
         LA    R1,2048(R1)                                                      
         ST    R1,RFPAMIN          - A(MINIO IO BUFFER)                         
         LA    R1,2048(R1)         RFPBLK + 5K                                  
         LA    R1,3072(R1)                                                      
         ST    R1,RFPAMINR         - A(RFP REQUEST TABLE)                       
         MVC   RFPMINRL,=H'1024'   - LENGTH OF RFPAMINR                         
         MVC   RFPFUID,TWAORIG     - USER ID                                    
         MVC   RFPFAGY,TWAAGY      - AGENCY                                     
         MVC   RFPFSYS,SYSTEM      - CONNECTED SYSTEM                           
*                                                                               
         TM    GENSTAT5,USERFPXT   IF WE ARE TO USE EXTENSION                   
         BNO   *+8                                                              
         OI    RFPFLAGS,RFPXSYMS      SET FLAG                                  
*                                                                               
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
         CLI   RFPERROR,RFPNOERR   ANY ERRORS?                                  
         BE    *+6                  NO                                          
         DC    H'0'                                                             
*                                                                               
         OI    RFPFFLAG,RFPSPOOF    - SET SPOOF-STYLE REQUEST                   
         B     XIT                  NO                                          
         EJECT                                                                  
***********************************************************************         
*              VALIDATE GROUP                                         *         
***********************************************************************         
*                                                                               
GROUP    DS    0H                                                               
         MVC   RFPFGRP,QRFPWORK    GROUP NAME                                   
         OC    RFPFGRP,SPACES                                                   
         MVI   RFPMODE,RFPVALGP    VALIDATE GROUP                               
         OI    RFPFFLAG,RFPFSYMS   RETURN ALL SYMBOLIC EQUATES                  
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   RFPERROR,RFPNOERR                                                
         BE    XIT                                                              
         CLI   RFPERROR,RFPNOGRP                                                
         BE    *+6                                                              
         DC    H'0'                UNKNOWN ERROR                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSYS,X'FF'        GENERAL MSG SYSTEM                           
         OI    GENSTAT2,USGETTXT                                                
         MVI   GTMSGNO1,109        INVALID GROUP NAME                           
         DROP  R1                                                               
         B     VERREX                                                           
         EJECT                                                                  
***********************************************************************         
*              VALIDATE SYMBOL & RETURN CORRESPONDING ESCAPE SEQUENCE *         
***********************************************************************         
*                                                                               
SYMBOL   DS    0H                                                               
         ZIC   R0,RFPVNUMS         # OF SYMBOLS IN RFP TABLE                    
*                                                                               
         LA    R5,RFPVSYMS         DEFAULT TO ORIGINAL SYMBOL TABLE             
*                                                                               
         TM    GENSTAT5,USERFPXT   IF WE ARE TO USE EXTENSION                   
         BNO   *+12                                                             
         LH    R5,=Y(RFPXTNSN-RFPBLK)  RESET SYMBOL TABLE POINTER               
         LA    R5,RFPBLK(R5)                                                    
*                                                                               
         CLC   QRFPWORK,RFPVSYMB-RFPVSYMS(R5)                                   
         BE    SYMB100                                                          
         LA    R5,RFPVSYML(R5)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         XC    QRFPWORK,QRFPWORK   PASS BACK NULLS FOR INVALID SYMBOL           
         B     SYMBX                                                            
*                                                                               
SYMB100  XC    QRFPWORK,QRFPWORK                                                
         MVC   QRFPWORK(L'RFPVSYME),RFPVSYME-RFPVSYMS(R5)                       
*                                                                               
SYMBX    B     XIT                                                              
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              INPUT BRANCH TABLE                                     *         
***********************************************************************         
*                                                                               
RFPTAB   DS    0XL5                                                             
         DC    AL1(QRFPINIT),AL4(INIT)        INITIALIZE $RFP INTERFACE         
         DC    AL1(QRFPGVAL),AL4(GROUP)       VALIDATE GROUP                    
         DC    AL1(QRFPSYMB),AL4(SYMBOL)      VALIDATE SYMBOL NAME              
RFPTABN  EQU   (*-RFPTAB)/L'RFPTAB                                              
         EJECT                                                                  
*                          MORE LITERALS                                        
         LTORG                                                                  
         EJECT                                                                  
UPRLWR   DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7F' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'40919293949596979899404040404040' 90-9F                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' B0-BF                     
         DC    XL16'40818283848586878889404040404040' C0-CF                     
         DC    XL16'40919293949596979899404040404040' D0-DF                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                     
         SPACE 3                                                                
         DROP  R7,R9               DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              INITIALIZATION OF GENERAL STORAGE                                
         SPACE 1                                                                
         DS    0D                                                               
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB-8(8),=C'**DUB***'         SEED DSECT                         
         MVC   ADDAY-8(8),=C'*EXTRNS*'                                          
         MVC   BOOKVAL-8(8),=C'*CORERES'                                        
         MVC   DATADISP-8(8),=C'*SYSCON*'                                       
         MVC   KEY-8(8),=C'**KEYS**'                                            
         SPACE 1                                                                
INITB    LA    R2,IO-8                       SET UP IO AREAS                    
         LA    R3,AIO1                                                          
         LA    R4,X'F1'                                                         
         ZIC   R0,MAXIOS                                                        
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
         SPACE 1                                                                
INITD    MVC   0(8,R2),=C'**I/O1**'                                             
         STC   R4,5(R2)                                                         
         LA    R2,8(R2)                                                         
         ST    R2,0(R3)                                                         
         A     R2,SIZEIO                                                        
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,INITD                                                         
         MVC   AIO,AIO1                      DEFAULT IS IO AREA 1               
         SPACE 1                                                                
         MVC   0(8,R2),=C'**PAD***'          INITIALIZE USER'S AREA             
         LA    R2,8(R2)                                                         
         ST    R2,ASYSD                                                         
         LR    R1,R8                         COMPUTE SPACE AVAILABLE            
         A     R1,LWORK                                                         
         SR    R1,R2                                                            
         ST    R1,LSYSD                                                         
         EJECT                                                                  
*              TWA DETAILS                                                      
         SPACE 3                                                                
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         CLI   SYSTEM,C'C'                         IF CONTROL SYSTEM            
         BNE   *+8                                                              
         L     RA,20(R1)                                                        
         USING CONHEADH-64,RA                                                   
         ST    RA,ATWA                                                          
         MVC   AGENCY,TWAAGY                                                    
         MVI   OFFLINE,C'N'                                                     
         OC    TWAVPRNT,TWAVPRNT                                                
         BZ    INITIAL2                                                         
         MVI   OFFLINE,C'Y'                                                     
         L     RF,TWADCONS                                                      
         TM    TWACFLAG,TWACFIDF   TEST NEW VERSION OF SPOOF                    
         BZ    *+10                                                             
         MVC   VADUMMY,TDUMMY-TWADCOND(RF)                                      
         MVC   VPRINT,TWAVPRNT     OFF-LINE ADDRESSES IN TWA                    
         MVC   ABOX,TWAVBOX                                                     
         MVC   BUFFALO,TWAVBUFF                                                 
         MVC   SORTER,TWAVSORT                                                  
         MVC   WORKER,TWAVWORK                                                  
*                                                                               
INITIAL2 LA    R2,64(RA)                                                        
         ST    R2,AERRAREA                                                      
         LH    R2,=AL2(REPINFO-TWATASK)                                         
         AR    R2,RA                 SET UP A(REPORT ID INFO)                   
         ST    R2,AREPINFO                                                      
*                                                                               
         TM    GENSTAT1,NOSETEFH     DON'T BOTHER - APPLIC DID IT               
         BO    INITEX                                                           
         LA    R2,CONSERV+L'CONSERV  SET UP ACONS FOR ACCESS                    
         LA    R3,EFHREC             TO FIELDS WITH EXTENDED                    
         LA    R4,4                  FIELD HEADERS                              
         LA    R5,EFHOTH                                                        
INITE    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ST    R2,0(R3)                                                         
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BXLE  R3,R4,INITE                                                      
         ST    R2,EFHTAG                                                        
INITEX   DS    0H                                                               
         EJECT                                                                  
*              SET UP ADDRESSES                                                 
         SPACE 1                                                                
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         CLI   SYSTEM,C'C'               IF CONTROL SYSTEM                      
         BNE   INITF                                                            
         LM    R2,R3,0(R1)               SYSPARMS IS DIFFERENT                  
         L     R4,12(R1)                                                        
INITF    ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         ST    R4,RCCOMFAC                                                      
         MVC   ADDAY,CADDAY        COMFACS FACILITIES                           
         MVC   SWITCH,CSWITCH                                                   
         MVC   CALLOV,CCALLOV                                                   
         MVC   CASHVAL,CCASHVAL                                                 
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   RCDATCON,DATCON                                                  
         MVC   DATVAL,CDATVAL                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   GETTXT,CGETTXT                                                   
         MVC   GETPROF,CGETPROF                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   HEXIN,CHEXIN                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SCANNER,CSCANNER                                                 
         MVC   SCUNKEY,CSCUNKEY                                                 
         MVC   UNSCAN,CUNSCAN                                                   
         MVC   REQTWA,CREQTWA                                                   
         MVC   PERVAL,CPERVAL                                                   
         MVC   DICTATE,CDICTATE                                                 
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   PERVERT,CPERVERT                                                 
         MVC   SECRET,CSECRET                                                   
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         DROP  R4                                                               
         SPACE                                                                  
         L     R2,=A(CHKDEL)                                                    
         A     R2,GENCRELO                                                      
         ST    R2,ACHKDEL                                                       
         L     R2,=A(CHKDELN)                                                   
         A     R2,GENCRELO                                                      
         ST    R2,ACHKDELN                                                      
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         LA    R3,CRESLIST         MODULES WITH PHONY CALLOV READS.             
         LA    R4,CRESLISL                                                      
         SPACE 2                                                                
INIT2    XC    0(4,R2),0(R2)                                                    
         CLI   0(R3),X'FF'                                                      
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R3)                                                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
INIT2A   LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         SPACE 1                                                                
         L     R2,=A(VCOMMON)      SET UP COMMON ENTRIES                        
         A     R2,GENCRELO                                                      
         SR    R3,R3                                                            
         LA    R4,ANY                                                           
         LA    R5,VCOUNT                                                        
         SPACE 2                                                                
INIT4    ST    R2,0(R4)            A(COMMON)                                    
         STC   R3,0(R4)            ROUTINE NUMBER                               
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,INIT4                                                         
*                                                                               
         LA   R5,VCOUNT2           SET UP NEW COMMON ENTRIES                    
         LA    R4,RFP                                                           
*                                                                               
INIT4A   ST    R2,0(R4)            A(COMMON)                                    
         STC   R3,0(R4)            ROUTINE NUMBER                               
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,INIT4A                                                        
*                                                                               
         EJECT                                                                  
*              OTHER INITIALIZATION                                             
         SPACE 1                                                                
         MVI   DMINBTS,X'C0'       PRESET VALUES                                
         TM    GENSTAT1,RDUPAPPL                                                
         BZ    *+8                                                              
         NI    DMINBTS,X'7F'       READ FOR UPDATE CONTROLLED BY APPLIC         
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   NLISTS,15                                                        
         CLI   LRECACT,0           L'RECACT TABLE ENTRIES                       
         BNE   *+8                                                              
         MVI   LRECACT,12          DEFAULT IS 12                                
         GOTO1 GETFACT,DMCB,0      SAVE SE NUM                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SVSYS,FASYS                                                      
         MVC   LANG,FALANG         LANGUAGE CODE                                
         MVC   CTRY,FACTRY         COUNTRY CODE                                 
         DROP  R1                                                               
         LA    R1,TWAKEYSV         MAKE TWAKEYSV ADDRESSABLE                    
         ST    R1,ATWAKYSV                                                      
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         LTR   R1,R1               IF WE'RE OFF LINE                            
         BZ    INIT6                                                            
         CLC   MCDATE,SPACES       AND DATE CARD WAS RECEIVED                   
         BE    INIT6                                                            
         MVC   RCDATE,MCDATE       USE THIS DATE                                
         SPACE 1                                                                
INIT6    GOTO1 DATCON,DMCB,(4,RCDATE),(3,BTODAY)                                
*                                                                               
         GOTO1 DICTATE,DMCB,C'LU  ',DCLPUC,LPUCASE                              
         GOTO1 (RF),(R1),C'LL  ',DCLPMC,LPMCASE                                 
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    INIT8               YES                                          
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            A(TIOB)                                      
         CLI   SYSTEM,C'C'         TEST CONTROL SYSTEM                          
         BNE   *+8                 NO                                           
         L     RF,28(R1)           SYSPARMS IS DIFFERENT                        
         USING TIOBD,RF                                                         
         MVC   PFAID,TIOBAID       AID BYTE (0=ENTER, 1-24=PFKEY NO.)           
         DROP  RF                                                               
*                                                                               
INIT8    CLI   GCMODE,C'S'         EXIT HERE IF SLAVED MODE                     
         BE    INITX                                                            
*                                                                               
         LA    R2,CONSERVH         POSTION CURSOR IN CASE OF ERROR              
         MVC   DMCB+4(4),=X'D9000A31'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)      GO TO INITIALIZE DATA DICT. CONTROL          
INITX    J     XIT                                                              
         SPACE 1                                                                
CRESLIST EQU   *                                                                
*&&US*&& DC    AL1(QBOOKVAL)                                                    
*&&UK*&& DC    X'FF'                                                            
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QDAYVAL)                                                     
CRESSPUL DC    AL1(QSPOOL)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QTIMVAL)                                                     
*&&US*&& DC    AL1(QUNDAY)                                                      
*&&UK*&& DC    X'FF'                                                            
         DC    AL1(QUNDRLIN)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QXSORT)                                                      
         DC    AL1(QGENPRG)                                                     
         DC    AL1(QEDITOR)                                                     
CRESLISL EQU   *-CRESLIST                                                       
         EJECT                                                                  
*              CONSTANTS FOR INITIAL ROUTINE                                    
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DCLPMC   DS    0X                  MIXED CASE LITERALS                          
         DCDDL GE#BYID,(#BYIDLQ)   BY ID                                        
         DCDDL GE#CHGNO,(#CHGNOLQ) CHANGE NO.                                   
         DCDDL GE#CHGR,(#CHGRLQ)   CHANGE REASON                                
         DCDDL GE#DSKAD,(#DSKADLQ) DISK ADDRESS                                 
         DCDDL GE#PASSW,(#PASSWLQ) PASSWORD                                     
         DCDDL GE#RECAD,(#RECADLQ) RECORD ADDED                                 
         DCDDL GE#RECLN,(#RECLNLQ) RECORD LENGTH                                
         DCDDL GE#SEC,(#SECLQ)     SECURITY                                     
         DCDDL GE#SECLV,(#SECLVLQ) SECURITY LEVEL                               
         DC    AL1(0)                                                           
DCLPUC   DS    0X                  UPPER CASE LITERALS                          
         DCDDL GE#ACTV,(#ACTVLQ)   ACTIVE                                       
         DCDDL GE#ACTV,(#ACTLQ)    ACT                                          
         DCDDL GE#ASAP,WHENLQ      ASAP                                         
         DCDDL GE#CHA,(#CHASLQ)    CHANGE (SINGLE CHR)                          
         DCDDL GE#CHA,(#CHALQ)     CHANGE (3 CHR)                               
         DCDDL GE#DEL,(#DELSLQ)    DELETE (SINGLE CHR)                          
         DCDDL GE#DEL,(#DELLQ)     DELETE (3 CHR)                               
         DCDDL GE#DDS,WHENLQ       DDS (AT DDS)                                 
         DCDDL GE#DIS,(#DISSLQ)    DISPLAY (SINGLE CHR)                         
         DCDDL GE#DIS,(#DISLQ)     DISPLAY (3 CHR)                              
         DCDDL GE#DIS,(#DISLLQ)    DISPLAY (FULL WORD)                          
         DCDDL GE#EXIT,(#EXITPLQ)  EXIT (PADDED)                                
         DCDDL GE#FIRST,(#FRSTPLQ) FIRST (PADDED)                               
         DCDDL GE#HELP,(#HELPPLQ)  HELP (PADDED)                                
         DCDDL GE#ID,(#IDLQ)       ID                                           
         DCDDL GE#LAST,(#LASTPLQ)  LAST (PADDED)                                
         DCDDL GE#LIS,(#LISTPLQ)   LIST (PADDED)                                
         DCDDL GE#ME,(#MELQ)       ME                                           
         DCDDL GE#NEXT,(#NEXTLQ)   NEXT                                         
         DCDDL GE#NEXT,(#NEXTPLQ)  NEXT (PADDED)                                
         DCDDL GE#NO,WHENLQ        NO                                           
         DCDDL GE#NOW,WHENLQ       NOW                                          
         DCDDL GE#ON,(#OLQ)        O (OVERNIGHT)                                
         DCDDL GE#ONT,WHENLQ       ON (OVERNIGHT)                               
         DCDDL GE#OV,WHENLQ        OV (OVERNIGHT)                               
         DCDDL GE#PASSW,(#PASSLQ)  PASS                                         
         DCDDL GE#REP,(#REPSLQ)    REPORT (SINGLE CHR)                          
         DCDDL GE#REP,(#REPLQ)     REPORT (3 CHR)                               
         DCDDL GE#REQ,(#REQLQ)     REQ                                          
         DCDDL GE#RES,(#RESSLQ)    RESTORE (SINGLE CHR)                         
         DCDDL GE#RES,(#RESLQ)     RESTORE (3 CHR)                              
         DCDDL GE#SEL,(#SELSLQ)    SELECT (SINGLE CHR)                          
         DCDDL GE#SEL,(#SELPLQ)    SELECT (PADDED)                              
         DCDDL GE#SOON,WHENLQ      SOON                                         
         DCDDL GE#THIS,(#THISPLQ)  THIS (PADDED)                                
         DCDDL GE#TODAY,(#TODAYLQ) TODAY                                        
         DCDDL GE#X,(#XPLQ)        'X' (PADDED)                                 
         DC    AL1(0)                                                           
*                                                                               
         TITLE 'T00A30 - GENERAL SPOOL/MAINT CONTROLLER - VALOUT'               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE OUTPUT FIELD                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VALOUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 ANY                 READ IN FIELD                                
*                                                                               
         MVCDD DUB,GE#FILE         TRANSLATE 'FILE'                             
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),WORK         RFP?                                         
         BNE   VALOUTA             NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALOUTX                                                          
*                                                                               
         TM    WHEN,X'10'+X'08'    ONLY ALLOWED FOR OV & DDS                    
         BNZ   *+12                                                             
         MVI   ERROR,INVALID         NO - RFP NOT ALLOWED                       
         B     VALOUTR               GOTO1 ERREX                                
*                                                                               
         MVC   ARFPBLK,ATIA        DEFINE WORK AREA                             
         OI    GENSTAT7,GES$DRFP   SET GROUP IN DESTINATION FIELD               
*                                                                               
         B     VALOUTX                                                          
*                                                                               
VALOUTA  DS    0H                                                               
*                                                                               
*        CHECK IF DESTINATION IS 'FILE'                                         
*                                                                               
         ICM   R1,15,EFHDEST       SKIP IF NO DEST FIELD                        
         BZ    VALOUTB                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R1)          INPUT LENGTH                                 
         BZ    VALOUTB                                                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),8(R1)        RFP? - IE. 'FILE'                            
         BNE   VALOUTB             NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALOUTX                                                          
*                                                                               
         TM    WHEN,X'10'+X'08'    ONLY ALLOWED FOR OV & DDS                    
         BNZ   *+12                                                             
         MVI   ERROR,INVALID         NO - RFP NOT ALLOWED                       
         B     VALOUTR               GOTO1 ERREX                                
*                                                                               
         MVC   ARFPBLK,ATIA        DEFINE WORK AREA                             
*                                                                               
VALOUTB  DS    0H                                                               
*                                                                               
         OC    ARFPBLK,ARFPBLK     IF DOING RFP THEN GROUP ID HERE              
         BZ    VALOUTC                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALOUTX                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING QRFPD,R4                                                         
         MVI   QRFPMODE,QRFPINIT   INITIALIZE RFP                               
*                                                                               
         GOTO1 RFP,DMCB,(R4)                                                    
*                                                                               
         MVI   QRFPMODE,QRFPGVAL   VALIDATE GROUP ID                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          GET INPUT LENGTH                             
         BZ    *+10                SKIP MOVE IF NO INPUT                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)      COPY INPUT                                
*                                                                               
         GOTO1 RFP,DMCB,(R4)                                                    
         OI    GENSTAT7,GES$ORFP   SET GROUP IN OUTPUT FIELD                    
*                                                                               
         B     VALOUTX                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
VALOUTC  DS    0H                                                               
*                                                                               
         CLI   WORK,C'0'           TEST STARTS WITH A-I,J-R,S-Z,0-9             
         BL    *+12                                                             
         CLI   WORK,C'9'                                                        
         BNH   VALOUT1                                                          
         CLI   WORK,C'S'                                                        
         BL    *+12                                                             
         CLI   WORK,C'Z'                                                        
         BNH   VALOUT1                                                          
         CLI   WORK,C'J'                                                        
         BL    *+12                                                             
         CLI   WORK,C'R'                                                        
         BNH   VALOUT1                                                          
         CLI   WORK,C'A'                                                        
         BL    *+12                                                             
         CLI   WORK,C'I'                                                        
         BNH   VALOUT1                                                          
         CLI   WORK,C'+'           TEST OUTPUT TYPE SPECIALS +,&,#              
         BE    VALOUT1                                                          
         CLI   WORK,C'&&'                                                       
         BE    VALOUT1                                                          
         CLI   WORK,C'#'                                                        
         BE    VALOUT1                                                          
*                                                                               
VALOUT0  MVC   TWAOUT,WORK         SAVE SPECIAL CODE IN OUTPUT TYPE             
         CLI   WORK,C'@'           TEST IF SQL TRANSFORM                        
         BE    VALOUT4                                                          
         CLI   WORK,C'/'           TEST IF REMOTE AFP NAME                      
         BE    VALOUT5                                                          
         MVI   ERROR,INVOUT                                                     
         B     VALOUTR                                                          
*                                                                               
VALOUT1  LA    R4,KEY              VALIDATE OUTPUT TYPE                         
         CLI   OFFLINE,C'Y'                                                     
         BE    VALOUT2A            DONT BOTHER IF OFFLINE                       
         XC    KEY,KEY                                                          
         USING CTOREC,R4                                                        
         MVI   CTOKTYP,C'O'                                                     
         MVC   CTOKID(6),WORK                                                   
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVC   SAVUSEIO,USEIO                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                                                             
         MVI   ERROR,INVOUT                                                     
         CLC   KEY(20),KEYSAVE                                                  
         BNE   VALOUTR                                                          
         LA    R4,IO                                                            
         CLI   1(RA),C'*'          UNLESS THIS IS A DDS TERMINAL                
         BE    VALOUT2                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VALOUT2                                                          
         LA    R6,CTODATA                                                       
         MVI   ELCODE,X'38'                                                     
         BAS   RE,FIRSTEL2                                                      
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      CHECK OUTPUT IS ALLOWED                      
         BNO   VALOUTR                                                          
         SPACE 1                                                                
VALOUT2  XC    FILENAME,FILENAME                                                
         MVC   USEIO,SAVUSEIO                                                   
VALOUT2A MVC   TWAOUT,WORK                                                      
*&&US                                                                           
         CLC   TWAOUT(3),=CL3'MLR' MAILER                                       
         BNE   VALOUTX                                                          
         CLC   QCRDCODE(2),=C'TM'  ONLY ALLOWED FOR TM                          
         BE    VALOUTX                                                          
         CLC   QCRDCODE(2),=C'TA'  OR TA                                        
         BE    VALOUTX                                                          
         B     VALOUTR                                                          
*&&                                                                             
         B     VALOUTX                                                          
         SPACE 1                                                                
VALOUT4  MVI   ERROR,0             VALIDATE SQL TRANSFORM @ABCDEF               
         CLI   OFFLINE,C'Y'                                                     
         BE    VALOUT4D            DONT BOTHER IF OFFLINE                       
         GOTO1 SWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                                
         L     R1,0(R1)                                                         
         MVC   DUB(1),TSYS-UTLD(R1)                                             
         CLI   DUB,X'0A'                                                        
         BE    VALOUT4A                                                         
         GOTO1 SWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                                
         CLI   4(R1),0                                                          
         BE    VALOUT4A                                                         
         MVI   ERROR,INVSQL                                                     
         B     VALOUTR                                                          
VALOUT4A LA    R4,KEY              SET KEY FOR SQL REFORM RECORD                
         USING GREFD,R4                                                         
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKREC,GREFRECQ                                                
         MVC   GREFAGY,TWAAGY                                                   
         MVC   GREFID,WORK+1                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                   
         BE    *+8                                                              
         MVI   ERROR,INVSQL                                                     
VALOUT4B CLI   DUB,X'0A'           TEST IF NEED TO SWITCH BACK                  
         BE    VALOUT4C                                                         
         GOTO1 SWITCH,DMCB,(DUB,X'FFFFFFFF'),0                                  
VALOUT4C CLI   ERROR,0                                                          
         BNE   VALOUTR                                                          
VALOUT4D MVC   TWAOUT,WORK         SAVE SQL FORMULA IN OUTPUT TYPE              
         B     VALOUTX                                                          
*                                                                               
VALOUT5  CLI   5(R2),3             REMOTE AFP INPUT AS /XX                      
         BE    VALOUT5X                                                         
         CLI   5(R2),6             REMOTE AFP INPUT AS /SPPXX                   
         BE    VALOUT5X                                                         
         MVI   ERROR,INVOUT                                                     
         B     VALOUTR                                                          
VALOUT5X MVC   TWAOUT,WORK         SAVE SQL FORMULA IN OUTPUT TYPE              
         B     VALOUTX                                                          
*                                                                               
VALOUTX  J     XIT                                                              
*                                                                               
VALOUTR  GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL2 (R6),DATADISP,ELCODE                                            
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'T00A30 - GENERAL SPOOL/MAINT CONTROLLER - VALDEST'              
***********************************************************************         
*                                                                     *         
*              ROUTINE TO VALIDATE DESTINATION FIELD                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VALDEST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GENSTAT7,GES$ORFP   TEST GROUP IN OUTPUT FIELD                   
         BNZ   VALDESTX            YES - NO VALIDATION REQUIRED                 
         TM    GENSTAT7,GES$DRFP   TEST GROUP IN DESTINATION FIELD              
         BNZ   VALDESTB            YES - VALIDATE THE GROUP                     
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVCDD DUB,GE#FILE         TRANSLATE 'FILE'                             
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),WORK         RFP?                                         
         BNE   VALDESTA            NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALDESTX                                                         
*                                                                               
         TM    WHEN,X'10'+X'08'    ONLY ALLOWED FOR OV & DDS                    
         BNZ   *+12                                                             
         MVI   ERROR,INVALID         NO - RFP NOT ALLOWED                       
         B     VALDESTR              GOTO1 ERREX                                
*                                                                               
         ICM   R1,15,EFHOUT        ERROR IF NO OUTPUT FIELD                     
         BNZ   *+12                                                             
         MVI   ERROR,INVALID         NO - RFP NOT ALLOWED                       
         B     VDESTERR              GOTO1 ERREX                                
*                                                                               
         CLI   5(R1),0             THERE MUST BE INPUT IN OUTPUT FLD            
         BNE   *+14                                                             
         LR    R2,R1               CURSOR TO OUTPUT FIELD                       
         MVI   ERROR,MISSING                                                    
         B     VDESTERR              GOTO1 ERREX                                
*                                                                               
         MVC   ARFPBLK,ATIA        DEFINE WORK AREA                             
*                                                                               
         B     VALDESTX                                                         
*                                                                               
VALDESTA DS    0H                                                               
*                                                                               
*        CHECK IF OUTPUT IS 'FILE'                                              
*                                                                               
         ICM   R1,15,EFHOUT        SKIP IF NO OUTPUT FIELD                      
         BZ    VALDESTB                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R1)          INPUT LENGTH                                 
         BZ    VALDESTB                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),8(R1)        RFP?                                         
         BNE   VALDESTB                                                         
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALDESTX                                                         
*                                                                               
         TM    WHEN,X'10'+X'08'    ONLY ALLOWED FOR OV & DDS                    
         BNZ   *+12                                                             
         MVI   ERROR,INVALID         NO - RFP NOT ALLOWED                       
         B     VALDESTR              GOTO1 ERREX                                
*                                                                               
         MVC   ARFPBLK,ATIA        DEFINE WORK AREA                             
*                                                                               
VALDESTB DS    0H                                                               
*                                                                               
         OC    ARFPBLK,ARFPBLK     IF DOING RFP THEN GROUP ID HERE              
         BZ    VALDESTC                                                         
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFFLINE                              
         BE    VALDESTX                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING QRFPD,R4                                                         
         MVI   QRFPMODE,QRFPINIT   INITIALIZE RFP                               
*                                                                               
         GOTO1 RFP,DMCB,(R4)                                                    
*                                                                               
         MVI   QRFPMODE,QRFPGVAL   VALIDATE GROUP ID                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R2)            GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)      COPY INPUT                                
*                                                                               
         GOTO1 RFP,DMCB,(R4)                                                    
*                                                                               
         B     VALDESTX                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
VALDESTC DS    0H                                                               
*                                                                               
         USING CTIREC,R4                                                        
         LA    R4,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R4,BIGKEY                                                        
         XC    0(L'CTIKEY,R4),0(R4)                                             
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         CLC   WORK(10),=CL10'DDS'                                              
         BE    VALDEST1                                                         
         CLC   WORK(10),=CL10'SJR'                                              
         BNE   VALDEST2                                                         
VALDEST1 MVC   CTIKID,WORK                                                      
         SPACE 1                                                                
VALDEST2 BAS   RE,CTREAD           READ ID RECORD                               
         BNE   VALDESTR                                                         
         SPACE 1                                                                
         CLC   =C'FX=',WORK        TEST FOR FAX ID                              
         BNE   *+12                                                             
         BAS   RE,VALFAX           GO VALIDATE                                  
         B     VALDESTX            SKIP THE REST                                
         SPACE 1                                                                
*&&US*&& GOTO1 CALLOV,DMCB,0,X'D9000AFA',0 GET A(GETIDS) T00AFA IN US           
*&&UK*&& GOTO1 CALLOV,DMCB,0,X'D9000AF9',0 GET A(GETIDS) T00AF9 IN UK           
         L     RF,0(R1)                                                         
         L     R5,DATAMGR           GET LIST OF VALID IDS                       
         GOTO1 (RF),DMCB,(C'D',AIO),0,(R5)                                      
         CLI   DMCB,0                                                           
         BE    VALDESTR              NONE FOUND                                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                   DISK ERROR                                 
         DC    H'0'                                                             
         ZIC   R5,DMCB               R5=NUMBER OF DEST IDS                      
         L     R6,DMCB+4             R6=ADR OF DEST IDS                         
*                                                                               
VALDEST4 CLI   0(R6),X'FF'         SEARCH TABLE OF VALID IDS                    
         BE    VALDESTR                                                         
         CLC   WORK(10),0(R6)                                                   
         BE    VALDEST5                                                         
         LA    R6,12(R6)                                                        
         BCT   R5,VALDEST4                                                      
         B     VALDESTR                                                         
*                                                                               
VALDEST5 MVC   TWADEST,10(R6)      SET DEST ID NUM                              
*                                                                               
VALDESTX J     XIT                                                              
         EJECT                                                                  
*              VALIDATE A FAX ID (SET UP FAXINFO BLOCK IF OFFLINE)              
         SPACE 1                                                                
*                                  AIO = A(ID RECORD)                           
VALFAX   NTR1                                                                   
         L     R4,AIO                                                           
         USING CTIREC,R4                                                        
         LA    R3,CTIDATA                                                       
         XR    R1,R1                                                            
VFAX10   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTDSCELQ      GET DESCRIPTION ELEMENT                      
         BE    *+14                                                             
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VFAX10                                                           
         USING CTDSCD,R3           R3 = A(DESCRIPTION ELEMENT)                  
         SPACE 1                                                                
         XC    KEY,KEY             BUILD KEY FOR EDICT RECORD                   
         USING EDIKEYD,R4                                                       
         LA    R4,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R4,BIGKEY                                                        
         XC    0(L'EDIKEY,R4),0(R4)                                             
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    TYPE                                         
         MVC   EDINAME,CTDSC       EBCDIC USER ID FROM ID RECORD                
         BAS   RE,CTREAD           READ THE RECORD                              
         BNE   ERREDICT                                                         
         SPACE 1                                                                
         XC    KEY,KEY             BUILD KEY FOR FAX RECORD                     
         XC    0(L'EDIKEY,R4),0(R4)                                             
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU    TYPE                                         
         MVC   CTFXAGY,TWAAGY      AGENCY                                       
         MVC   CTFXCODE(5),8+3(R2) FIELD HAS FX=XXXXX                           
         OC    CTFXCODE,SPACES                                                  
         BAS   RE,CTREAD           READ THE RECORD                              
         BNE   ERRFAX                                                           
         SPACE 1                                                                
         OC    TWAOUT,TWAOUT       IF OUTPUT TYPE NOT INPUT                     
         BNZ   VFAX40                                                           
         MVC   TWAOUT,=CL6'FAX'    SET OUTPUT TYPE TO 'FAX'                     
         ICM   R1,15,EFHOUT        IF OUTPUT TYPE FIELD EXISTS                  
         BZ    VFAX40                                                           
         MVC   8(6,R1),TWAOUT      DISPLAY TO SCREEN                            
         OI    6(R1),X'80'                                                      
         SPACE 1                                                                
VFAX40   CLI   OFFLINE,C'Y'        IF WE'RE NOT OFFLINE                         
         BNE   VFAXX               THEN DONE                                    
         SPACE 1                                                                
         L     R3,TWADCONS         ELSE INITIALIZE FAXLINK'S INFO BLOCK         
         USING TWADCOND,R3                                                      
         L     R3,TFAXINFO         R3 = A(FAX INFO BLOCK)                       
         USING FAXINFOD,R3                                                      
         MVI   FXISTAT,FXISPEND    SET FAX PENDING                              
         MVC   FXISIDNO,TWAORIG    ORIGIN ID NUMBER                             
         MVC   FXISAGY,TWAAGY      ALPHA AGENCY CODE                            
         MVC   FXISFXCD,CTFXCODE   FAX ID CODE (FROM CTFILE KEY)                
         MVC   FXISRQST,REMUSER    REQUESTOR                                    
         MVC   FXITRSYS,RCPROG     SYSTEM                                       
         MVC   FXITRPRG(2),RCPROG+2  REPORT ID                                  
         SPACE 1                                                                
VFAXX    B     VALDESTX                                                         
         EJECT                                                                  
*              READ A CONTROL FILE RECORD                                       
         SPACE 2                                                                
CTREAD   NTR1                                                                   
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVC   SAVUSEIO,USEIO                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         MVC   USEIO,SAVUSEIO                                                   
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+14                                                             
         CLC   BIGKEY(25),KEYSAVE                                               
         B     *+10                                                             
         CLC   KEY(25),KEYSAVE                                                  
         B     VALDESTX            RETURN CC                                    
         EJECT                                                                  
*              ODDMENTS FOR VALDEST ROUTINE                                     
         SPACE 2                                                                
ERREDICT MVI   ERROR,NOEDICTR                                                   
         B     VDESTERR                                                         
         SPACE 1                                                                
ERRFAX   MVI   ERROR,INVFAX                                                     
         B     VDESTERR                                                         
         SPACE 1                                                                
VALDESTR MVI   ERROR,INVDEST                                                    
VDESTERR GOTO1 ERREX                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF DELETED RECORD EXISTS ON FILE                
         SPACE 1                                                                
         DS    0D                                                               
CHKDEL   NMOD1 CHKDX-CHKD,**CHKD**,CLEAR=YES                                    
         LR    R4,RC               R4 = LOCAL WORKING STORAGE                   
         USING CHKD,R4                                                          
         L     RC,0(R1)                                                         
         MVC   SAVEIO,AIO                                                       
         LA    R1,CHKIO                                                         
         ST    R1,AIO                                                           
         MVI   RDUPDATE,C'Y'       ALWAYS WANT READ FOR UPDATE HERE             
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                CHECK IF RECORD ALREADY EXISTS               
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,SAVEIO                                                       
*                                                                               
         LA    R2,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R2,BIGKEY                                                        
*                                                                               
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),KEYSAVE                                                  
         BNE   CHKDNO                                                           
         TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         BNO   CHKDNO                                                           
         GOTO1 WRITE               YES - WRITE BACK NEW ONE                     
         XR    RF,RF               AND RETURN CC EQUAL                          
         LTR   RF,RF                                                            
         J     XIT                                                              
CHKDNO   MVC   KEY,KEYSAVE                                                      
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         LTR   RF,RF                                                            
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF DELETED RECORD EXISTS ON FILE                
         SPACE 1                                                                
         DS    0D                                                               
CHKDELN  NMOD1 CHKDX-CHKD,**CHKD**,CLEAR=YES                                    
         LR    R4,RC               R4 = LOCAL WORKING STORAGE                   
         USING CHKD,R4                                                          
         L     RC,0(R1)                                                         
         MVC   SAVEIO,AIO                                                       
         LA    R1,CHKIO                                                         
         ST    R1,AIO                                                           
         MVI   RDUPDATE,C'Y'       ALWAYS WANT READ FOR UPDATE HERE             
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                CHECK IF RECORD ALREADY EXISTS               
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         LA    R2,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R2,BIGKEY                                                        
*                                                                               
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),KEYSAVE                                                  
         BE    CHKD10                                                           
         MVC   KEY,KEYSAVE                                                      
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         B     CHKDNON                                                          
CHKD10   TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         BNO   CHKDNON                                                          
*                                  YES-WRITE BACK NEW KEY/REC                   
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LR    R5,R6                                                            
         AH    R6,LKEY                                                          
         NI    2(R6),X'7F'        CLEAR DELETED BIT                             
         MVC   SAVRLEN,0(R6)                                                    
         MVC   AIO,SAVEIO                                                       
         L     R6,AIO                                                           
         LH    R1,LKEY                                                          
         LA    R3,0(R1,R6)                                                      
         MVC   SAVRLEN,0(R3)       SAVE NEW LEN                                 
         AH    R1,LSTATUS                                                       
         LA    R1,2+4-1     LENGTH PLUS LINK -1                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)         COPY KEY,LEN,STAT,LINK TO NEW              
         MVC   0(2,R3),SAVRLEN        RESTORE NEW LEN                           
         GOTO1 PUTREC                                                           
*                                                                               
         LA    R6,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
*                                                                               
         AH    R6,LKEY                                                          
         NI    0(R6),X'7F'         TURN OFF DELETE BIT                          
*                                                                               
         GOTO1 WRITE                                                            
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         XR    RF,RF               AND RETURN CC EQUAL                          
CHKDNON  LTR   RF,RF                                                            
         MVC   AIO,SAVEIO                                                       
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MERGE IN KEY FIELDS FROM SAVED SCREEN                            
         SPACE 1                                                                
         DS    0D                                                               
KEYMERGE NTR1  BASE=*,LABEL=*                                                   
         TM    GENSTAT1,USKYMRG    UNLESS OVERRIDDEN,                           
         BNZ   *+12                                                             
         CLI   ACTNUM,ACTLIST      DON'T MERGE FOR LIST                         
         BE    KXIT                                                             
         TM    GENSTAT1,EFHKYMRG   MERGE ON EXTENDED HEADERS                    
         BNZ   KEYM5                                                            
         SPACE 1                                                                
*                                                                               
*       R3 HAS NEW SCREEN AFSTKEY                                               
*       R4 IS USED TO WALK THROUGH OLD SCREEN KEY AREA                          
*       NEW SCREEN KEY FIELDS WILL BE FILLED FROM OLD IF:                       
*            1) TAG FIELD (PRECEEDING PROTECTED FIELD) MATCHES                  
*               IN LENGTH AND CONTENTS                                          
*            2) DATA FIELD (FOLLOWING UNPROTECTED FIELD) MATCHES                
*               IN LENGTH                                                       
*            3) IF MORE THAN ONE MATCH, FIRST FOUND IN OLD                      
*               KEY FIELDS IS USED                                              
*            4) IF EFHKYMRG IS ON USE EXTENDED FIELD HEADER NUMBERS             
*               TO MATCH FIELDS                                                 
*                                                                               
KEYM1    CLI   0(R3),0             TEST END-OF-(NEW)SCREEN                      
         BE    KEYM20                                                           
         TM    1(R3),X'20'         FIRST FIELD MUST BE PROTECTED                
         BNO   KEYM4                                                            
         CLI   0(R3),9             IF LENGTH IS 1 WE'RE DONE                    
         BE    KEYM20                                                           
         ZIC   RF,0(R3)            RF=L'TAG FIELD                               
         LA    R6,0(RF,R3)         R6=A(NEW DATA FIELD)                         
         TM    1(R6),X'20'                                                      
         BO    KEYM4               MUST BE UNPROTECTED                          
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         BZ    *+12                                                             
         TM    1(R6),X'02'                                                      
         BZ    KEYM4                                                            
*                                  SET UP TO LOOP THROUGH OLD KEYS              
         LA    R4,IO                                                            
KEYM2    CLI   0(R4),0             TEST END-OF-(OLD)SCREEN                      
         BE    KEYM4                                                            
         TM    1(R4),X'20'         FIRST MUST BE PROTECTED                      
         BNO   KEYM3                                                            
         CLI   0(R4),9             IF LENGTH IS 1 WE'RE DONE                    
         BE    KEYM4                                                            
         CLC   0(1,R3),0(R4)       TAG LENGTHS MUST MATCH                       
         BNE   KEYM3                                                            
         LR    R1,RF                                                            
         AHI   R1,-9               CONTENTS OF TAG FIELD                        
         TM    1(R3),X'02'            MUST MATCH                                
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,KEYCLC                                                        
         BNE   KEYM3                                                            
*                                                                               
         LA    R5,0(RF,R4)         R5=A(OLD DATA FIELD)                         
         TM    1(R5),X'20'         MUST BE UNPROTECTED                          
         BO    KEYM3                                                            
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         BZ    *+12                                                             
         TM    1(R5),X'02'                                                      
         BZ    KEYM3                                                            
         ZIC   R0,0(R5)            LENGTH OF DATA FIELDS MUST MATCH             
         TM    1(R5),X'02'         DON'T INCLUDE EXTENDED HEADERS               
         BZ    *+8                                                              
         AHI   R0,-8               R0=L'OLD DATA FIELD + IT'S HEADER            
         ZIC   R1,0(R6)                                                         
         TM    1(R6),X'02'                                                      
         BZ    *+8                                                              
         AHI   R1,-8               R1=L'NEW DATA FIELD + IT'S HEADER            
         CR    R0,R1                                                            
         BNE   KEYM3                                                            
*                                                                               
         LR    R3,R6               MOVE IN KEY DATA                             
         LR    R4,R5                                                            
         AHI   R1,-5               MOVEING SOME INDICES AND DATA                
         EX    R1,KEYMVC                                                        
         OI    4(R3),X'80'         TURN ON INPUT THIS TIME                      
         B     KEYM4                                                            
*                                                                               
KEYM3    ZIC   R1,0(R4)            MOVE POINTER TO NEXT OLD FIELD               
         AR    R4,R1                                                            
         B     KEYM2                                                            
*                                                                               
KEYM4    ZIC   R1,0(R3)            MOVE POINTER TO NEXT NEW FIELD               
         AR    R3,R1                                                            
         B     KEYM1                                                            
         SPACE 2                                                                
KEYM5    CLI   0(R3),0             TEST END OF (NEW) SCREEN                     
         BE    KEYM20                                                           
         TM    1(R3),X'20'         IGNORE PROTECTED FIELDS                      
         BZ    *+16                                                             
         CLI   0(R3),9             IF LENGTH IS 1 WE'RE DONE                    
         BE    KEYM20                                                           
         B     KEYM6                                                            
*                                                                               
         TM    1(R3),X'02'         TEST FOR EXTENDED FIELD                      
         BNZ   KEYM7                                                            
*                                                                               
KEYM6    ZIC   RF,0(R3)            BUMP TO NEXT NEW FIELD                       
         AR    R3,RF                                                            
         B     KEYM5               LOOK FOR NEXT UNPROT FIELD                   
*                                                                               
KEYM7    DS    0H                  SET UP TO LOOP THROUGH OLD KEYS              
         LA    R4,IO                                                            
*                                                                               
KEYM8    CLI   0(R4),0             TEST END OF (OLD) SCREEN                     
         BE    KEYM6                                                            
         TM    1(R4),X'20'         IGNORE PROTECTED FIELDS                      
         BZ    *+16                                                             
         CLI   0(R4),9             IF THIS IS 1-BYTE PROT. FIELD                
         BE    KEYM6               GET NEXT FIELD FROM NEW SCREEN               
         B     KEYM10                                                           
*                                                                               
         TM    1(R4),X'02'         EXTENDED FIELDS ONLY                         
         BNZ   KEYM12                                                           
*                                                                               
KEYM10   ZIC   R1,0(R4)            MOVE POINTER TO NEXT OLD FIELD               
         AR    R4,R1                                                            
         B     KEYM8                                                            
*                                                                               
KEYM12   ZIC   RF,0(R3)            FIND NEW FIELD NUMBER                        
         LR    R1,RF               R1=NEW FIELD LENGTH                          
         AR    RF,R3                                                            
         AHI   RF,-8                                                            
         ZIC   RE,0(R4)            FIND OLD FIELD NUMBER                        
         LR    R0,RE               R0=OLD FIELD LENGTH                          
         AR    RE,R4                                                            
         AHI   RE,-8                                                            
         CLC   0(1,RE),0(RF)       DO FIELD NUMBERS MATCH                       
         BNE   KEYM10                                                           
         CR    R1,R0               IF L'NEW FIELD GT L'OLD FIELD                
         BNH   *+6                                                              
         LR    R1,R0               MERGE USING L'OLD FIELD                      
*                                                                               
         SH    R1,=Y(8+4+1)        EX LENGTH OF DATA                            
         EX    R1,KEYMVC                                                        
         OI    4(R3),X'80'         SET INPUT THIS TIME                          
         B     KEYM6               GET NEXT FIELD FROM NEW SCREEN               
*                                                                               
KEYM20   LA    R0,IO               CLEAR SCREEN FROM I/O AREA                   
         LA    R1,L'IO                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
KXIT     J     XIT                                                              
*                                                                               
KEYCLC   CLC   8(0,R3),8(R4)                                                    
KEYMVC   MVC   4(0,R3),4(R4)                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MAINTAIN ACTIVITY ELEMENTS                                       
         SPACE 1                                                                
ADDACTIV NTR1  LABEL=NO                                                         
         CLI   ACTELOPT,C'N'       OPTION NOT TO MONITOR ACTIVITY               
         JE    XIT                                                              
         LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
         J     ACTIV6                                                           
         SPACE 1                                                                
CHAACTIV NTR1  LABEL=NO                                                         
         LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         CLI   ACTELOPT,C'N'       OPTION NOT TO MONITOR ACTIVITY               
         JE    XIT                                                              
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
         CLI   ELEMENT,0                                                        
         JNE   CHAACT2                                                          
         MVI   ACTVEL,X'F1'        REPAIRING IF NO ELEMENT YET                  
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVADID,TWAORIG    AND ID                                       
         MVC   ACTVCHDT,BTODAY                                                  
         SPACE 1                                                                
CHAACT2  LA    R3,ACTVCHID                                                      
         CLC   ACTVCHDT,BTODAY     WAS RECORD CHANGED TODAY                     
         JNE   ACTIV3                                                           
         CLI   ACTVCHNM,0          (ADDED TODAY)                                
         JNE   ACTIV4                                                           
         SPACE 1                                                                
ACTIV3   MVC   ACTVCHDT,BTODAY     NO SO SET TODAYS DATE                        
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
         MVC   ACTVCHRE,CHREASON   AND MOVE IN THE REASON                       
         J     ACTIV6                                                           
         SPACE 1                                                                
ACTIV4   OC    ACTVCHRE,CHREASON                                                
         SPACE 1                                                                
ACTIV6   MVC   0(2,R3),TWAORIG     MOVE IN ID                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
         SPACE 1                                                                
ACTIV8   OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE                       
         JZ    ACTIV10                                                          
         L     R1,ASECBLK          NEW SECURITY BLOCK                           
         USING SECD,R1                                                          
         MVC   ACTVSCID,SECPID     USER'S PERSONAL ID                           
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         DROP  R1                                                               
         SPACE 1                                                                
ACTIV10  GOTO1 ADDELEM                                                          
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE CLEARS FIELD AT R2                                       
         SPACE 1                                                                
CLEARFLD STM   RE,R1,12(RD)                                                     
         SR    R1,R1                                                            
         ICM   R1,1,0(R2)          CALCULATE L'FIELD                            
         JZ    CLEARFLX                                                         
         SHI   R1,9                                                             
         CLI   1(R2),X'FF'         NOP FIELD - CAN'T HAVE EXTENDED HDR          
         JE    *+16                                                             
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         JNO   *+8                                                              
         SHI   R1,8                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)            TEST IF ANYTHING IN FIELD                    
         JZ    CLEARFLX                                                         
         OC    8(0,R2),8(R2)       TEST IF ANYTHING IN FIELD                    
         OI    6(R2),X'80'         TRANSMIT IT                                  
         MVI   5(R2),0             AND SIMULATE NO INPUT LENGTH                 
         BASR  RF,0                                                             
         EX    R1,8(RF)            ELSE GO AHEAD AND CLEAR IT                   
         J     CLEARFLX                                                         
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
CLEARFLX LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
*              LOCAL DSECTS                                                     
         SPACE 2                                                                
CHKD     DSECT                                                                  
SAVEIO   DS    CL4                                                              
SAVRLEN  DS    CL2                                                              
CHKIO    DS    CL2000                                                           
CHKDX    EQU   *                                                                
         SPACE 2                                                                
LISTLD   DSECT                                                                  
LISTSNAM DS    XL2                 NAME (S TYPE)                                
LISTSET  DS    XL1                 LISTSW SETTING                               
         DS    XL1                 N/D - REQURED TO HW ALIGN S-TYPES            
LISTLLQ  EQU   *-LISTLD                                                         
         SPACE 1                                                                
PRTOPTD  DSECT                                                                  
POPTSNAM DS    XL2                 NAME (S TYPE)                                
POPTWHEN DS    XL1                 WHEN SETTING                                 
POPTTWAW DS    XL1                 TWAWHEN SETTING                              
POPTBR   DS    AL4                 BRANCH ADDR NON-SPOOL (0=INVALID)            
POPTSPUL DS    AL4                 DITTO SPOOL-ONLY                             
POPTLQ   EQU   *-PRTOPTD                                                        
         SPACE 1                                                                
SELTABD  DSECT                                                                  
SELSCHR  DS    XL2                 SINGLE CHR (S TYPE)                          
SELSNAM  DS    XL2                 3 CHR EXPANSION (S TYPE)                     
SELTABLQ EQU   *-SELTABD                                                        
         SPACE 1                                                                
ACDD     DSECT                     ACTIVITY DISPLAY SCREEN LINE                 
ACDHDR   DS    XL8                 FIELD HEADER                                 
ACDDATA  DS    0CL70                                                            
ACDTXT1  DS    CL18                                                             
ACDDATE  DS    CL9                                                              
         DS    CL1                                                              
ACDTXT2  DS    CL10                                                             
ACDIDNO  DS    CL4                                                              
         DS    CL1                                                              
ACDID    DS    CL10                                                             
         DS    CL1                                                              
ACDPASS  DS    CL12                                                             
         ORG   ACDDATE                                                          
ACDDSKAD DS    CL8                                                              
         ORG   ACDDATE                                                          
ACDRECL  DS    CL4                                                              
         ORG   ACDDATE                                                          
ACDSECLV DS    CL3                                                              
         ORG   ACDDATE                                                          
ACDCHRSN DS    CL40                                                             
         ORG   ACDTXT1+14                                                       
ACDCHNO  DS    CL3                                                              
         ORG   ACDDATA+L'ACDDATA                                                
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
AENDSYSD DS    A                                                                
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE DDSCANBLKD                                                     
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
*                                                                               
       ++INCLUDE DDMASTD                                                        
*                                                                               
       ++INCLUDE DDACTIVD                                                       
*                                                                               
       ++INCLUDE DDSECURED                                                      
*                                                                               
       ++INCLUDE FAPQPL                                                         
*                                                                               
       ++INCLUDE DDSPOOK                                                        
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE DDGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* GEGENREF                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENREF                                                       
         PRINT ON                                                               
* DMREQHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
* DDFAXINFOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDFAXINFOD                                                     
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* GERFPIOD                                                                      
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
* GEGENRFPD                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENRFPD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038GEGENCON  06/20/17'                                      
         END                                                                    
