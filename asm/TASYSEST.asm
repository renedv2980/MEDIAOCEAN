*          DATA SET TASYSEST   AT LEVEL 060 AS OF 11/22/13                      
*PHASE T00A8CA,*                                                                
*INCLUDE TWABLD                                                                 
*INCLUDE TAINTER                                                                
         TITLE 'T00A8C - TALENT SYSTEM SESSION ESTIMATING'                      
T00A8C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TASYSEST,R8,R7,R6,RR=R3                                        
         LR    RA,R1               RA=A(PARAMETER BLOCK)                        
         USING TEBLOCK,RA                                                       
         ST    R3,TEARELO                                                       
         L     RC,TEAGEND          RC=A(GENCON STORAGE AREA)                    
         USING GEND,RC                                                          
         L     R9,TEAWORKD         R9=A(TALENT SYSTEM WORK AREAS)               
         USING TAWORKD,R9                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         CLI   MODE,RECDEL         IGNORE DELETE                                
         BE    XIT                                                              
         CLI   MODE,RECREST        AND RESTORE MODES                            
         BE    XIT                                                              
*                                                                               
         BAS   RE,INITIAL          ALWAYS INITIALIZE FIRST                      
*                                                                               
         L     R3,ATWA             R3=A(TWA)  (MODE ROUTINES ONLY)              
         USING T702FFD,R3                                                       
         CLI   MODE,SETFILE        GUAR. TO GET THIS MODE DURING SELECT         
         BNE   ES5                                                              
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   ESX                                                              
         CLI   THISLSEL,CHASELQ    AND SELECTED FOR CHANGE                      
         BNE   ESX                                                              
         L     R1,SYSPARMS                                                      
         L     R1,0(R1)                                                         
         USING TIOBD,R1            R1 = A(TRANSLATOR I/O BLOCK)                 
         L     RF,TEAOPTSH                                                      
         S     RF,ATWA                                                          
         STH   RF,TIOBLAST         FORCE GENCON TO THINK A FLD WAS I/P          
         B     ESX                                                              
*                                                                               
ES5      CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     ESX                                                              
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     ESX                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     ESX                                                              
*                                                                               
         CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BNE   ES10                                                             
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         BE    ESX                                                              
         CLI   THISLSEL,C'D'       IF SELECTING FOR DELETE                      
         BE    ESX                 ALWAYS RETURN FOR CONFIRM MESSAGE            
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PARTDISP                                                         
         CLI   ACTNUM,ACTCHA                 OR CHANGE                          
         BE    PARTDISP                                                         
         CLI   ACTNUM,ACTSEL                 OR SELECT                          
         BE    PARTDISP            GIVE MY OWN MESSAGE                          
         B     ESX                                                              
*                                                                               
ES10     CLI   MODE,XRECADD        IF RECORD ADDED                              
         BE    ES20                                                             
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BE    ES20                                                             
         TM    GENSTAT4,CONFDEL    OR IF DIDN'T HAVE TO CONFIRM DELETE          
         BO    ES30                                                             
         CLI   MODE,XRECDEL        AND RECORD DELETED                           
         BNE   ES30                                                             
ES20     BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
         B     ESX                                                              
*                                                                               
ES30     CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   ES60                                                             
         CLI   ACTNUM,ACTSEL       AND ACTION IS SELECT                         
         BE    *+12                                                             
         CLI   TWALACT,ACTSEL      OR LAST ACTION WAS SELECT                    
         BNE   ES40                                                             
         TM    TESTAT,TESTMORE+TESTCHGD & NOTHING CHG'D OR LEFT TO DISP         
         BZ    ESX                 THEN DON'T RE-DISPLAY                        
*                                                                               
ES40     BAS   RE,DISPLAY          ELSE RE-DISPLAY THE RECORD                   
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   ES50                                                             
         CLI   TWALACT,ACTSEL      AND LAST ACTION WASN'T SELECT                
         BE    *+16                                                             
         TM    TESTAT,TESTMORE     AND THERE'S MORE TO DISPLAY                  
         BO    ES50                GIVE PARTIAL MESSAGE                         
         B     ESX                 ELSE GIVE REGULAR MESSAGE                    
         L     R1,EFHACT                                                        
         MVC   8(6,R1),=C'SELECT'  ELSE RETURN TO SELECT NEXT TIME              
ES50     B     PARTDISP            GIVE MY OWN MESSAGE                          
*                                                                               
ES60     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   ESX                                                              
         BAS   RE,PREP                                                          
*                                                                               
ESX      BAS   RE,FINAL            PROCESS FINISHING-UP ROUTINES                
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INITIAL  NTR1                                                                   
         LA    R2,CORETAB          PICK UP A(CORE-RESIDENT ROUTINES)            
         LA    R0,NCORES                                                        
         LA    R4,COREFACS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   4(3,R1),=X'D9000A'                                               
INIT10   MVC   7(1,R1),0(R2)                                                    
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,INIT10                                                        
         SPACE 1                                                                
         L     R2,TASYSVAL         PICK UP A(SYSTEM COMMON ROUTINES)            
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R0,NSYSCOMM                                                      
INIT20   ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,INIT20                                                        
         SPACE 1                                                                
         L     RE,TASYSTAB         PICK UP A(GLOBAL TABLES)                     
         LA    RF,TGTABLES                                                      
         LA    R0,NTABLS                                                        
INIT30   L     R1,TASYSTAB         RELOCATE TABLE ENTRIES                       
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT30                                                        
         SPACE 1                                                                
         LA    R1,PFKTTAB          SET A(APPROPRIATE PFKEY TABLE)               
         CLI   TEMEDIA,TEMEDTV     IF NOT TV                                    
         BE    *+8                                                              
         LA    R1,PFKRTAB          THEN MUST BE RADIO                           
         CLC   TEAOPFKS,ZEROES                                                  
         BE    *+8                                                              
         L     R1,TEAOPFKS         SET TO USE IT                                
         ST    R1,TEAPFKS          SAVE A(PFKEY TABLE)                          
         SPACE 1                                                                
         MVC   TESVSTSV,ASTARTSV   SAVE USER'S A(SAVED STORAGE)                 
         LA    R1,TGD                                                           
         ST    R1,ASTARTSV         SET A(SAVED STORAGE) FOR TAL RTNS            
         SPACE 1                                                                
         MVC   TGAS2ACC,TEAS2ACC   A(STAFF2 AGENCY/CLIENT COMBINATIONS)         
         SPACE 1                                                                
         MVI   USEIO,C'N'                                                       
         MVC   LKEY,=Y(L'TLRCKEY)  DETAILS OF DIRECTORY AND KEY                 
         MVC   TESVLSTA,LSTATUS                                                 
         MVC   LSTATUS,=Y(L'TLRCSTAT)                                           
         MVC   TESVKEY,KEY         SAVE CURRENT KEY                             
         SPACE 1                                                                
         NI    TESTAT,ALL-TESTRDTL CLEAR HAVE READ TALENT FILE                  
         SPACE 1                                                                
         CLC   TETALAGY,ZEROES     IF TALENT AGENCY DEFINED                     
         BE    INIT50                                                           
         CLC   TETALAGY,TESVTAGY   AND IT HAS CHANGED                           
         BNE   INIT40                                                           
         CLC   TETALCLI,TESVTCLI   OR TALENT CLIENT HAS CHANGED                 
         BNE   INIT40                                                           
         CLC   TEPROCLI,TESVPCLI   OR PRODUCTION CLIENT HAS CHANGED             
         BE    INITX                                                            
         SPACE 1                                                                
INIT40   BAS   RE,TALTNH           GET T&H RATE FROM TALENT SYSTEM              
         BAS   RE,INTINIT          INITIALIZE PRODUCTION INTERFACE DATA         
         SPACE 1                                                                
INIT50   MVC   TESVTAGY,TETALAGY   SAVE CURRENT TALENT AGENCY                   
         MVC   TESVTCLI,TETALCLI                       CLIENT                   
         MVC   TESVPCLI,TEPROCLI   SAVE CURRENT PRODUCTION CLIENT               
         SPACE 1                                                                
INITX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS T&H RATE FROM TALENT SYSTEM                         
         SPACE 1                                                                
TALTNH   NTR1                                                                   
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
         XC    TETLTAXR,TETLTAXR   CLEAR TALENT TAX RATE                        
         XC    TETLHNDR,TETLHNDR                HANDLING RATE                   
         XC    TETLEMSR,TETLEMSR                EMS RATE                        
         SPACE 1                                                                
         MVC   TGAGY,TETALAGY      SET TALENT AGENCY                            
         MVC   TGCLI,TETALCLI      AND TALENT CLIENT                            
         SPACE 1                                                                
         CLC   TETALCLI,ZEROES     IF TALENT CLIENT DEFINED                     
         BNE   TTNH10              GO GET IT                                    
*                                                                               
         CLC   TEPROCLI,ZEROES     IF PRODUCTION CLIENT DEFINED                 
         BE    TTNH20                                                           
         MVC   TGCLI(3),TEPROCLI   SET TALENT CLI BASED ON PROD CLI             
         OC    TGCLI,SPACES                                                     
         SPACE 1                                                                
TTNH10   GOTO1 READTAL,DMCB,TLCLCDQ  GET CLIENT RECORD                          
         SPACE 1                                                                
         CLI   ERROR,0             IF IT WAS FOUND                              
         BNE   TTNH20                                                           
         BAS   RE,GETTNH           EXTRACT T&H INFO FROM CLIENT RECORD          
         BE    TTNHX                                                            
         SPACE 1                                                                
TTNH20   GOTO1 READTAL,DMCB,TLAYCDQ  GET AGENCY RECORD                          
         SPACE 1                                                                
         CLI   ERROR,0             IF AGENCY NOT FOUND THEN GIVE ERROR          
         BNE   ERRAGY                                                           
         BAS   RE,GETTNH           ELSE EXTRACT T&H INFO FROM AGENCY            
         SPACE 1                                                                
TTNHX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EXTRACTS T&H INFO FROM TALENT BILLING RULES EL.          
         SPACE 1                                                                
*                                  AIO3 HAS A(TALENT RECORD)                    
GETTNH   NTR1                                                                   
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R4,AIO3                                                          
         USING TLRCD,R4                                                         
         LA    R4,TLRCELEM         R4=A(FIRST ELEMENT)                          
         BAS   RE,FIRSTEL                                                       
         BNE   GTNHNO                                                           
         SPACE 1                                                                
         USING TABRD,R4                                                         
         MVC   TGBTYPE,TABRTYPE    SAVE BILLING TYPE IN GLOBAL                  
         CLI   TABRTYPE,0          IF BILLING TYPE DEFINED                      
         BE    GTNHNO                                                           
         GOTO1 BTYPVAL,DMCB,TABRTYPE  LOOK UP STANDARD RATE CARD                
         SPACE 1                                                                
         CLC   TABRRATE,ZEROES     IF RATE OVERRIDES DEFINED                    
         BE    *+10                                                             
         MVC   TGBSRC,TABRRATE     MOVE RATES TO GLOBAL STORAGE                 
         SPACE 1                                                                
         MVC   TETLTAXR,TGBFUTA    SET FUTA RATE (HIGHEST) AS TAX RATE          
         CLI   TABRTYPE,TABRTY6    IF TYPE 6                                    
         BNE   *+10                                                             
         MVC   TETLTAXR,TGBSUTA    USE SUTA RATE (SESSION RATE)                 
         SPACE 1                                                                
         MVC   TETLHNDR,TGBHAND    SET HANDLING RATE                            
*                                                                               
         CLI   TABRTYPE,TABRTY21   IF TYPE 21                                   
         BE    *+8                                                              
         CLI   TABRTYPE,TABRTY23   IF TYPE 23                                   
         BNE   *+10                                                             
         MVC   TETLHNDR,TGBSUTA    USE SUTA RATE (SESSION RATE)                 
         SPACE 1                                                                
         CLI   TABRTYPE,TABRTYE    IF TYPE E (EMS)                              
         BNE   YES                                                              
         MVC   TETLHNDR,TGBFICA    SET FICA RATE AS HANDLING RATE               
         MVC   TETLEMSR,TGBOFIC    SET EMS RATE AS OVERFICA RATE                
         B     YES                 RETURN CC EQ                                 
         SPACE 1                                                                
GTNHNO   B     NO                  ELSE RETURN CC NE                            
         EJECT                                                                  
*              ROUTINE INITIALIZES PRODUCTION INTERFACE FIELDS                  
         SPACE 1                                                                
INTINIT  NTR1                                                                   
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
         XC    TEIFEL,TEIFEL       CLEAR INTERFACE ELEMENT AREA                 
         SPACE 1                                                                
         MVC   TGAGY,TETALAGY      SET INTERFACE AGENCY                         
         MVC   TGPCLI(3),TEPROCLI                CLIENT                         
         OC    TGPCLI,SPACES                                                    
         GOTO1 READTAL,DMCB,TLIFCDQ  GET INTERFACE RECORD                       
         SPACE 1                                                                
         CLI   ERROR,0             IF INTERFACE RECORD FOUND                    
         BNE   INTIX                                                            
         SPACE 1                                                                
         MVI   ELCODE,TAIFELQ      GET INTERFACE DETAILS ELEMENT                
         L     R4,AIO3                                                          
         USING TLIFD,R4                                                         
         LA    R4,TLIFELEM         R4=A(FIRST ELEMENT)                          
         BAS   RE,FIRSTEL                                                       
         BNE   INTIX                                                            
         USING TAIFD,R4                                                         
         ZIC   R1,TAIFLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEIFEL(0),TAIFEL    SAVE IT IN LOCAL W/S                         
         SPACE 1                                                                
INTIX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO READ A RECORD IN TALENT SYSTEM                        
         SPACE 1                                                                
*                                  P1=RECORD TYPE EQUATE                        
READTAL  NTR1                                                                   
         L     R3,0(R1)            LOAD PARAMETER                               
         SPACE 1                                                                
         BAS   RE,SWTAL            SWITCH TO TALENT SYSTEM (IF NEC.)            
         BNE   ERRSW                                                            
         SPACE 1                                                                
         MVC   SYSDIR,=CL8'TALDIR' SET TALENT FILE OVERRIDES                    
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   AIO,AIO3                                                         
         OI    TGFASTAT,TGFROMFA                                                
         GOTO1 RECVAL,DMCB,(R3),(X'20',0)  GET RECORD                           
         MVC   AIO,TEAIO                                                        
         MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         SPACE 1                                                                
         OI    TESTAT,TESTRDTL     SET HAVE READ TALENT FILE                    
         SPACE 1                                                                
         BAS   RE,SWBACK           SWITCH BACK TO ORIGINAL SYSTEM               
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE 3                                                                
*              ROUTINES TO CONTROL SYSTEM SWITCHING                             
         SPACE 2                                                                
SWTAL    DS    0H                                                               
         MVI   DMCB,TAL1SE         SET TO SWITCH TO TAL1                        
         CLI   SVSYS,C'W'          ACCT?                                        
         BNE   SWSYS                                                            
         MVI   DMCB,TAL2SE         THEN SWITCH TO TAL2                          
         B     SWSYS                                                            
         SPACE 2                                                                
SWBACK   DS    0H                                                               
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         B     SWSYS                                                            
         SPACE 2                                                                
SWSYS    NTR1                                                                   
         CLI   SVSYS,TAL1SE        DON'T BOTHER IF ORIG. SYS IS TALENT          
         BE    SWX                                                              
         CLI   SVSYS,TAL2SE                                                     
         BE    SWX                                                              
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0             RETURN CC                                    
         BNE   NO                                                               
SWX      B     YES                                                              
         EJECT                                                                  
*              ROUTINE HANDLES VALIDATE KEY MODE                                
         SPACE 1                                                                
VKEY     NTR1                                                                   
         BAS   RE,MYINIT           LOCAL INITIALIZATION                         
         SPACE 1                                                                
         CLI   ACTNUM,ACTREP       IF ACTION IS REPORT                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTDWN       IF ACTION IS DOWNLOAD                        
         BNE   VK10                                                             
         L     R1,TEAOVLYH         CLEAR THE SCREEN IN CASE REQUEST IS          
         TWAXC (R1),PROT=Y,CLRINPUTLEN=Y                                        
*                                  FOR OFFLINE SO REQTWA WON'T GRAB             
         SPACE 1                                                                
VK10     CLI   ACTNUM,ACTADD       IF ADDING NEW RECORD                         
         BNE   XIT                                                              
         TM    TESVSTAT,TESTINIT   AND INITIALIZATION WAS REQUESTED             
         BZ    XIT                                                              
         BAS   RE,BLDSCRN          BUILD SCREEN TABLE                           
         L     R2,TEAFRSTH         SET R2=A(FIRST DATA FIELD)                   
         B     PLSENTER            ASK FOR INPUT                                
         SPACE 3                                                                
*              ROUTINE HANDLES DISPLAY KEY MODE                                 
         SPACE 1                                                                
DKEY     NTR1                                                                   
         BAS   RE,MYINIT           LOCAL INITIALIZATION                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PERFORMS LOCAL INITIALIZATION ROUTINES                   
         SPACE 1                                                                
MYINIT   NTR1                                                                   
         MVC   TESVSTAT,TESTAT     SAVE STATUS BYTE AS PASSED BY USER           
         SPACE 1                                                                
         CLI   ACTNUM,ACTREP       IF ACTION IS REPORT                          
         BE    *+12                                                             
         TM    TESTAT,TESTINIT     OR NEED TO (RE-)INITIALIZE                   
         BZ    MYIN10                                                           
         LH    RF,=Y(TECLRLNQ)     CLEAR W/S                                    
         XCEFL TECLEAR,(RF)                                                     
         SPACE 1                                                                
MYIN10   MVI   ELCODE,0            CLEAR ELCODE FOR TABLE LOOPS                 
         SPACE 1                                                                
         MVC   TELINE,TEOLINE      SET OVERRIDE LINE DEFINITION TABLE           
         LA    R4,TELINE                                                        
         BAS   RE,FIRSTEL          FIND END OF OVERRIDE TABLE                   
         B     *+8                                                              
         BAS   RE,NEXTEL                                                        
         BE    *-4                                                              
         MVC   0(DEFLINEQ,R4),DEFLINE  APPEND DEFAULT LINE TABLE AFTER          
         SPACE 1                                                                
         MVC   TEFLD(L'TEOFLD),TEOFLD  SET OVERRIDE FIELD DEF TABLE             
         LA    R4,TEFLD                                                         
         BAS   RE,FIRSTEL          FIND END OF OVERRIDE TABLE                   
         B     *+8                                                              
         BAS   RE,NEXTEL                                                        
         BE    *-4                                                              
         LA    R5,DEFFLDQ                                                       
         LA    R2,DEFFLD                                                        
         LR    R3,R5                                                            
         MVCL  R4,R2               APPEND DEFAULT FIELD TABLE AFTER             
         LA    R1,TEFLD+L'TEFLD                                                 
         CR    R4,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                NEED TO MAKE TEFLD LARGER                    
         SPACE 1                                                                
         GOTO1 MEDVAL,DMCB,TEMEDIA SET GLOBAL MEDIA DETAILS                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS SCREEN TABLE FOR CURRENT RECORD                   
         SPACE 1                                                                
BLDSCRN  NTR1                                                                   
         NI    TESTAT,ALL-TESTTADD-TESTXMIT-TESTMISC  CLEAR SOME BITS           
         SPACE 1                                                                
         L     R2,TEAOVLYH         CALCULATE N'LINES AVAIL. ON SCREEN           
         LH    R1,2(R2)                                                         
         XR    R0,R0                                                            
         D     R0,=F'80'           R1=CURRENT ROW - 1                           
         LA    R0,22                                                            
         SR    R0,R1               SUBTRACT FROM MAX - 1                        
         STC   R0,TELINES          NOW HAVE N'LINES LEFT                        
         SPACE 1                                                                
         BAS   RE,BUMP             BUMP TO 1ST FIELD AFTER OVLY                 
         BNE   *+8                                                              
         OI    TESTAT,TESTXMIT     IF END OF SCRN THEN NEED TO XMIT ALL         
         SPACE 1                                                                
         MVC   TESVSCRN,TESCRN     SAVE CURRENT SCREEN TABLE (FOR ERRS)         
         XC    TESCRN,TESCRN                                                    
         LA    R3,TESCRN           R3=A(SCREEN TABLE)                           
         USING SCRND,R3                                                         
         L     R4,AIO                                                           
         MVC   ELCODE,TEECELCD     SET TO LOOP THROUGH EST. CAST ELS.           
         BAS   RE,GETEL                                                         
         BE    BLDS10                                                           
*                                  * NEW RECORD - SET STANDARD SCREEN *         
         CLI   TEPFTYPE,0          UNLESS PFKEY PRESSED                         
         BE    *+14                                                             
         CLC   PFAID,TEPFCMNT      AND IT WASN'T COMMENT                        
         BNE   BLDS20              IN WHICH CASE GO HANDLE PFKEY                
         SPACE 1                                                                
         MVC   TESCRN,TEOSCRN      SET OVERRIDE DEFINITION TABLE                
         CLC   TESCRN,ZEROES       IF OVERRIDE NOT DEFINED                      
         BNE   BLDS90                                                           
         CLI   TEMEDIA,TEMEDTV     USE DEFAULT TABLE FOR MEDIA                  
         BNE   *+14                                                             
         MVC   TESCRN(DEFSCTVQ),DEFSCTV                                         
         B     *+10                                                             
         MVC   TESCRN(DEFSCRAQ),DEFSCRA                                         
         B     BLDS90                                                           
         SPACE 1                                                                
         USING TAECD,R4            R4=A(ESTIMATE CAST ELEMENT)                  
BLDS10   CLC   SCRNTYPE,TAECTYPE   IF TYPE DIDN'T CHANGE                        
         BNE   *+12                                                             
         LA    R2,1(R2)            BUMP COUNT                                   
         B     BLDS60              AND GET NEXT ELEMENT                         
         SPACE 1                                                                
BLDS20   CLI   SCRNTYPE,0          IF THIS IS NOT FIRST TIME                    
         BE    BLDS40                                                           
         STC   R2,SCRNNUM          SAVE COUNT IN TABLE                          
         SPACE 1                                                                
         MVC   BYTE,SCRNTYPE       SET SEARCH ARGUMENT FOR COMMENT              
         MVI   TEXCSEQ,0           SET TO COUNT ALL ELS. FOR TYPE               
         BAS   RE,GETCMNT          LOOK FOR COMMENT FOR THIS TYPE               
         BNE   *+10                                                             
         MVC   SCRNNUMC,FULL       RETURNS N'LINES REQUIRED IN FULL             
         SPACE 1                                                                
         BAS   RE,ADDLINES         ADD LINES TO TYPE IF PFKEY PRESSED           
         LA    R3,SCRNNEXT         BUMP TO NEXT SCREEN TABLE ENTRY              
         SPACE 1                                                                
BLDS40   BAS   RE,ADDTYPE          ADD NEW TYPE IF NECESSARY                    
         BNE   *+8                                                              
         LA    R3,SCRNNEXT         ADDED NEW TYPE SO BUMP TO NEXT ENTRY         
         SPACE 1                                                                
         CLI   TAECEL,0            IF AT END OF RECORD                          
         BE    BLDS90              THEN DONE                                    
         MVC   SCRNTYPE,TAECTYPE   ELSE SET NEW TYPE IN TABLE                   
         LA    R2,1                START NEW COUNT                              
         SPACE 1                                                                
BLDS60   MVC   ELCODE,TEECELCD     RESET ESTIMATE CAST ELEMENT CODE             
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    BLDS10              LOOP IF FOUND                                
         B     BLDS20              ELSE GO FINISH UP                            
         SPACE 1                                                                
BLDS90   BAS   RE,CALCLNS          CALC. TOTAL N'LINES REQ'D FOR RECORD         
         BAS   RE,BLDTWA           BUILD THE ACTUAL SCREEN                      
         MVC   TESVMED,TEMEDIA     SAVE CURRENT MEDIA                           
         SPACE 1                                                                
         L     R2,TEAOVLYH         FIND A(FIRST UNPROTECTED FIELD)              
         BAS   RE,BUMPUN                                                        
         ST    R2,TEAFRSTH         SAVE IT                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET COMMENT ELEMENTS                                  
         SPACE 1                                                                
*                                  BYTE = COMMENT TYPE                          
*                                  TEXCSEQ = PREV. SEQUENCE NUMBER              
GETCMNT  NTR1                                                                   
         XR    R3,R3               R3=COUNT                                     
         L     R4,AIO                                                           
         MVC   ELCODE,TEXCELCD     SET TO LOOP THROUGH COMMENT ELS.             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETC10   BAS   RE,NEXTEL                                                        
         BNE   GETCX                                                            
         SPACE 1                                                                
         USING TAXCD,R4                                                         
         CLC   TAXCTYPE,BYTE       MATCH ON TYPE                                
         BNE   GETC10                                                           
         CLC   TAXCSEQ,TEXCSEQ     FIND NEXT EL. IN SEQUENCE                    
         BL    GETC10                                                           
         SPACE 1                                                                
         LTR   R3,R3               IF THIS IS FIRST TIME                        
         BNZ   *+8                                                              
         ST    R4,FULL             SAVE A(ELEMENT IN FULL)                      
         SPACE 1                                                                
         LA    R3,1(R3)            BUMP COUNT AND CONTINUE                      
         B     GETC10                                                           
         SPACE 1                                                                
GETCX    LTR   R3,R3               IF NO ELS FOUND                              
         BZ    NO                  RETURN CC NE                                 
         STC   R3,FULL             ELSE RETURN COUNT IN HOB                     
         B     YES                 AND RETURN CC EQ                             
         EJECT                                                                  
*              ROUTINE HANDLES ADDING NEW TYPES BASED ON PFKEY                  
         SPACE 1                                                                
         USING SCRND,R3            R3=A(CURRENT SCREEN TABLE ENTRY)             
         USING TAECD,R4            R4=A(CURRENT EST. CAST ELEMENT)              
ADDTYPE  NTR1                                                                   
         CLI   TEPFTYPE,0          IF PFKEY PRESSED                             
         BE    ADDTNO                                                           
         CLC   PFAID,TEPFCMNT      AND PFKEY WAS NOT COMMENT                    
         BE    ADDTNO                                                           
         TM    TESTAT,TESTTADD     AND IF HAVEN'T ALREADY ADDED TYPE            
         BO    ADDTNO                                                           
         CLI   TAECEL,0            THEN IF AT END OF RECORD                     
         BE    *+14                                                             
         CLC   TEPFTYPE,TAECTYPE   OR IF PFKEY TYPE LT ELEMENT TYPE             
         BNL   ADDTNO                                                           
         MVC   SCRNTYPE,TEPFTYPE   INSERT NEW TYPE                              
         ZIC   R2,TELINES          SET TO INSERT MAX N'LINES                    
         BCTR  R2,0                SUBTRACT 1 FOR HEADS                         
         STC   R2,SCRNNUM          SAVE N'LINES OF THIS TYPE                    
         SPACE 1                                                                
         CLI   TEPFTYPE,TAECTYPM   IF ADDING MISCELLANEOUS                      
         BNE   *+16                                                             
         CLI   TENMISCS,0          AND THERE ARE PRE-DEFINED DESCS.             
         BE    *+8                                                              
         OI    TESTAT,TESTMISC     SET TO ADD DUMMY ELEMENTS TO RECORD          
         SPACE 1                                                                
         OI    TESTAT,TESTTADD     SET HAVE ALREADY ADDED TYPE                  
         B     YES                 RETURN CC EQ                                 
         SPACE 1                                                                
ADDTNO   B     NO                  RETURN CC NE                                 
         EJECT                                                                  
*              ROUTINE HANDLES ADDING LINES TO EXISTING TYPES                   
         SPACE 1                                                                
         USING SCRND,R3            R3=A(CURRENT SCREEN TABLE ENTRY)             
ADDLINES NTR1                                                                   
         CLC   TEPFTYPE,SCRNTYPE   IF PFKEY PRESSED FOR THIS TYPE               
         BNE   ADDLX                                                            
         ZIC   R1,SCRNNUM          CALCULATE N'LINES AVAILABLE FOR TYPE         
         ZIC   R2,SCRNNUMC                                                      
         LA    R1,1(R1,R2)         ADD ONE FOR HEADS                            
         IC    R2,TELINES                                                       
         SR    R2,R1                                                            
         BNP   ERRNOMOR            NO MORE ROOM FOR LINES OF THIS TYPE          
         SPACE 1                                                                
         OI    TESTAT,TESTTADD     SET HAVE ALREADY ADDED TYPE                  
         SPACE 1                                                                
         CLC   PFAID,TEPFCMNT      IF COMMENT PFKEY PRESSED                     
         BNE   ADDL10                                                           
         IC    R1,SCRNNUMC         BUMP N'COMMENT LINES                         
         AR    R1,R2                                                            
         STC   R1,SCRNNUMC                                                      
         B     ADDLX                                                            
         SPACE 1                                                                
ADDL10   IC    R1,SCRNNUM          ELSE BUMP N'LINES                            
         AR    R1,R2                                                            
         STC   R1,SCRNNUM                                                       
         SPACE 1                                                                
ADDLX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CALCULATES N'SCREEN LINES REQUIRED TO DISP. REC.         
         SPACE 1                                                                
CALCLNS  NTR1                                                                   
         XR    R4,R4               INITIALIZE TOTAL                             
         SPACE 1                                                                
         LA    R3,TESCRN           R3=A(SCREEN TABLE)                           
         USING SCRND,R3                                                         
CALCL10  CLI   SCRND,0             IF NOT AT END OF TABLE                       
         BE    CALCLX                                                           
         ZIC   R1,SCRNNUM          ADD N'REGULAR LINES                          
         ZIC   R2,SCRNNUMC         +   N'COMMENT LINES                          
         LA    R1,1(R1,R2)         +   ONE FOR HEADS                            
         AR    R4,R1               BUMP TOTAL                                   
         LA    R3,SCRNNEXT         BUMP TO NEXT TABLE ENTRY                     
         B     CALCL10             LOOP                                         
         SPACE 1                                                                
CALCLX   STC   R4,TENLINES         SAVE TOTAL N'LINES                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD TWA                                             
         SPACE 1                                                                
BLDTWA   NTR1                                                                   
         XC    TESCFLDS,TESCFLDS   CLEAR FIELD NUMBERS ON SCREEN                
         MVI   TESCFIND,0          AND ITS INDEX                                
*                                                                               
         BAS   RE,BSTART           RETURNS R3=START ADDR IN SCREEN TBL          
         USING SCRND,R3            R3=A(SCREEN TABLE ENTRY)                     
*                                                                               
         MVC   TESCSTTY,SCRNTYPE   SAVE TYPE OF FIRST ENTRY                     
         LA    R1,TESCRN                                                        
         SR    R1,R3                                                            
         LCR   R1,R1                                                            
         STH   R1,TESCSTRT         SAVE DISPLACEMENT TO START                   
*                                                                               
         MVC   TEANEXTH,TEAOVLYH   SET A(NEXT AVAILABLE SPOT IN TWA)            
         ZIC   R2,TELINES          R2=N'LINES AVAILABLE ON SCREEN               
*                                                                               
BLDT10   CLI   SCRND,0             IF END OF SCREEN TABLE                       
         BE    BLDT80              THEN FINISH UP                               
*                                                                               
         ZIC   R1,SCRNNUM          R1=N'LINES OF THIS TYPE                      
         ZIC   RE,SCRNNUMC         RE=N'COMMENT LINES OF THIS TYPE              
         LA    R1,1(RE,R1)         ADD 1 FOR HEADINGS                           
         CR    R2,R1               IF NOT ENOUGH ROOM FOR ENTIRE TYPE           
         BL    BLDT80              THEN FINISH UP NOW                           
         SR    R2,R1               ELSE REDUCE N'LINES REMAINING                
*                                                                               
         L     R1,TEANEXTH         SAVE DISPLACEMENT TO NEW TYPE                
         S     R1,ATWA                                                          
         STH   R1,SCRNDISP                                                      
*                                                                               
         L     RE,AIO3             INITIALIZE TWABLD ELEMENT BLOCK              
         LA    RF,1000                                                          
         XCEFL                                                                  
         L     R5,AIO3                                                          
         USING TWAELEMD,R5                                                      
*                                                                               
         MVI   ELCODE,0            CLEAR ELCODE FOR TABLE LOOKUPS               
         LA    R4,TELINE           LOOK UP CURRENT LINE TYPE FOR HEADS          
         USING LINED,R4                                                         
         CLC   LINETYPE,SCRNTYPE   MATCH ON TYPE                                
         BE    *+14                                                             
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT LINE TYPE                  
         BE    *-14                                                             
         DC    H'0'                UNDEFINED LINE TYPE IN SCREEN TABLE          
         MVC   TETYPE,SCRNTYPE     SAVE TYPE                                    
*                                                                               
         BAS   RE,BLDHEAD          BUILD HEADINGS ELEMENT FOR THIS TYPE         
*                                                                               
         ZIC   R0,SCRNNUM          R0=N'LINES OF THIS TYPE                      
         OI    TESTAT,TESTL1ST     SET THIS IS FIRST LINE FOR TYPE              
         BAS   RE,BLDLINE          BUILD REGULAR LINE                           
         NI    TESTAT,ALL-TESTL1ST                                              
         BCT   R0,*-8              LOOP                                         
*                                                                               
         ICM   R0,1,SCRNNUMC       R0=N'COMMENT LINES OF THIS TYPE              
         BZ    *+16                                                             
         BAS   RE,BLDCMNTT         BUILD COMMENT TAG                            
         BAS   RE,BLDCMNT          BUILD COMMENT LINE                           
         BCT   R0,*-4              LOOP                                         
*                                                                               
         GOTO1 GOTWABLD,DMCB,AIO3  BUILD ACTUAL SCREEN FOR TYPE                 
*                                                                               
         LA    R3,SCRNNEXT         BUMP TO NEXT SCREEN TABLE ENTRY              
         B     BLDT10              LOOP                                         
*                                                                               
BLDT80   SH    R3,=Y(SCRNLNQ)                                                   
         MVC   TESCENTY,SCRNTYPE   SAVE TYPE OF LAST ENTRY                      
*                                                                               
         L     RE,AIO3             INITIALIZE TWABLD ELEMENT BLOCK              
         MVC   0(TOTELLNQ,RE),TOTEL                                             
*                                                                               
         L     R1,TEANEXTH         SAVE DISPLACEMENT TO TOTALS LINE             
         S     R1,ATWA                                                          
         STH   R1,TEDSPTOT                                                      
         GOTO1 GOTWABLD,DMCB,AIO3  BUILD TOTALS LINE                            
*                                                                               
         L     R4,AIO3             ADD TOTAL FLDS TO SCREEN FIELD TABLE         
         MVI   ELCODE,0                                                         
BLDT94   TM    TWAEATB-TWAELEMD(R4),X'20'  SKIP PROTECTED FIELDS (TAGS)         
         BO    BLDT96                                                           
         GOTO1 ADDSCFLD,DMCB,TWAEFLD-TWAELEMD(R4)                               
BLDT96   BAS   RE,NEXTEL                                                        
         BE    BLDT94                                                           
*                                                                               
         BAS   RE,BLDPFK           BUILD PFKEY TWABLD ELEMENT                   
*                                                                               
         TM    TESTAT,TESTXMIT     IF WE NEED TO TRANSMIT ENTIRE SCREEN         
         BZ    *+14                                                             
         L     R2,TEANEXTH                                                      
         MVC   1(2,R2),=X'0101'    SET TRANSMIT BITS AT EOS                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS A(1ST SCREEN TABLE ENTRY TO DISPLAY)                
         SPACE 1                                                                
BSTART   NTR1                                                                   
         LA    R3,TESCRN           ASSUME WILL START AT BEGINNING               
         SPACE 1                                                                
         CLI   TEPFTYPE,0          IF NO PFKEY PRESSED                          
         BNE   BST20                                                            
         TM    TESTAT,TESTCHGD     AND IF SCREEN CHANGED                        
         BZ    BST20                                                            
         CLC   TENLINES,TELINES    THEN IF CAN'T FIT ENTIRE RECORD              
         BNH   *+8                                                              
         AH    R3,TESCSTRT         START AT SAME SPOT                           
         B     BST50                                                            
         SPACE 1                                                                
BST20    CLI   SCRND,0             IF NOT AT END OF TABLE                       
         BE    BST50                                                            
         CLI   TEPFTYPE,0          AND IF PFKEY PRESSED                         
         BE    BST40                                                            
         CLC   SCRNTYPE,TEPFTYPE   THEN START SCREEN WITH CORRES. TYPE          
         BE    BST50                                                            
         B     *+14                                                             
BST40    CLC   SCRNTYPE,TESCENTY   ELSE IF PAST LAST TYPE START NOW             
         BH    BST50                                                            
         LA    R3,SCRNNEXT         ELSE BUMP TO NEXT ENTRY                      
         B     BST20               KEEP ON LOOKING                              
         SPACE 1                                                                
BST50    CLI   SCRND,0             IF REACHED END OF TABLE                      
         BNE   *+8                                                              
         LA    R3,TESCRN           SET TO START OVER AT BEGINNING               
         SPACE 1                                                                
         B     XITR3               RETURN R3 =  START ADDRESS                   
         EJECT                                                                  
*              ROUTINE BUILDS TWABLD HEADINGS ELEMENT FOR CURRENT LINE          
         SPACE 1                                                                
         USING LINED,R4            R4=A(CURRENT LINE TABLE ENTRY)               
         USING TWAELEMD,R5         R5=A(AREA FOR TWABLD ELEMENT)                
BLDHEAD  NTR1                                                                   
         LA    R1,TWAEDTA          SET START OF DATA IN TWABLD EL.              
         CLI   LINEXATT,0          IF EXTENDED ATTRIBUTES DEFINED               
         BE    BLDH05                                                           
         MVC   TWAEXATT,LINEXATT   SAVE IN TWABLD EL.                           
         MVI   TWAERLN,TWAERLXA    SET CORRES. STATUS BIT                       
         LA    R1,TWAEDTA2         DATA STARTS LATER                            
         SPACE 1                                                                
BLDH05   LR    R3,R1               R3=A(START OF DATA IN TWABLD EL.)            
         LA    R2,LINEFLDS         R2=A(LIST OF FIELDS FOR THIS LINE)           
         SPACE 1                                                                
BLDH10   CLI   0(R2),0             TEST NO MORE FIELDS FOR THIS LINE            
         BE    BLDH30                                                           
         LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         CLC   FLDID,0(R2)         MATCH ON FIELD ID NUMBER                     
         BE    BLDH20                                                           
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT FIELD TABLE ENTRY          
         BE    *-14                                                             
         DC    H'0'                UNDEFINED FIELD TYPE IN LINE TABLE           
         SPACE 1                                                                
BLDH20   ZIC   RE,FLDHDLEN         RE=L'HEADING                                 
         LTR   RE,RE                                                            
         BZ    BLDH25                                                           
         BCTR  RE,0                -1                                           
         CLI   TETYPE,TAECTYPM     FOR ALL EXCEPT MISC.                         
         BE    BLDH23                                                           
         CLI   FLDID,TE#CCMT       IF THIS IS CAST COMMENT FIELD                
         BNE   *+8                                                              
         LA    R3,78-TE#NETL-1-TE#CCMTL(R1)  FORCE TO RHS                       
         CLI   FLDID,TE#NET        IF THIS IS NET AMOUNT FIELD                  
         BNE   *+8                                                              
         LA    R3,78-TE#NETL(R1)   FORCE TO RHS                                 
BLDH23   EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FLDHEAD     MOVE HEADING TO OUTPUT AREA                  
         SPACE 1                                                                
         LA    RE,1(RE)            RE=L'HEADING                                 
BLDH25   ZIC   RF,FLDIPLEN         RF=L'INPUT FIELD                             
         CR    RE,RF               TAKE LARGER                                  
         BNL   *+6                                                              
         LR    RE,RF                                                            
         CLI   ACTNUM,ACTREP       IF ACTION IS REPORT, DON'T MOVE ^ IN         
         BNE   *+8                                                              
         LA    RE,1(RE)            ADD SPACE BETWEEN FIELDS                     
         AR    R3,RE               BUMP TO NEXT DATA SPOT                       
         CLI   ACTNUM,ACTREP                                                    
         BE    *+12                                                             
         MVI   0(R3),C'^'          SPACE OUT FIELDS FOR EMULATOR                
         LA    R3,1(R3)            BUMP TO NEXT DATA SPOT                       
         SPACE 1                                                                
         LA    R2,1(R2)            BUMP TO NEXT FIELD FOR LINE                  
         B     BLDH10              LOOP                                         
         SPACE 1                                                                
BLDH30   MVI   TWAELCD,1           BUILD REST OF TWABLD ELEMENT                 
         LR    RF,R3                                                            
         LA    RE,TWAELCD+1                                                     
         SR    RF,RE                                                            
         STC   RF,TWAELLN          L'ELEMENT                                    
         LA    R1,1(R1)                                                         
         SR    R3,R1               CALC L'FIELD                                 
         CH    R3,=H'79'           STARTING IN COL 2, SO MAX LEN=79             
         BNH   *+8                                                              
         LH    R3,=H'79'                                                        
         STC   R3,TWAEFLN          L'FIELD                                      
         MVI   TWAECOL,2           START IN COLUMN 2                            
         MVI   TWAEATB,X'28'       SET PROTECTED/HIGHLIGHTED                    
         AR    R5,RF               BUMP TO NEXT POSITION IN ELEMENT TAB         
         B     XITR5               RETURN NEW R5                                
         EJECT                                                                  
*              ROUTINE BUILDS TWABLD FIELD ELEMENTS FOR CURRENT LINE            
         SPACE 1                                                                
         USING LINED,R4            R4=A(CURRENT LINE TABLE ENTRY)               
         USING TWAELEMD,R5         R5=A(AREA FOR TWABLD ELEMENT)                
BLDLINE  NTR1                                                                   
         LA    R2,LINEFLDS         R2=A(LIST OF FIELDS FOR THIS LINE)           
         LA    R3,2                R3=COLUMN NUMBER OF CURRENT FIELD            
         SPACE 1                                                                
BLDL10   CLI   0(R2),0             TEST NO MORE FIELDS FOR THIS LINE            
         BE    BLDLX                                                            
         LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         CLC   FLDID,0(R2)         MATCH ON FIELD ID NUMBER                     
         BE    BLDL20                                                           
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT FIELD TABLE ENTRY          
         BE    *-14                                                             
         DC    H'0'                UNDEFINED FIELD TYPE IN LINE TABLE           
         SPACE 1                                                                
BLDL20   CLI   FLDHDLEN,0          IF THIS IS DUMMY FIELD, SKIP                 
         BE    BLDL30                                                           
         MVI   TWAELCD,1           ELSE BUILD TWABLD ELEMENT                    
         MVI   TWAELLN,TWAELLNQ    L'ELEMENT                                    
         SPACE 1                                                                
         CLI   TETYPE,TAECTYPM     FOR ALL EXCEPT MISC.                         
         BE    BLDL25                                                           
         CLI   FLDID,TE#CCMT       IF THIS IS CAST COMMENT FIELD                
         BNE   *+8                                                              
         LA    R3,80-TE#NETL-1-TE#CCMTL  FORCE TO RHS                           
         CLI   FLDID,TE#NET        IF THIS IS NET AMOUNT FIELD                  
         BNE   *+8                                                              
         LA    R3,80-TE#NETL       FORCE TO RHS                                 
         SPACE 1                                                                
BLDL25   STC   R3,TWAECOL          START COLUMN                                 
         LR    RF,R3                                                            
         ZIC   R1,FLDIPLEN         L'FIELD                                      
         AR    RF,R1                                                            
         SH    RF,=H'80'           ENSURE FIELD DOESN'T GO PAST EOL             
         BNP   *+6                                                              
         SR    R1,RF                                                            
         STC   R1,TWAEFLN                                                       
         SPACE 1                                                                
         TM    TESTAT,TESTL1ST     IF THIS IS FIRST LINE FOR TYPE               
         BZ    *+10                                                             
         MVC   TWAEFLD,FLDID       FIELD ID NUMBER                              
         SPACE 1                                                                
         GOTO1 ADDSCFLD,DMCB,FLDID ADD TO LIST OF SCREEN FIELDS                 
         SPACE 1                                                                
         LA    R5,TWAELLNQ(R5)     BUMP TO NEXT POSITION IN ELEMENT TAB         
         SPACE 1                                                                
BLDL30   ZIC   RE,FLDHDLEN         RE=L'HEADING                                 
         ZIC   RF,FLDIPLEN         RF=L'INPUT FIELD                             
         CR    RE,RF               TAKE LARGER                                  
         BNL   *+6                                                              
         LR    RE,RF                                                            
         LA    RE,1(RE)            ADD SPACE BETWEEN FIELDS                     
         AR    R3,RE               SET NEXT COLUMN                              
         SPACE 1                                                                
         LA    R2,1(R2)            BUMP TO NEXT FIELD FOR LINE                  
         B     BLDL10              LOOP                                         
         SPACE 1                                                                
BLDLX    B     XITR5               RETURN NEW R5                                
         EJECT                                                                  
*              ROUTINE BUILDS TWABLD FIELD ELEMENT FOR COMMENT TAG              
         SPACE 1                                                                
         USING TWAELEMD,R5         R5=A(AREA FOR TWABLD ELEMENT)                
BLDCMNTT NTR1                                                                   
         LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         CLI   FLDID,TE#CMNT       LOOK UP COMMENT FIELD                        
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         DC    H'0'                COMMENT FIELD NOT DEFINED                    
         SPACE 1                                                                
         ZIC   RE,FLDHDLEN         RE=L'HEADING                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),FLDHEAD  MOVE HEADING TO OUTPUT AREA                  
         SPACE 1                                                                
         MVI   TWAELCD,1           BUILD TWABLD ELEMENT                         
         LA    RE,TWAELLNQ+1(RE)                                                
         STC   RE,TWAELLN          L'ELEMENT                                    
         MVI   TWAECOL,10          START COLUMN                                 
         MVC   TWAEFLN,FLDHDLEN    L'FIELD                                      
         MVI   TWAEATB,X'20'       SET PROTECTED                                
         SPACE 1                                                                
         AR    R5,RE               BUMP TO NEXT POSITION IN ELEMENT TAB         
         B     XITR5               RETURN NEW R5                                
         SPACE 3                                                                
*              ROUTINE BUILDS TWABLD FIELD ELEMENT FOR COMMENT LINE             
         SPACE 1                                                                
         USING TWAELEMD,R5         R5=A(AREA FOR TWABLD ELEMENT)                
BLDCMNT  NTR1                                                                   
         LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         CLI   FLDID,TE#CMNT       LOOK UP COMMENT FIELD                        
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         DC    H'0'                COMMENT FIELD NOT DEFINED                    
         SPACE 1                                                                
         MVI   TWAELCD,1           BUILD TWABLD ELEMENT                         
         MVI   TWAELLN,TWAELLNQ    L'ELEMENT                                    
         ZIC   RE,FLDHDLEN                                                      
         LA    RE,10+1(RE)                                                      
         STC   RE,TWAECOL          START COLUMN                                 
         MVC   TWAEFLN,FLDIPLEN    L'FIELD                                      
         MVC   TWAEFLD,FLDID       FIELD ID NUMBER                              
         GOTO1 ADDSCFLD,DMCB,FLDID ADD TO LIST OF SCREEN FIELDS                 
         LA    R5,TWAELLNQ(R5)     BUMP TO NEXT POSITION IN ELEMENT TAB         
         B     XITR5               RETURN NEW R5                                
         EJECT                                                                  
*              ROUTINE BUILDS TWABLD ELEMENT FOR PFKEY LINE                     
         SPACE 1                                                                
BLDPFK   NTR1                                                                   
         MVI   ELCODE,0            CLEAR ELCODE FOR TABLE LOOKUP                
         SPACE 1                                                                
         LA    R5,ELEMENT                                                       
         USING TWAELEMD,R5         R5=A(AREA FOR TWABLD ELEMENT)                
         XC    ELEMENT,ELEMENT                                                  
         SPACE 1                                                                
         L     R4,TEAPFKS          R4=A(PFKEY TABLE)                            
         USING PFKD,R4                                                          
         MVC   TWAEDTA2(2),=C'PF'  SET START OF LINE DATA                       
         LA    R3,TWAEDTA2+2       R3=A(NEXT AVAILABLE SPOT FOR DATA)           
         SPACE 1                                                                
BLDP10   LA    RF,PFKDSP                                                        
         CLI   PFKTYPE,PFKTYPU     OK IF USER-DEFINED TABLE ENTRY               
         BE    BLDP14                                                           
         ZIC   RF,PFKDSP                                                        
         LA    RF,TEPFKEYS(RF)     RF=A(PFKEY DEFINITION IN W/S)                
BLDP14   CLI   0(RF),0                                                          
         BE    BLDP20              IGNORE IF NO PFKEY NUMBER DEFINED            
         SPACE 1                                                                
         EDIT  (1,0(RF)),(2,(R3)),ALIGN=LEFT  DISPLAY PFKEY NUMBER              
         AR    R3,R0                                                            
         MVI   0(R3),C'='                                                       
         ZIC   RE,PFKLEN                                                        
         SH    RE,=Y(PFKLNQ+1)     RE=L'LITERAL-1                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),PFKLIT      MOVE LITERAL TO OUTPUT AREA                  
         SPACE 1                                                                
         LA    R3,3(RE,R3)         BUMP TO NEXT DATA SPOT                       
         CLC   TEAOPFKS,ZEROES     IF OVERRIDE TABLE NOT DEFINED                
         BNE   *+8                                                              
         LA    R3,1(R3)            BUMP ADDL CHAR BETWEEN DESCRIPTIONS          
         SPACE 1                                                                
BLDP20   BAS   RE,NEXTEL           BUMP TO NEXT PFKEY TABLE ENTRY               
         BE    BLDP10              LOOP                                         
         SPACE 1                                                                
         MVI   TWAELCD,1           BUILD REST OF TWABLD ELEMENT                 
         LA    R1,TWAEDTA2+1                                                    
         CLC   TEAOPFKS,ZEROES     IF OVERRIDE TABLE NOT DEFINED                
         BNE   *+6                                                              
         BCTR  R3,0                ACCOUNT FOR ADDL CHAR BETWEEN DESCS          
         SR    R3,R1               R3=L'FIELD                                   
         LA    RF,TWAELLNQ+TWAELLQ2(R3)  RF=L'ELEMENT                           
         STC   RF,TWAELLN          L'ELEMENT                                    
         MVI   TWAECOL,2           START IN COLUMN 2                            
         STC   R3,TWAEFLN          L'FIELD                                      
         MVI   TWAEATB,X'20'       SET PROTECTED                                
         MVI   TWAERLN,TWAERLXA    SET EXTENDED ATTRIBUTES DEFINED              
         MVI   TWAEXATT,FXATHREV+FXATCRED  SET REVERSE VIDEO / COLOR            
         SPACE 1                                                                
         L     R4,TEANEXTH         SAVE A(FIELD) FOR DISPLAY BELOW              
         SPACE 1                                                                
         GOTO1 GOTWABLD,DMCB,(R5)  BUILD PFKEY LINE                             
         SPACE 1                                                                
         L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         CLI   CONSERV+1,C'*'      ONLY IF YOU KNOW HOW                         
         BNE   BLDPX                                                            
         L     R2,TEANEXTH         DISPLAY L'TWA USED                           
         SR    R2,R3                                                            
         STH   R2,HALF                                                          
         GOTO1 HEXOUT,DMCB,HALF,8(R4),2,0  DISPLAY IN PFKEY LINE                
         SPACE 1                                                                
BLDPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PASSES CONTROL TO TWABLD ROUTINE TO BUILD SCREEN         
         SPACE 1                                                                
*                                  P1=A(TWABLD ELEMENTS)                        
GOTWABLD NTR1                                                                   
         L     R3,0(R1)                                                         
         LA    R2,PARAS                                                         
         USING TWAPARMD,R2                                                      
         MVC   TWAPATWA,ATWA       A(TWA)                                       
         ST    R3,TWAPAFST         A(ELEMENTS)                                  
         MVC   TWAPAOUT,TEANEXTH   A(FIRST FIELD)                               
         OI    TWAPINDS,TWAPIFLD   SET TO TRANSMIT BY FIELD                     
         GOTOR =V(TWABLD),PARAS,RR=TEARELO                                      
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEANEXTH,TWAPANXT   RETURN A(NEXT FIELD)                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE ADDS FIELD ID NUMBER TO SCREEN FIELD TABLE               
         SPACE 1                                                                
*                                  P1=A(FIELD ID NUMBER)                        
ADDSCFLD NTR1                                                                   
         L     R2,0(R1)                                                         
         CLI   TESCFIND,L'TESCFLDS INSURE WE DON'T EXCEED MAX                   
         BNH   *+6                                                              
         DC    H'0'                INCREASE SIZE OF TESCFLDS                    
         SPACE 1                                                                
         ZIC   R1,TESCFIND                                                      
         LA    RF,TESCFLDS(R1)     RF=A(NEXT SPOT IN TABLE)                     
         MVC   0(1,RF),0(R2)       MOVE IN FIELD ID NUMBER                      
         LA    R1,1(R1)            BUMP INDEX                                   
         STC   R1,TESCFIND                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY RECORD                                        
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BAS   RE,BLDSCRN          BUILD SCREEN TABLE BASED ON RECORD           
         BAS   RE,DISOPTS          DISPLAY OPTIONS FIELD                        
         SPACE 1                                                                
         TM    TESTAT,TESTMISC     IF NEED TO ADD DUMMY MISC EST ELS.           
         BZ    *+8                                                              
         BAS   RE,ADDMISCS         GO DO IT                                     
         SPACE 1                                                                
         BAS   RE,DSTART           SET A(1ST ELEMENT TO DISPLAY)                
         BZ    DISX                                                             
         USING TAECD,R5            RETURNS R5=A(1ST ELEMENT)                    
         SPACE 1                                                                
         MVI   TELASTTY,0          CLEAR SAVED TYPE                             
         XC    TEELSTRT,TEELSTRT         SAVED START                            
         L     R2,TEAFRSTH         R2=A(FIRST FIELD)                            
         LA    R3,TESCFLDS         R3=A(FIELD ID TABLE FOR SCREEN)              
         BAS   RE,NEWLINE          GO TO NEW LINE RTN TO SET LINE TYPE          
         SPACE 1                                                                
DIS04    CLC   TAECTYPE,TETYPE     IF EL TYPE DOESN'T MATCH LINE TYPE           
         BE    DIS10                                                            
         CLC   ACURFORC,ZEROES     THEN IF NOT ALREADY SET                      
         BNE   *+8                                                              
         ST    R2,ACURFORC         SET TO PLACE CURSOR HERE FOR USER            
         SPACE 1                                                                
DIS08    CLI   0(R3),TE#CMNT       IF AT COMMENT FIELD                          
         BE    DIS14               GO DISPLAY                                   
         BAS   RE,BUMPUN           BUMP TO NEXT DISPLAY FIELD                   
         BE    DISX                                                             
         LA    R3,1(R3)            BUMP FIELD ID NUMBER POINTER                 
         BAS   RE,NEWLINE          IF WE DIDN'T REACH NEXT LINE YET             
         BNE   DIS08               THEN BUMP TO NEXT FIELD                      
         B     DIS04               SEE IF NEW LINE IS CORRECT TYPE              
         SPACE 1                                                                
DIS10    CLC   TETYPE,TELASTTY     NEW LINE - IF TYPE CHANGED                   
         BE    DIS12                                                            
         MVC   TELASTTY,TETYPE     SET NEW TYPE                                 
         MVI   TEXCSEQ,0           RESTART COMMENT SEQUENCE NUMBER              
         SPACE 1                                                                
         CLI   TETYPE,TAECTYPM     IF NEW TYPE IS MISCELLANEOUS                 
         BNE   DIS12                                                            
         TM    TESTAT,TESTMISC     AND JUST ADDED DUMMY ELEMENTS                
         BZ    DIS12                                                            
         LR    R0,R2               THEN SAVE A(CURRENT FIELD)                   
         BAS   RE,BUMP             BUMP TO NEXT                                 
         ST    R2,ACURFORC         INSERT CURSOR HERE                           
         LR    R2,R0               RESTORE A(CURRENT FIELD)                     
         SPACE 1                                                                
DIS12    CLC   TEELSTRT,ZEROES     IF NOT SAVED YET                             
         BNE   *+10                                                             
         MVC   TEELSTRT,TAECTYPE   SAVE START TYPE AND SEQUENCE                 
         MVC   TEELEND,TAECTYPE    SAVE END TYPE AND SEQUENCE                   
         MVI   TAECEL,X'FF'        SET ELEMENT HAS BEEN PROCESSED               
         SPACE 1                                                                
DIS14    LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         MVI   ELCODE,0                                                         
         CLC   FLDID,0(R3)         MATCH ON FIELD ID NUMBER                     
         BE    DIS20                                                            
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT FIELD TABLE ENTRY          
         BE    *-14                                                             
         DC    H'0'                UNDEFINED FIELD TYPE IN LINE TABLE           
         SPACE 1                                                                
DIS20    LH    RF,FLDDDIS          RF=A(DISPLAY RTN. FOR CURRENT FIELD)         
         LTR   RF,RF                                                            
         BZ    DIS35                                                            
         AR    RF,RB                                                            
         LA    RE,DIS30                                                         
         NTR1  ,                                                                
         BR    RF                  DISPLAY THE FIELD                            
         SPACE 1                                                                
DIS30    ST    R2,TEALASTH         SAVE A(LAST FIELD DISPLAYED)                 
         OI    4(R2),X'20'         SET PREV. VALIDATED                          
         SPACE 1                                                                
DIS35    BAS   RE,BUMPUN           BUMP TO NEXT DISPLAY FIELD                   
         BE    DIS40                                                            
         LA    R3,1(R3)            BUMP FIELD ID NUMBER POINTER                 
         BAS   RE,NEWLINE          IF WE'RE NOT STARTING NEW LINE               
         BNE   DIS14               THEN DISPLAY NEXT FIELD                      
         CLI   TAECEL,TAECELQ      ELSE IF CURRENT EL. HASN'T BEEN PROC         
         BE    DIS04               THEN GO DISPLAY IT                           
         MVC   ELCODE,TEECELCD                                                  
         BAS   RE,NEXTEL2          ELSE GET NEXT ELEMENT                        
         BE    DIS04               AND GO DISPLAY IT                            
         CLC   ACURFORC,ZEROES     NO MORE ELS - IF NOT ALREADY SET             
         BNE   *+8                                                              
         ST    R2,ACURFORC         PLACE CURSOR HERE                            
         BAS   RE,BMPTOCMT         BUMP TO COMMENT FIELD (IF PRESENT)           
         BE    DIS14               FOUND IT - GO DISPLAY                        
         SPACE 1                                                                
DIS40    CLI   TAECEL,0            UNLESS ALREADY AT END OF RECORD              
         BE    DISX                                                             
         MVC   ELCODE,TEECELCD     REACHED END OF SCREEN                        
         BAS   RE,NEXTEL2          SET A(NEXT ELEMENT IF EXISTS)                
         SPACE 1                                                                
DISX     BAS   RE,DISTOTS          DISPLAY RECORD TOTALS                        
         SPACE 1                                                                
         NI    TESTAT,ALL-TESTCHGD-TESTMORE  CLEAR CHANGED/MORE BITS            
         SPACE 1                                                                
         LTR   R5,R5               IF NO ELEMENTS FOUND                         
         BNZ   *+14                                                             
         MVC   ACURFORC,TEAFRSTH   FORCE CURSOR TO FIRST FIELD                  
         B     YES                 AND RETURN CC EQ                             
         SPACE 1                                                                
         CLI   TAECEL,0            IF REACHED END OF RECORD                     
         BE    YES                 RETURN CC EQ                                 
         SPACE 1                                                                
         OI    TESTAT,TESTMORE     ELSE SET STATUS - MORE TO COME               
         B     NO                  AND RETURN CC NE                             
         EJECT                                                                  
*              ROUTINE DISPLAYS ESTIMATE OPTIONS ELEMENT                        
         SPACE 1                                                                
DISOPTS  NTR1                                                                   
         L     R2,TEAOPTSH         R2=A(OPTIONS FIELD)                          
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR IT                                     
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVC   ELCODE,TEEOELCD     GET ESTIMATE OPTIONS ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DOPTX                                                            
         USING TAEOD,R4            R4=A(ESTIMATE OPTIONS ELEMENT)               
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(OUTPUT AREA)                            
         XR    R2,R2               R2=COUNT                                     
         SPACE 1                                                                
         USING OPTD,R5                                                          
         LA    R5,OPTTAB           R5=A(OPTION TABLE ENTRY)                     
DOPT20   CLI   OPTD,X'FF'                                                       
         BE    DOPT90              END-OF-TABLE                                 
         XC    0(20,R3),0(R3)                                                   
         LH    RF,OPTDDIS                                                       
         AR    RF,RB               RF=A(DISPLAY ROUTINE)                        
         LA    RE,DOPT30           RE=A(RETURN FOR COMMON NTR1)                 
         NTR1  ,                                                                
         BR    RF                  ** DISPLAY OPTION **                         
DOPT30   BNE   DOPT40                                                           
         MVC   0(1,R3),OPTLHS      DISPLAY 1 CHAR OF LHS                        
         LA    R3,20(R3)           BUMP BLOCK                                   
         LA    R2,1(R2)            BUMP COUNT                                   
         SPACE 1                                                                
DOPT40   LA    R5,OPTNEXT          GET NEXT OPTION                              
         B     DOPT20                                                           
         SPACE 1                                                                
DOPT90   GOTO1 UNSCAN,DMCB,((R2),BLOCK),TEAOPTSH,0  UNSCAN THE BLOCK            
         SPACE 1                                                                
DOPTX    B     XIT                                                              
         EJECT                                                                  
*              OPTION DISPLAY SUBSIDIARY ROUTINES                               
         SPACE 1                                                                
*                                  R3=A(UNSCAN BLOCK ENTRY)                     
         USING TAEOD,R4            R4=A(ESTIMATE OPTIONS ELEMENT)               
         SPACE 1                                                                
DISPNHR  DS    0H                  PENSION AND HEALTH RATE                      
         MVC   HALF,TAEOPNHR                                                    
         B     DISRATE                                                          
         SPACE 1                                                                
DISTNHR  DS    0H                  TAX AND HANDLING RATE                        
         MVC   HALF,TAEOTNHR                                                    
         B     DISRATE                                                          
         SPACE 1                                                                
DISTAXR  DS    0H                  TAX RATE                                     
         MVC   HALF,TAEOTAXR                                                    
         B     DISRATE                                                          
         SPACE 1                                                                
DISHNDR  DS    0H                  HANDLING RATE                                
         MVC   HALF,TAEOHNDR                                                    
         B     DISRATE                                                          
         SPACE 1                                                                
DISYEAR  DS    0H                  CONTRACT YEAR                                
         CLI   TAEOYEAR,0                                                       
         BE    NO                                                               
         GOTO1 YRVAL,DMCB,(X'80',TAEOYEAR)                                      
         MVC   10(L'TGYRCDE,R3),TGYRCDE                                         
         B     YES                                                              
         SPACE 1                                                                
DISUSE   DS    0H                  USE CODE                                     
         CLC   TAEOUSE,ZEROES                                                   
         BE    NO                                                               
         GOTO1 USEVAL,DMCB,TAEOUSE,TAEOUTYP                                     
         MVC   10(L'TGUSCDE,R3),TGUSCDE                                         
         MVC   10+L'TGUSCDE(L'TGUSTYCD,R3),TGUSTYCD                             
         B     YES                                                              
         SPACE 1                                                                
DISADST  DS    0H                  ADDENDUM STATE                               
         CLC   TAEOADST,ZEROES                                                  
         BE    NO                                                               
         MVC   10(L'TAEOADST,R3),TAEOADST                                       
         B     YES                                                              
         SPACE 1                                                                
DISMULT  DS    0H                  MULTIPLIER                                   
         LH    RF,TAEOMULT                                                      
         LTR   RF,RF                                                            
         BZ    NO                                                               
         LA    R2,10-8(R3)                                                      
         BAS   RE,EDITAMT6                                                      
         B     YES                                                              
         SPACE 1                                                                
DISAFM   DS    0H                  AFM RATE                                     
         CLI   TAEOAFM,0                                                        
         BE    NO                                                               
         MVC   10(1,R3),TAEOAFM                                                 
         B     YES                                                              
         SPACE 1                                                                
DISRATE  DS    0H                  DISPLAY RATE OPTION                          
         LH    RF,HALF             HALF = RATE (2 DEC. PLACES)                  
         TM    HALF,X'80'          TEST IF INPUT ZERO                           
         BZ    *+10                                                             
         XR    RF,RF               MAKE RF=0                                    
         B     *+10                AND DISPLAY IT                               
         LTR   RF,RF                                                            
         BZ    NO                  ELSE DON'T DISPLAY ZERO                      
         LA    R2,10-8(R3)                                                      
         BAS   RE,EDITAMT6         DISPLAY RATE                                 
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE ADDS PRE-DEFINED DUMMY MISC. ESTIMATE CAST ELS.          
         SPACE 1                                                                
ADDMISCS NTR1                                                                   
         L     R2,TEAMISCS         R2=A(LIST OF DESCRIPTIONS)                   
         ZIC   R3,TENMISCS         R3=N'PRE-DEFINED DESCRIPTIONS                
         XR    R4,R4               R4=SEQUENCE NUMBER                           
         SPACE 1                                                                
         LA    R5,ELEMENT          R5=A(ESTIMATE CAST ELEMENT)                  
         USING TAECD,R5                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVC   TAECEL,TEECELCD     ELEMENT CODE                                 
         MVI   TAECLEN,TAECLNQ     ELEMENT LENGTH                               
         MVI   TAECTYPE,TAECTYPM   MISCELLANEOUS TYPE                           
         SPACE 1                                                                
ADDM10   STC   R4,TAECSEQ          SET SEQUENCE NUMBER                          
         MVC   TAECDESC,0(R2)      PRE-DEFINED DESCRIPTION                      
         BAS   RE,MYADDL           ADD THE ELEMENT                              
         SPACE 1                                                                
         LA    R4,1(R4)            BUMP SEQUENCE NUMBER                         
         LA    R2,L'TAECDESC(R2)   AND PRE-DEFINED DESCRIPTION                  
         BCT   R3,ADDM10           LOOP                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS A(1ST ELEMENT TO DISPLAY)                           
         SPACE 1                                                                
DSTART   NTR1                                                                   
         L     R5,AIO                                                           
         MVC   ELCODE,TEECELCD     GET ESTIMATE CAST ELEMENTS                   
         BAS   RE,GETEL2                                                        
         BE    *+10                                                             
         XR    R5,R5                                                            
         B     DSTX                                                             
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
         LR    R0,R5               SAVE A(FIRST ELEMENT)                        
         SPACE 1                                                                
         CLI   TEPFTYPE,0          IF PFKEY PRESSED                             
         BE    DST20                                                            
         CLC   TEPFTYPE,TAECTYPE   START AT CORRESPONDING TYPE                  
         BNH   DSTX                                                             
         BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BE    *-14                                                             
         B     DST40                                                            
         SPACE 1                                                                
DST20    TM    TESTAT,TESTCHGD     ELSE IF SCREEN CHANGED                       
         BZ    DST30                                                            
         CLC   TENLINES,TELINES    AND CAN FIT ENTIRE RECORD ON SCREEN          
         BNH   DSTX                THEN DISPLAY FROM BEGINNING                  
         SPACE 1                                                                
         CLC   TAECTYPE,TESCSTTY   ELSE SET TO RESTART AT SAME SPOT             
         BNL   DSTX                                                             
         BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BE    *-14                                                             
         B     DST40                                                            
         SPACE 1                                                                
DST30    CLC   TEELEND,ZEROES      ELSE IF DON'T HAVE PREVIOUS END              
         BE    DSTX                START DISPLAY AT BEGINNING                   
         SPACE 1                                                                
         CLC   TAECTYPE,TEELEND    ELSE START AFTER SAVED END                   
         BH    DSTX                                                             
         BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BE    *-14                                                             
         SPACE 1                                                                
DST40    LR    R5,R0               REACHED EOR - START AGAIN                    
         SPACE 1                                                                
DSTX     LTR   R5,R5               SET CC                                       
         B     XITR5               RETURN R5=A(ELEMENT)                         
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES                                      
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST DETAILS ELEMENT)          
DISNUM   DS    0H                                                               
         LA    RF,TAECNUM          NUMBER'CAST                                  
         BAS   RE,EDIT3                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISOV    DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,3,TAECOV1        OVERSCALE RATE                               
         BZ    *+8                                                              
         BAS   RE,EDITAMT6                                                      
         B     XIT                                                              
         SPACE 1                                                                
DISSPT   DS    0H                                                               
         LA    RF,TAECSP           SPOTS                                        
         BAS   RE,EDIT1                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISDAY   DS    0H                                                               
         LA    RF,TAECDAY          DAYS                                         
         BAS   RE,EDIT1                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISOT    DS    0H                                                               
         LA    RF,TAECOT           OVERTIME                                     
         BAS   RE,EDIT1                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISDT    DS    0H                                                               
         LA    RF,TAECDT           DOUBLETIME                                   
         BAS   RE,EDIT1                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISTRV   DS    0H                                                               
         LA    RF,TAECTRV          TRAVEL TIME                                  
         BAS   RE,EDIT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISPDW   DS    0H                                                               
         LA    RF,TAECPDW          PRIOR-DAY WARDROBE                           
         BAS   RE,EDIT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
DISHRM   DS    0H                                                               
         LA    RF,TAECHRM          HOURS/MINUTES                                
         BAS   RE,EDIT2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING FLDD,R4             R4=A(FIELD TABLE ENTRY)                      
         USING TAECD,R5            R5=A(ESTIMATE CAST DETAILS ELEMENT)          
DISNET   DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         TM    TAECSTAT,TAECSNET   IF AMOUNT IS OVERRIDE                        
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         L     RF,TAECNET          NET AMOUNT                                   
         B     DISAMT              GO DISPLAY                                   
         SPACE 2                                                                
DISPNH   DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         TM    TAECSTAT,TAECSPNH   IF AMOUNT IS OVERRIDE                        
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         L     RF,TAECPNH          PENSION AND HEALTH                           
         B     DISAMT              GO DISPLAY                                   
         SPACE 2                                                                
DISHNW   DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         TM    TAECSTAT,TAECSHNW   IF AMOUNT IS OVERRIDE                        
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         L     RF,TAECHNW          HEALTH AND WELFARE                           
         B     DISAMT              GO DISPLAY                                   
         SPACE 2                                                                
DISTAX   DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         TM    TAECSTAT,TAECSTAX   IF AMOUNT IS OVERRIDE                        
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         L     RF,TAECTAX          TAX                                          
         B     DISAMT              GO DISPLAY                                   
         SPACE 2                                                                
DISHND   DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         TM    TAECSTAT,TAECSHND   IF AMOUNT IS OVERRIDE                        
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         L     RF,TAECHND          HANDLING                                     
         B     DISAMT              GO DISPLAY                                   
         SPACE 2                                                                
DISAMT   DS    0H                                                               
         CLI   TETYPE,TAECTYPM     IF TYPE IS MISCELLANEOUS                     
         BNE   *+12                                                             
         TM    TESTAT,TESTMISC     AND JUST ADDED DUMMY ELEMENTS                
         BO    *+8                 THEN DON'T DISPLAY AMOUNTS                   
         BAS   RE,EDITAMT          DISPLAY AMOUNT                               
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST DETAILS ELEMENT)          
         SPACE 1                                                                
DISTAG   DS    0H                                                               
         LA    RF,TAECTAG          TAGS                                         
         BAS   RE,EDIT3                                                         
         B     XIT                                                              
         SPACE 2                                                                
DISMISC  DS    0H                  MISC. DESCRIPTION                            
         MVC   8(L'TAECDESC,R2),TAECDESC                                        
         B     XIT                                                              
         SPACE 2                                                                
DISCART  DS    0H                  CARTAGE FEE                                  
         ICM   RF,15,TAECCART                                                   
         BZ    *+8                                                              
         BAS   RE,EDITAMT                                                       
         B     XIT                                                              
         SPACE 2                                                                
DISDBL   DS    0H                  DOUBLES                                      
         MVC   8(L'TAECDBL,R2),TAECDBL                                          
         B     XIT                                                              
         SPACE 2                                                                
DISNON   DS    0H                  FORCE NON-UNION                              
         TM    TAECSTAT,TAECSNON                                                
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST DETAILS ELEMENT)          
         SPACE 1                                                                
DISDEM   DS    0H                  PAY DEMO RATES                               
         TM    TAECSTAT,TAECSDEM                                                
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         B     XIT                                                              
         SPACE 2                                                                
DISONC   DS    0H                  FORCE ON-CAMERA                              
         TM    TAECSTAT,TAECSONC                                                
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         B     XIT                                                              
         SPACE 2                                                                
DISCAT   DS    0H                  CATEGORY                                     
         MVC   8(L'TAECCAT,R2),TAECCAT                                          
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST DETAILS ELEMENT)          
         SPACE 1                                                                
DISCCMT  DS    0H                  CAST LINE COMMENT                            
         ZIC   R1,TAECLEN                                                       
         SH    R1,=Y(TAECLNQ+1)                                                 
         BM    XIT                                                              
         ZIC   RE,0(R2)            COMPARE L'COMMENT TO L'FIELD                 
         SH    RE,=H'9'                                                         
         TM    1(R2),FATBXHDR                                                   
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         CR    R1,RE               IF IT'S MORE, ONLY DISPLAY WHAT FITS         
         BNH   *+6                                                              
         LR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   8(0,R2),TAECCMNT                                                 
         SPACE 2                                                                
DISCMNT  DS    0H                  COMMENT                                      
         MVC   BYTE,TETYPE         SET TYPE                                     
         BAS   RE,GETCMNT          LOOK FOR (NEXT) COMMENT FOR TYPE             
         BE    DCMNT10                                                          
         CLC   ACURFORC,ZEROES     NOT FOUND - IF NOT ALREADY SET               
         BNE   *+8                                                              
         ST    R2,ACURFORC         SET CURSOR HERE FOR USER                     
         B     DCMNTX                                                           
DCMNT10  ZIC   R1,TEXCSEQ          BUMP SEQUENCE NUMBER FOR NEXT TIME           
         LA    R1,1(R1)                                                         
         STC   R1,TEXCSEQ                                                       
         L     R4,FULL                                                          
         USING TAXCD,R4                                                         
         ZIC   R1,TAXCLEN                                                       
         SH    R1,=Y(TAXCLNQ+1)                                                 
         BM    DCMNTX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TAXCCMNT                                                 
DCMNTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD TOTALS                                   
         SPACE 1                                                                
DISTOTS  NTR1                                                                   
         L     R4,AIO                                                           
         MVC   ELCODE,TEETELCD     GET ESTIMATE TOTALS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         XC    ELEMENT,ELEMENT     NOT FOUND - USE DUMMY                        
         LA    R4,ELEMENT                                                       
         USING TAETD,R4            R4=A(ESTIMATE TOTALS ELEMENT)                
         SPACE 1                                                                
         L     R2,ATWA             ESTABLISH ADDRESS OF FIRST TOTAL FLD         
         AH    R2,TEDSPTOT                                                      
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         L     RF,TAETNET          TOTAL NET                                    
         BAS   RE,EDITAMT2                                                      
         LA    RF,TAETSNET                                                      
         BAS   RE,SETATTR                                                       
         BAS   RE,BUMP2                                                         
         SPACE 1                                                                
         L     RF,TAETPNH          PENSION AND HEALTH                           
         BAS   RE,EDITAMTL                                                      
         LA    RF,TAETSPNH                                                      
         BAS   RE,SETATTR                                                       
         BAS   RE,BUMP2                                                         
         SPACE 1                                                                
         L     RF,TAETHNW          HEALTH AND WELFARE                           
         BAS   RE,EDITAM9L                                                      
         LA    RF,TAETSHNW                                                      
         BAS   RE,SETATTR                                                       
         BAS   RE,BUMP2                                                         
         SPACE 1                                                                
         L     RF,TAETTAX          TAX AND HANDLING                             
         A     RF,TAETHND                                                       
         A     RF,TAETEMSF         AND EMS FEE                                  
         BAS   RE,EDITAMTL                                                      
         LA    RF,TAETSTNH                                                      
         BAS   RE,SETATTR                                                       
         BAS   RE,BUMP2                                                         
         SPACE 1                                                                
         L     RF,TAETNET          NET                                          
         A     RF,TAETPNH          PENSION AND HEALTH                           
         A     RF,TAETHNW          HEALTH AND WELFARE                           
         A     RF,TAETTAX          TAX                                          
         A     RF,TAETHND          HANDLING                                     
         A     RF,TAETEMSF         EMS FEE                                      
         BAS   RE,EDITAMT2                                                      
         LA    RF,TAETSTOT                                                      
         BAS   RE,SETATTR                                                       
DTOTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS INTENSITY AND PREV. VALIDATED BITS                  
         SPACE 1                                                                
*                                  RF=BIT MASK                                  
         USING TAETD,R4            R4=A(ESTIMATE TOTALS ELEMENT)                
SETATTR  DS    0H                                                               
         NI    1(R2),X'F3'         SET TO NORMAL INTENSITY                      
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TAETSTAT,0          IF AMOUNT IS OVERRIDEN                       
         BZ    *+8                                                              
         OI    1(R2),X'08'         HIGHLIGHT IT                                 
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         BR    RE                                                               
         EJECT                                                                  
*              AMOUNT EDIT ROUTINES                                             
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         SPACE 1                                                                
EDITAMT6 LTR   RF,RF               IF AMOUNT IS 0, JUST DISPLAY "0"             
         BNZ   *+10                                                             
         MVI   8(R2),C'0'                                                       
         BR    RE                                                               
         EDIT  (RF),(6,8(R2)),2,FLOAT=-,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,8-3(R1,R2)                                                    
         CLC   =C'.00',0(R1)       DON'T DISPLAY ZERO DEC.                      
         BNE   *+10                                                             
         XC    0(3,R1),0(R1)                                                    
         BR    RE                                                               
         SPACE 1                                                                
         USING FLDD,R4             R4=A(FIELD TABLE ENTRY)                      
EDITAMT  CLI   FLDIPLEN,10         EDIT BASED ON INPUT FIELD LENGTH             
         BE    EDITAMT0                                                         
         CLI   FLDIPLEN,9                                                       
         BE    EDITAMT9                                                         
         CLI   FLDIPLEN,7                                                       
         BE    EDITAMT7                                                         
EDITAMT8 EDIT  (RF),(8,8(R2)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
EDITAMT7 EDIT  (RF),(7,8(R2)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
EDITAMT9 EDIT  (RF),(9,8(R2)),2,ZERO=NOBLANK,FLOAT=-                            
         BR    RE                                                               
EDITAMT0 EDIT  (RF),(10,8(R2)),2,ZERO=NOBLANK,FLOAT=-                           
         BR    RE                                                               
         SPACE 1                                                                
EDITAMT2 EDIT  (RF),(12,8(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT                
         BR    RE                                                               
EDITAMTL EDIT  (RF),(10,8(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT                
         BR    RE                                                               
EDITAM9L EDIT  (RF),(9,8(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT                 
         BR    RE                                                               
         SPACE 3                                                                
*              OTHER EDIT ROUTINES                                              
         SPACE 1                                                                
EDIT1    DS    0H                  EDIT 1 BYTE TO 2 BYTES                       
         EDIT  (1,0(RF)),(2,8(R2)),ZERO=BLANK,ALIGN=LEFT                        
         BR    RE                                                               
         SPACE 2                                                                
EDIT2    DS    0H                  EDIT 2 BYTES TO 5 BYTES W/ 2 DEC.            
         EDIT  (2,0(RF)),(5,8(R2)),2,ZERO=BLANK,ALIGN=LEFT                      
         BR    RE                                                               
         SPACE 2                                                                
EDIT3    DS    0H                  EDIT 1 BYTE TO 3 BYTES                       
         EDIT  (1,0(RF)),(3,8(R2)),ZERO=BLANK,ALIGN=LEFT                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE DETERMINES IF ON FIRST FIELD OF NEW LINE                 
         SPACE 1                                                                
*                                  R3=A(FIELD ID NUMBER)                        
NEWLINE  NTR1                                                                   
         MVI   ELCODE,0            CLEAR ELCODE FOR TABLE LOOKUP                
         LA    R4,TELINE           LOOP THROUGH LINE TYPE TABLE                 
         USING LINED,R4                                                         
         CLC   0(1,R3),LINEFLDS    MATCH ON FIRST FIELD FOR EACH TYPE           
         BE    NEWLX                                                            
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT LINE TYPE                  
         BE    *-14                                                             
         B     NO                  NOT FOUND - NOT NEW LINE                     
         SPACE 1                                                                
NEWLX    MVC   TETYPE,LINETYPE     NEW LINE - RETURN LINE TYPE                  
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE BUMPS R2 TO NEXT FIELD                                   
         SPACE 1                                                                
BUMP2    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE BUMPS R2 TO NEXT UNPROTECTED FIELD                       
         SPACE 1                                                                
BUMPUN   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPUN                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE BUMPS R2 TO COMMENT FIELD                                
         SPACE 1                                                                
*                                  R3=A(CURRENT FIELD ID NUMBER)                
BMPTOCMT CLI   0(R2),0             TEST FOR END OF SCREEN                       
         BE    BCMTNO                                                           
         TM    1(R2),X'20'         TEST UNPROTECTED FIELD                       
         BO    BCMT10                                                           
         CLI   0(R3),TE#CMNT       MATCH ON COMMENT FIELD ID                    
         BER   RE                                                               
         LA    R3,1(R3)            ELSE TRY NEXT                                
BCMT10   ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         B     BMPTOCMT                                                         
BCMTNO   LTR   RE,RE               NOT FOUND - RETURN CC NE                     
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD A RECORD                                        
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         BAS   RE,VALPFKEY         VALIDATE PFKEY - SET CORRES. TYPE            
         SPACE 1                                                                
         L     R2,TEAOVLYH         FIND A(FIRST UNPROTECTED FIELD)              
         BAS   RE,BUMPUN                                                        
         ST    R2,TEAFRSTH         SAVE IT                                      
         CLI   0(R2),0             IF NO SCREEN (SHOULDN'T HAPPEN,              
         BNE   *+8                               BUT DOES RARELY!)              
         BAS   RE,DISPLAY          DISPLAY THE RECORD FIRST                     
         SPACE 1                                                                
         BAS   RE,REMOVE           REMOVE DISPLAYED ELEMENTS                    
         SPACE 1                                                                
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
         SPACE 1                                                                
         MVI   TELASTTY,0          CLEAR LAST TYPE                              
         XC    TEETEL,TEETEL       CLEAR LOCAL TOTALS ELEMENT                   
         LA    R5,TEECEL           R5=A(LOCAL ESTIMATE CAST ELEMENT)            
         USING TAECD,R5                                                         
         XC    TEECEL,TEECEL       CLEAR IT                                     
         L     R2,TEAFRSTH         R2=A(FIRST DATA LINE)                        
         LA    R3,TESCFLDS         R3=A(FIELD ID TABLE FOR SCREEN)              
         SPACE 1                                                                
BLDR10   BAS   RE,NEWLINE          IF THIS IS FIRST FIELD ON NEW LINE           
         BNE   BLDR50                                                           
         SPACE 1                                                                
BLDR20   CLI   TAECEL,0            TEST IF WE HAVE ELEMENT TO PROCESS           
         BE    BLDR30                                                           
         CLI   TAECNUM,0           IGNORE IF THERE IS NO COUNT                  
         BE    BLDR30                                                           
         CLI   TAECTYPE,TAECTYPM   IF TYPE IS NOT MISCELLANEOUS                 
         BE    BLDR25                                                           
         CLC   TAECDTLS(TAECDTLQ),ZEROES   AND NO TIME DETAILS INPUT            
         BNE   BLDR25                                                           
         CLI   TAECTYPE,TAECTYPU   THEN IF NOT MUSIC                            
         BE    *+12                                                             
         MVI   TAECSP,1            DEFAULT TO 1 SPOT                            
         B     *+10                                                             
         MVC   TAECHRM,=AL2(100)   ELSE DEFAULT TO 1 HOUR                       
         SPACE 1                                                                
BLDR25   MVC   ELEMENT,TAECEL                                                   
         BAS   RE,MYADDL           ADD THE ELEMENT                              
         SPACE 1                                                                
         ZIC   R1,TESEQ            BUMP SEQUENCE NUMBER FOR NEXT TIME           
         LA    R1,1(R1)                                                         
         STC   R1,TESEQ                                                         
         SPACE 1                                                                
BLDR30   CLI   0(R2),0             IF AT END OF SCREEN THEN DONE                
         BE    BLDRX                                                            
         CLC   TETYPE,TELASTTY     IF TYPE CHANGED                              
         BE    BLDR40                                                           
         MVI   TESEQ,0             RESTART SEQUENCE NUMBER                      
         MVC   TELASTTY,TETYPE     SET NEW TYPE                                 
         MVI   TEXCSEQ,0           RESTART COMMENT SEQUENCE NUMBER              
         SPACE 1                                                                
BLDR40   XC    TEECEL,TEECEL       BUILD NEW ELEMENT                            
         MVC   TAECEL,TEECELCD     ELEMENT CODE                                 
         MVI   TAECLEN,TAECLNQ     ELEMENT LENGTH                               
         MVC   TAECTYPE,TETYPE     TYPE                                         
         MVC   TAECSEQ,TESEQ       SEQUENCE NUMBER                              
         SPACE 1                                                                
         BAS   RE,ANYINPUT         TEST FOR INPUT ON THIS LINE                  
         BE    BLDR50                                                           
         CLI   0(R2),0             NO - IF REACHED END OF SCREEN                
         BE    BLDRX                    THEN DONE                               
         B     BLDR10              ELSE RETURNED A(NEXT LINE) OR CMNT           
         SPACE 1                                                                
BLDR50   LA    R4,TEFLD            R4=A(FIELD TABLE)                            
         USING FLDD,R4                                                          
         MVI   ELCODE,0                                                         
         CLC   FLDID,0(R3)         MATCH ON FIELD ID NUMBER                     
         BE    BLDR60                                                           
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT FIELD TABLE ENTRY          
         BE    *-14                                                             
         DC    H'0'                UNDEFINED FIELD TYPE IN LINE TABLE           
         SPACE 1                                                                
BLDR60   LH    RF,FLDDVAL          RF=A(VALIDATION RTN FOR CURRENT FLD)         
         LTR   RF,RF                                                            
         BZ    BLDR80                                                           
         AR    RF,RB                                                            
         LA    RE,BLDR70                                                        
         NTR1  ,                                                                
         LA    R4,TEETEL           R4=A(ESTIMATE TOTALS ELEMENT)                
         BR    RF                  VALIDATE THE FIELD                           
         SPACE 1                                                                
BLDR70   ST    R2,TEALASTH         SAVE A(LAST FIELD VALIDATED)                 
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+8                                                              
         OI    TESTAT,TESTCHGD     SET TO RE-DISPLAY THIS PAGE                  
         SPACE 1                                                                
BLDR80   BAS   RE,BUMPUN           BUMP TO NEXT DISPLAY FIELD                   
         BE    BLDR20                                                           
         LA    R3,1(R3)            BUMP FIELD ID NUMBER POINTER                 
         B     BLDR10                                                           
         SPACE 1                                                                
BLDRX    BAS   RE,GENERATE         GENERATE RATE-RELATED ELEMENTS               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES PFKEY AND SETS CORRESPONDING TYPE              
         SPACE 1                                                                
VALPFKEY NTR1                                                                   
         MVI   TEPFTYPE,0          CLEAR PF TYPE                                
         SPACE 1                                                                
         CLI   PFAID,0             DON'T BOTHER IF NO PFKEY PRESSED             
         BE    VPFKX                                                            
         L     R1,SYSPARMS         GET DISPLACEMENT TO CURSOR POS.              
         L     R1,0(R1)            R1=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R1                                                         
         LH    R5,TIOBCURD         R5=DISPLACMENT TO CURSOR                     
         SPACE 1                                                                
         L     R4,TEAPFKS          R4=A(PFKEY TABLE)                            
         USING PFKD,R4                                                          
         MVI   ELCODE,0            CLEAR ELCODE FOR TABLE LOOKUP                
VPFK10   CLI   PFKTYPE,PFKTYPU     IGNORE USER-DEFINED TABLE ENTRIES            
         BE    VPFK14                                                           
         ZIC   R2,PFKDSP                                                        
         LA    R2,TEPFKEYS(R2)     R2=A(PFKEY DEFINITION IN W/S)                
         CLC   PFAID,0(R2)         IF IT MATCHES PFKEY PRESSED                  
         BE    VPFK20              GO HANDLE                                    
VPFK14   BAS   RE,NEXTEL           ELSE TRY NEXT                                
         BE    VPFK10                                                           
         B     ERRPFK              ERROR IF NO MATCH                            
         SPACE 1                                                                
VPFK20   MVC   TEPFTYPE,PFKTYPE    SAVE TYPE                                    
         SPACE 1                                                                
         CLC   PFAID,TEPFCMNT      IF THIS ISN'T COMMENT PFKEY                  
         BNE   VPFKX               THEN DONE                                    
         SPACE 1                                                                
         MVI   TEPFTYPE,0          ELSE SET TYPE BASED ON CURSOR POS.           
         LA    R3,TESCRN           R3=A(SCREEN TABLE)                           
         USING SCRND,R3                                                         
VPFK30   CLI   SCRND,0             IF REACHED END OF SCREEN TABLE               
         BE    VPFK50              THEN DONE                                    
         CLC   SCRNDISP,ZEROES     ELSE IF CURRENT TYPE ON SCREEN               
         BE    VPFK40                                                           
         CH    R5,SCRNDISP         AND DISP TO CURSOR ON/AFTER                  
         BL    VPFK40                                                           
         MVC   TEPFTYPE,SCRNTYPE   SAVE TYPE                                    
         SPACE 1                                                                
VPFK40   LA    R3,SCRNNEXT         TRY NEXT SCREEN TABLE ENTRY                  
         B     VPFK30                                                           
         SPACE 1                                                                
VPFK50   CLI   TEPFTYPE,0          IF TYPE NOT SET                              
         BE    ERRPFKF             THEN PFKEY IS INVALID FOR THIS FIELD         
         SPACE 1                                                                
VPFKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE REMOVES DISPLAYED ELEMENTS FROM RECORD                   
         SPACE 1                                                                
REMOVE   NTR1                                                                   
         MVC   ELCODE,TEETELCD     REMOVE ESTIMATE TOTALS ELEMENT               
         GOTO1 REMELEM                                                          
         MVC   ELCODE,TEEOELCD     REMOVE ESTIMATE OPTIONS ELEMENT              
         GOTO1 REMELEM                                                          
         MVC   ELCODE,TEWCELCD     REMOVE WORK-CODE SUMMARY ELEMENT             
         GOTO1 REMELEM                                                          
*                                  SET TO DELETE DISPLAYED TYPES                
         LA    R3,TESCRN           LOOP THROUGH SCREEN TABLE                    
         USING SCRND,R3                                                         
REM50    CLI   SCRND,0             TEST END OF TABLE                            
         BE    REMX                                                             
         CLC   SCRNDISP,ZEROES     TEST THIS TYPE DISPLAYED                     
         BE    REM60                                                            
         SPACE 1                                                                
         MVC   ELCODE,TEECELCD     SET ESTIMATE CAST ELEMENT CODE               
         GOTO1 DELL,DMCB,(1,SCRNTYPE)  DELETE EST CAST ELS OF THIS TYPE         
         SPACE 1                                                                
         CLI   SCRNNUMC,0          IF THERE ARE COMMENT LINES                   
         BE    REM60                                                            
         MVC   ELCODE,TEXCELCD     SET COMMENTS ELEMENT CODE                    
         GOTO1 DELL,DMCB,(1,SCRNTYPE)  DELETE COMMENT ELS OF THIS TYPE          
         SPACE 1                                                                
REM60    LA    R3,SCRNNEXT         BUMP TO NEXT ENTRY                           
         B     REM50                                                            
         SPACE 1                                                                
REMX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES OPTIONS FIELD                                  
         SPACE 1                                                                
VALOPTS  NTR1                                                                   
         XC    TEOPTS(TEOPTSLQ),TEOPTS  CLEAR W/S FOR OPTIONS                   
         SPACE 1                                                                
         L     R2,TEAOPTSH         R2=A(FIELD)                                  
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          R4=A(ESTIMATE OPTIONS ELEMENT)               
         USING TAEOD,R4                                                         
         MVC   TAEOEL,TEEOELCD     ELEMENT CODE                                 
         MVI   TAEOLEN,TAEOLNQ     ELEMENT LENGTH                               
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN ENTRIES                            
         SPACE 1                                                                
VOPT2    MVC   TEERRDSP,SCDISP1    SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         ZIC   R1,SCLEN1                                                        
         LTR   R1,R1               MUST HAVE LHS                                
         BZ    ERRINV                                                           
         BCTR  R1,0                R1=L'LHS-1                                   
         SPACE 1                                                                
         USING OPTD,R5                                                          
         LA    R5,OPTTAB           R5=A(OPTION TABLE ENTRY)                     
VOPT4    CLI   OPTD,X'FF'                                                       
         BE    ERRINV              END-OF-TABLE                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTLHS(0),SCDATA1   MATCH ON L'INPUT                             
         BE    *+12                                                             
         LA    R5,OPTNEXT          TRY NEXT                                     
         B     VOPT4                                                            
         SPACE 1                                                                
         MVC   TEERRDSP,SCDISP2    SET DISP. INTO FLD OF RHS FOR ERRORS         
         SPACE 1                                                                
         CLI   SCLEN2,0            MUST HAVE RHS                                
         BE    ERRINV                                                           
         LH    RF,OPTDVAL                                                       
         AR    RF,RB               RF=A(VALIDATION ROUTINE)                     
         LA    RE,VOPT8            RE=A(RETURN FOR COMMON NTR1)                 
         NTR1  ,                                                                
         BR    RF                  ** VALIDATE OPTION **                        
         SPACE 1                                                                
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT SCAN FIELD                      
         BCT   R0,VOPT2            AND VALIDATE IT                              
         MVI   TEERRDSP,0                                                       
         SPACE 1                                                                
         BAS   RE,MYADDL           ADD ESTIMATE OPTIONS EL. TO RECORD           
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES                            
         SPACE 1                                                                
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
         USING TAEOD,R4            R4=A(ESTIMATE OPTIONS ELEMENT)               
         SPACE 1                                                                
VALPNHR  DS    0H                  PENSION AND HEALTH RATE                      
         BAS   RE,OPTAMTVL         VALIDATE RHS                                 
         OC    HALF,HALF           IF INPUT IS 0                                
         BNZ   *+8                                                              
         OI    HALF,X'80'          TURN ON BIT TO KNOW 0 INPUT                  
         MVC   TAEOPNHR,HALF       SAVE RATE                                    
         MVC   TEOPNHR,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALTNHR  DS    0H                  TAX AND HANDLING RATE                        
         BAS   RE,OPTAMTVL         VALIDATE RHS                                 
         OC    HALF,HALF           IF INPUT IS 0                                
         BNZ   *+8                                                              
         OI    HALF,X'80'          TURN ON BIT TO KNOW 0 INPUT                  
         MVC   TAEOTNHR,HALF       SAVE RATE                                    
         MVC   TEOTNHR,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALTAXR  DS    0H                  TAX RATE                                     
         BAS   RE,OPTAMTVL         VALIDATE RHS                                 
         OC    HALF,HALF           IF INPUT IS 0                                
         BNZ   *+8                                                              
         OI    HALF,X'80'          TURN ON BIT TO KNOW 0 INPUT                  
         MVC   TAEOTAXR,HALF       SAVE RATE                                    
         MVC   TEOTAXR,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALHNDR  DS    0H                  HANDLING RATE                                
         BAS   RE,OPTAMTVL         VALIDATE RHS                                 
         OC    HALF,HALF           IF INPUT IS 0                                
         BNZ   *+8                                                              
         OI    HALF,X'80'          TURN ON BIT TO KNOW 0 INPUT                  
         MVC   TAEOHNDR,HALF       SAVE RATE                                    
         MVC   TEOHNDR,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALYEAR  DS    0H                  CONTRACT YEAR                                
         GOTO1 YRVAL,DMCB,SCDATA2                                               
         BNE   ERRINV                                                           
         MVC   TAEOYEAR,TGYREQU    SAVE BINARY YEAR                             
         MVC   TEOYEAR,TGYREQU                                                  
         B     XIT                                                              
         SPACE 2                                                                
VALUSE   DS    0H                  USE CODE                                     
         GOTO1 USEVAL,DMCB,(X'10',SCDATA2),SCDATA2+3                            
         BNE   ERRINV                                                           
         TM    TGUSSTAT,SESSION    MUST BE SESSION USE TYPE                     
         BZ    ERRINV                                                           
         MVC   BYTE,TGUSTYMD       INSURE VALID FOR MEDIA                       
         NC    BYTE,TGMEEQU                                                     
         BZ    ERRUSEM                                                          
         MVC   TAEOUSE,TGUSCDE     SAVE USE CODE                                
         MVC   TAEOUTYP,TGUSTYP         AND TYPE                                
         MVC   TEOUSE,TGUSCDE                                                   
         MVC   TEOUTYP,TGUSTYP                                                  
         B     XIT                                                              
         SPACE 2                                                                
VALAFM   DS    0H                  AFM RATE                                     
         CLI   SCLEN2,1            RHS MUST BE 1 CHAR                           
         BNE   ERRINV                                                           
         CLI   SCDATA2,C'1'        CHOICES ARE '1'                              
         BE    VAFMX                                                            
         CLI   SCDATA2,C'2'                    '2'                              
         BE    VAFMX                                                            
         CLI   SCDATA2,C'5'                    '5'                              
         BNE   ERRINV                                                           
VAFMX    MVC   TAEOAFM,SCDATA2     SAVE IT                                      
         MVC   TEOAFM,SCDATA2                                                   
         B     XIT                                                              
         EJECT                                                                  
*              OPTION VALIDATION SUBSIDIARY ROUTINES, CONT'D.                   
         SPACE 1                                                                
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
         USING TAEOD,R4            R4=A(ESTIMATE OPTIONS ELEMENT)               
         SPACE 1                                                                
VALADST  DS    0H                  ADDENDUM STATE                               
         CLI   SCLEN2,2                                                         
         BNE   ERRINV                                                           
         GOTO1 TAXVAL,DMCB,(2,SCDATA2)                                          
         BNE   ERRINV                                                           
         TM    TGTASTAT,TALUOKAD   TEST ADDENDUM CONTRACT INSTALLED             
         BZ    ERRINV                                                           
         MVC   TAEOADST,SCDATA2    SAVE IT                                      
         MVC   TEOADST,SCDATA2                                                  
         B     XIT                                                              
         SPACE 2                                                                
VALMULT  DS    0H                  MULTIPLIER                                   
         BAS   RE,OPTAMTVL         VALIDATE RHS                                 
         MVC   TAEOMULT,HALF       SAVE RATE                                    
         MVC   TEOMULT,HALF                                                     
         B     XIT                                                              
         SPACE 3                                                                
OPTAMTVL NTR1                                                                   
         ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)  VALIDATE FOR 2 DEC. PLACES            
         CLI   0(R1),X'FF'                                                      
         BE    ERRAMT                                                           
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE                         
         BO    ERRAMT                                                           
         CLC   4(4,R1),=X'00007FFF'  DON'T ALLOW MORE THAN HALFWORD             
         BH    ERRAMT                                                           
         MVC   HALF,6(R1)          SAVE AMOUNT IN FULL                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF THERE'S INPUT ON THIS LINE                 
         SPACE 1                                                                
*                                  R2=A(FIRST FIELD ON LINE)                    
*                                  R3=CORRESPONDING FIELD ID NUMBER             
ANYINPUT NTR1                                                                   
ANYIN10  CLI   5(R2),0             TEST FOR INPUT                               
         BNE   YES                 RETURN CC EQ IF FOUND                        
         SPACE 1                                                                
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+8                                                              
         OI    TESTAT,TESTCHGD     SET TO RE-DISPLAY THIS PAGE                  
         SPACE 1                                                                
         BAS   RE,BUMPUN           BUMP TO NEXT UNPROTECTED FIELD               
         BE    ANYINNO                                                          
         LA    R3,1(R3)            AND TO NEXT FIELD ID NUMBER                  
         SPACE 1                                                                
         BAS   RE,NEWLINE          IF THIS ISN'T START OF NEW LINE              
         BE    *+12                                                             
         CLI   0(R3),TE#CMNT       AND IF THIS ISN'T COMMENT FIELD              
         BNE   ANYIN10             THEN LOOP                                    
         SPACE 1                                                                
ANYINNO  LTR   R2,R2               NO INPUT ON LINE - RETURN CC NE              
         XIT1  REGS=(R2,R3)                           AND NEW R2,R3             
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES                                   
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALNUM   DS    0H                  N'CAST                                       
         CLI   5(R2),1                                                          
         BL    ERRMISS                                                          
         BH    *+12                                                             
         CLI   8(R2),C'0'          IGNORE INPUT OF ZERO                         
         BE    *+14                (WILL DELETE LINE)                           
         BAS   RE,NUMVAL                                                        
         MVC   TAECNUM,ACTUAL                                                   
         B     XIT                                                              
         SPACE 2                                                                
VALOV    DS    0H                  OVERSCALE RATE                               
         BAS   RE,AMTVAL                                                        
         CLC   FULL,=AL4(60000)    DON'T ALLOW MORE THAN 600%                   
         BH    ERRAMT                                                           
         MVC   TAECOV1,FULL+2                                                   
         B     XIT                                                              
         SPACE 2                                                                
VALSPT   DS    0H                  SPOTS                                        
         BAS   RE,NUMVAL                                                        
         MVC   TAECSP,ACTUAL                                                    
         B     XIT                                                              
         SPACE 2                                                                
VALDAY   DS    0H                  DAYS                                         
         BAS   RE,NUMVAL                                                        
         MVC   TAECDAY,ACTUAL                                                   
         B     XIT                                                              
         SPACE 2                                                                
VALOT    DS    0H                  OVERTIME HOURS                               
         BAS   RE,NUMVAL                                                        
         MVC   TAECOT,ACTUAL                                                    
         B     XIT                                                              
         SPACE 2                                                                
VALDT    DS    0H                  DOUBLETIME HOURS                             
         BAS   RE,NUMVAL                                                        
         MVC   TAECDT,ACTUAL                                                    
         B     XIT                                                              
         SPACE 2                                                                
VALTAG   DS    0H                  TAGS                                         
         BAS   RE,NUMVAL                                                        
         MVC   TAECTAG,ACTUAL                                                   
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES, CONT'D.                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALTRV   DS    0H                  TRAVEL TIME                                  
         MVC   TGFULL,=F'15'       VALID INCREMENTS ARE 15 MINUTES              
         OI    TGFULL,X'80'        SET NO 1 HR MINIMUM                          
         BAS   RE,VALHRMN                                                       
         MVC   TAECTRV,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALPDW   DS    0H                  PRIOR-DAY WARDROBE TIME                      
         MVC   TGFULL,=F'15'       VALID INCREMENTS ARE 15 MINUTES              
         BAS   RE,VALHRMN                                                       
         MVC   TAECPDW,HALF                                                     
         B     XIT                                                              
         SPACE 2                                                                
VALHRM   DS    0H                  HOURS/MINUTES                                
         MVC   TGFULL,=F'1'        VALID INCREMENTS ARE 1 MINUTE                
         CLI   TETYPE,TAECTYPU     IF PROCESSING MUSICIAN                       
         BNE   *+10                                                             
         MVC   TGFULL,=F'20'       VALID INCREMENTS ARE 20 MINUTES              
         BAS   RE,VALHRMN                                                       
         MVC   TAECHRM,HALF                                                     
         CLI   TETYPE,TAECTYPU     IF PROCESSING MUSICIAN                       
         BNE   VHRMX                                                            
         CLC   TAECHRM,ZEROES      AND NO HOURS/MINUTES INPUT                   
         BNE   VHRMX                                                            
         CLI   TAECSP,0            AND SPOTS WERE INPUT                         
         BE    VHRMX                                                            
         MVC   TAECHRM,=AL2(100)   DEFAULT TO 1 HOUR                            
VHRMX    B     XIT                                                              
         SPACE 2                                                                
VALDEM   DS    0H                  PAY DEMO RATES                               
         BAS   RE,YORNVAL                                                       
         OI    TAECSTAT,TAECSDEM   ONLY RETURNS IF INPUT Y(ES)                  
         B     XIT                                                              
         SPACE 2                                                                
VALONC   DS    0H                  FORCE ON-CAMERA                              
         BAS   RE,YORNVAL                                                       
         OI    TAECSTAT,TAECSONC   ONLY RETURNS IF INPUT Y(ES)                  
         B     XIT                                                              
         SPACE 2                                                                
VALNON   DS    0H                  FORCE NON-UNION                              
         BAS   RE,YORNVAL                                                       
         OI    TAECSTAT,TAECSNON   ONLY RETURNS IF INPUT Y(ES)                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES, CONT'D.                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALCCMT  DS    0H                  CAST LINE COMMENT                            
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAECCMNT(0),8(R2)   MOVE COMMENT TO END OF ELEMENT               
         LA    R1,TAECLNQ+1(R1)    ADD TO L'ELEMENT                             
         STC   R1,TAECLEN          SAVE NEW ELEMENT LENGTH                      
         B     XIT                                                              
         SPACE 2                                                                
VALCAT   DS    0H                  CATEGORY                                     
         CLI   5(R2),0                                                          
         BE    ERRMISS             GIVE MISSING ERROR HERE (NOT IN ANY)         
         GOTO1 ANY                                                              
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY CODE                       
         BNE   ERRINV                                                           
         SPACE 1                                                                
         CLI   TETYPE,TAECTYPS     IF TYPE IS SINGER                            
         BE    *+12                                                             
         CLI   TETYPE,TAECTYPT     OR RADIO SINGER                              
         BNE   *+12                                                             
         TM    TGCASTAT,SINGER     TEST THIS IS A SINGER                        
         BZ    ERRINV                                                           
         SPACE 1                                                                
         CLI   TETYPE,TAECTYPX     IF TYPE IS EXTRA                             
         BNE   *+12                                                             
         TM    TGCATYPE,EXTRA      TEST THIS IS AN EXTRA                        
         BZ    ERRINV                                                           
         SPACE 1                                                                
         CLI   TETYPE,TAECTYPU     IF TYPE IS MUSIC                             
         BNE   VCAT10                                                           
*        TM    TGCAUNI,AFM         INSURE AFM IS A VALID UNION                  
         GOTOR UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    ERRINV                                                           
         SPACE 1                                                                
VCAT10   MVC   TAECCAT,TGCAT       SAVE CATEGORY CODE                           
         B     XIT                                                              
         SPACE 2                                                                
VALDBL   DS    0H                  DOUBLES                                      
         BAS   RE,NUMVAL                                                        
         MVC   TAECDBL,8(R2)       SAVE CHARACTER                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES, CONT'D.                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALMISC  DS    0H                  MISC. DESCRIPTION                            
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         GOTO1 ANY                                                              
         MVC   TAECDESC,WORK                                                    
         B     XIT                                                              
         SPACE 2                                                                
VALNET   DS    0H                  NET AMOUNT                                   
         CLI   TETYPE,TAECTYPM     IF PROCESSING MISC. TYPE                     
         BNE   *+12                                                             
         BAS   RE,AMTVAL           SIMPLY VALIDATE AMOUNT                       
         B     *+12                                                             
         BAS   RE,TOTVAL           ELSE VALIDATE TOTAL                          
         BNE   VNETX               CC NE IMPLIES IGNORE OR NO INPUT             
         L     R1,FULL                                                          
         CLI   TETYPE,TAECTYPU     IF PROCESSING MUSICIANS                      
         BNE   *+10                                                             
         ICM   R0,15,TAECCART      OVERRIDDEN NET INCLUDES CARTAGE              
         SR    R1,R0               SO BACK IT OUT FOR CALCULATIONS              
         ST    R1,TAECNET          SAVE OVERRIDE AMOUNT                         
         OI    TAECSTAT,TAECSNET   SET OVERRIDE BIT                             
VNETX    B     XIT                                                              
         SPACE 2                                                                
VALPNH   DS    0H                  PENSION AND HEALTH                           
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAECPNH,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAECSTAT,TAECSPNH   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALHNW   DS    0H                  HEALTH AND WELFARE                           
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAECHNW,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAECSTAT,TAECSHNW   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALTAX   DS    0H                  TAX                                          
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAECTAX,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAECSTAT,TAECSTAX   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALHND   DS    0H                  TAX                                          
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAECHND,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAECSTAT,TAECSHND   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES, CONT'D.                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALCMNT  DS    0H                  COMMENT                                      
         CLI   5(R2),0                                                          
         BE    XIT                 GET OUT IF NOTHING INPUT                     
         SPACE 1                                                                
         CLI   TAECNUM,0           IF CURRENT EST. CAST EL HAS NO COUNT         
         BNE   VCMNT10                                                          
         MVC   ELCODE,TEECELCD     SET ELEMENT CODE FOR EST. CAST ELS.          
         GOTO1 GETL,DMCB,(1,TETYPE)  AND IF THERE AREN'T ANY CAST ELS           
         BNE   XIT                 THEN IGNORE COMMENTS                         
         SPACE 1                                                                
VCMNT10  XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          BUILD EXTENDED COMMENT EL.                   
         USING TAXCD,R4                                                         
         MVC   TAXCEL,TEXCELCD     ELEMENT CODE                                 
         MVC   TAXCTYPE,TETYPE     ELEMENT TYPE                                 
         MVC   TAXCSEQ,TEXCSEQ     ELEMENT SEQ. NUMBER                          
         ZIC   R1,TEXCSEQ                                                       
         LA    R1,1(R1)            BUMP SEQ. NUMBER FOR NEXT                    
         STC   R1,TEXCSEQ                                                       
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)   MOVE COMMENT TO END OF ELEMENT               
         LA    R1,TAXCLNQ+1(R1)    ADD TO L'ELEMENT                             
         STC   R1,TAXCLEN          SAVE NEW ELEMENT LENGTH                      
         BAS   RE,MYADDL           ADD IT TO RECORD                             
         B     XIT                                                              
         SPACE 2                                                                
VALCART  DS    0H                  CARTAGE FEE                                  
         BAS   RE,AMTVAL                                                        
         MVC   TAECCART,FULL                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION SUBSIDIARY ROUTINES, CONT'D.                          
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
         USING TAETD,R4            R4=A(ESTIMATE TOTALS ELEMENT)                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
VALTNET  DS    0H                  TOTAL NET                                    
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAETNET,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAETSTAT,TAETSNET   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALTPNH  DS    0H                  TOTAL PENSION AND HEALTH                     
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAETPNH,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAETSTAT,TAETSPNH   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALTHNW  DS    0H                  TOTAL HEALTH AND WELFARE                     
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
         BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
         MVC   TAETHNW,FULL        ELSE SAVE OVERRIDE AMOUNT                    
         OI    TAETSTAT,TAETSHNW   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALTTNH  DS    0H                  TOTAL TAX AND HANDLING                       
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
***      BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
***      MVC   TAETTNH,FULL        ELSE SAVE OVERRIDE AMOUNT                    
***      OI    TAETSTAT,TAETSTNH   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         SPACE 2                                                                
VALTGRS  DS    0H                  TOTAL GROSS                                  
         BAS   RE,TOTVAL           VALIDATE TOTAL                               
***      BNE   *+14                CC NE IMPLIES IGNORE OR NO INPUT             
***      MVC   TAETTOT,FULL        ELSE SAVE OVERRIDE AMOUNT                    
***      OI    TAETSTAT,TAETSTOT   AND SET OVERRIDE BIT                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES NUMERIC INPUT                                  
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
NUMVAL   NTR1                                                                   
         MVI   ACTUAL,0                                                         
         CLI   5(R2),0             INPUT ALWAYS OPTIONAL                        
         BE    NUMVX                                                            
         L     R3,ASTARTSV         NEED TO CORRECT SAVED STORAGE IF             
         MVC   ASTARTSV,TESVSTSV   EXTERNAL ROUTINE CAN EXIT TO USER            
         GOTO1 VALINUM                                                          
         ST    R3,ASTARTSV                                                      
NUMVX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE VALIDATES Y(ES) OR N(O) FIELDS                           
         SPACE 1                                                                
YORNVAL  DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   8(R2),C'N'                                                       
         BE    XIT                                                              
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         BR    RE                  ONLY RETURN IF INPUT WAS Y(ES)               
         EJECT                                                                  
*              ROUTINE VALIDATES AMOUNT FIELDS                                  
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
AMTVAL   NTR1                                                                   
         XC    FULL,FULL                                                        
         CLI   5(R2),0             RETURN CC NE IF NOT INPUT                    
         BE    NO                                                               
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)  VALIDATE FOR 2 DEC. PLACES              
         CLI   0(R1),X'FF'                                                      
         BE    ERRAMT                                                           
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE                         
         BO    ERRAMT                                                           
         CLC   4(4,R1),=AL4(999999999)  DON'T ALLOW 10 MILLION OR MORE          
         BH    ERRAMT                                                           
         MVC   FULL,4(R1)          SAVE AMOUNT IN FULL                          
         B     YES                 RETURN CC EQUAL                              
         SPACE 3                                                                
*              ROUTINE VALIDATES OVERRIDABLE TOTAL FIELDS                       
         SPACE 1                                                                
TOTVAL   NTR1                                                                   
         TM    4(R2),X'20'         IF FIELD DIDN'T CHANGE                       
         BZ    *+12                                                             
         TM    1(R2),X'08'         THEN IF FIELD WASN'T HIGHLIGHTED             
         BZ    NO                    IGNORE                                     
         CLI   5(R2),0             ELSE IF THERE'S INPUT                        
         BE    NO                                                               
         BAS   RE,AMTVAL           VALIDATE IT                                  
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN HOURS/MINUTES FIELD                       
         SPACE 1                                                                
*                                  TGFULL BYTE 0 X'80'=NO MINIMUM               
*                                            1-3 = VALID INCREMENTS             
*                                  R2=A(FIELD)                                  
VALHRMN  NTR1                                                                   
         XC    HALF,HALF           PRE-CLEAR RETURN FIELD                       
         BAS   RE,AMTVAL           VALIDATE AMOUNT                              
         BNE   VALHRX                                                           
         ICM   RF,15,FULL          MOVE TO RF / TEST FOR ZERO                   
         BZ    VALHRX                                                           
         STH   RF,HALF             SAVE VALUE IN HALF                           
         SPACE 1                                                                
         TM    TGFULL,X'80'        UNLESS SPECIFIED                             
         BO    *+12                                                             
         C     RF,=F'100'          MINIMUM IS 1 HOUR                            
         BL    ERRAMT                                                           
         C     RF,=F'9900'         MAXIMUM IS 99 HOURS                          
         BH    ERRAMT                                                           
         XR    RE,RE                                                            
         D     RE,=F'100'          RF=N'HOURS                                   
         LR    R1,RE               R1=N'MINUTES                                 
         CH    R1,=H'60'           MUST BE LESS THAN SIXTY MINUTES              
         BNL   ERRAMT                                                           
         SPACE                                                                  
         MVI   TGFULL,0            CLEAR FLAG                                   
         CLC   TGFULL,=F'1'        DON'T BOTHER IF TGFULL=1                     
         BE    VALHRX                                                           
         MH    RF,=H'60'             N'HOURS * 60 MINUTES                       
         AR    R1,RF               + N'MINUTES = TOTAL N'MINUTES                
         XR    R0,R0                                                            
         D     R0,TGFULL           INSURE VALID INTERVAL                        
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BNZ   ERRAMT                                                           
VALHRX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES ACCUMS IN THE VARIOUS ELEMENTS                 
         SPACE 1                                                                
GENERATE NTR1                                                                   
         XC    TEWCTAB,TEWCTAB     CLEAR WORK-CODE TABLE                        
         XC    TCARATES,TCARATES   CLEAR SO SYSCALC WILL LOAD RATE TBLS         
         SPACE 1                                                                
         BAS   RE,SETTNHR          SET TAX AND HANDLING RATES TO USE            
         SPACE 1                                                                
         XR    R0,R0               CLEAR ACCUM REGISTERS                        
         XR    R1,R1                                                            
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         XR    R4,R4                                                            
         XC    TEEMSACC,TEEMSACC                                                
         SPACE 1                                                                
         L     R5,AIO                                                           
         MVC   ELCODE,TEECELCD     LOOP THROUGH ALL CAST ELEMENTS               
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
GEN30    BAS   RE,NEXTEL2                                                       
         BNE   GEN40                                                            
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
         SPACE 1                                                                
         BAS   RE,SETUP            SET UP W/S FOR SYSCALC                       
         SPACE 1                                                                
         BAS   RE,CALC             CALCULATE AMOUNTS                            
         SPACE 1                                                                
         BAS   RE,INTFACE          UPDATE PRODUCTION INTERFACE BLOCK            
         SPACE 1                                                                
         A     R0,TAECNET          ADD UP ALL ACCUMS                            
         A     R1,TAECPNH                                                       
         A     R2,TAECHNW                                                       
         A     R3,TAECTAX                                                       
         A     R4,TAECHND                                                       
         L     RE,TEEMSACC                                                      
         A     RE,TAECEMSF                                                      
         ST    RE,TEEMSACC                                                      
         B     GEN30               GET NEXT ESTIMATE CAST ELEMENT               
         SPACE 1                                                                
GEN40    BAS   RE,BLDTOTS          BUILD/ADD TOTALS ELEMENT                     
         SPACE 1                                                                
         BAS   RE,BLDWC            BUILD/ADD WORK-CODE SUMMARY ELEMENT          
         B     XIT                                                              
         EJECT                                                                  
*              SET UP TAX AND HANDLING RATES                                    
         SPACE 1                                                                
SETTNHR  NTR1                                                                   
*                                  FIRST DETERMINE TAX RATE                     
         LH    R0,TEOTAXR          USE TAX RATE OPTION                          
         TM    TEOTAXR,X'80'       IF INPUT=0                                   
         BZ    *+10                                                             
         XR    R0,R0               MAKE R0=0                                    
         B     STNH10                                                           
         LTR   R0,R0                                                            
         BNZ   STNH10                                                           
         LH    R0,TEOTNHR          ELSE USE T&H RATE OPTION                     
         TM    TEOTNHR,X'80'       IF INPUT=0                                   
         BZ    *+10                                                             
         XR    R0,R0               MAKE R0=0                                    
         B     STNH10                                                           
         LTR   R0,R0                                                            
         BNZ   STNH10                                                           
         LH    R0,TEOVTAXR         ELSE USE OVERRIDE RATE FROM SHELL            
         LTR   R0,R0                                                            
         BNZ   STNH10                                                           
         LH    R0,TETLTAXR         ELSE USE RATE FROM TALENT AGENCY REC         
         LTR   R0,R0                                                            
         BNZ   STNH10                                                           
         LH    R0,=H'1600'         ELSE USE 16%                                 
STNH10   STH   R0,TETAXR                                                        
         SPACE 1                                                                
*                                  NOW DETERMINE HANDLING RATE                  
         LH    R0,TEOHNDR          USE HANDLING RATE OPTION                     
         TM    TEOHNDR,X'80'       IF INPUT=0                                   
         BZ    *+10                                                             
         XR    R0,R0               MAKE R0=0                                    
         B     STNH20                                                           
         LTR   R0,R0                                                            
         BNZ   STNH20                                                           
         CLC   TEOTNHR,ZEROES      ELSE IF T&H RATE OPTION SET THEN             
         BNE   STNH20              NO HANDLING                                  
         LH    R0,TEOVHNDR         ELSE USE OVERRIDE RATE FROM SHELL            
         LTR   R0,R0                                                            
         BNZ   STNH20                                                           
         LH    R0,TETLHNDR         ELSE USE RATE FROM TALENT AGENCY REC         
         LTR   R0,R0                                                            
         BNZ   STNH20                                                           
         LH    R0,=H'400'          ELSE USE 4%                                  
STNH20   STH   R0,TEHNDR                                                        
         B     XIT                                                              
         EJECT                                                                  
*              SET UP WORKING STORAGE FOR SYSCALC                               
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
SETUP    NTR1                                                                   
         XC    TCCAST(TCCSTLNQ),TCCAST  CLEAR SYSCALC CAST LEVEL INFO           
         MVI   TCOPTS,0                                                         
         SPACE 1                                                                
         BAS   RE,SETUSE           SET UP USE TYPE DETAILS                      
         SPACE 1                                                                
         BAS   RE,SETTASD          SET SESSION DETAILS ELEMENT                  
         SPACE 1                                                                
         BAS   RE,SETCAST          SET UP CAST DETAILS                          
         SPACE 1                                                                
         LA    R4,TETACO           PASS A(COMMERCIAL DETAILS ELEMENT)           
         ST    R4,TCATACO                                                       
         USING TACOD,R4                                                         
         MVC   TACOAFM,TEOAFM      SET AFM RATE IF DEFINED                      
         MVC   TACOADST,TEOADST    SET ADDENDUM STATE IF DEFINED                
         SPACE 1                                                                
         MVC   TCPNHR,TEOPNHR      SET P&H RATE OVERRIDE                        
         TM    TEOPNHR,X'80'       IF INPUT = 0                                 
         BZ    SUP5                                                             
         NI    TCPNHR,X'7F'        TURN OFF X'80' BIT FOR RATE CALC             
         B     SUP10                                                            
SUP5     CLC   TEOPNHR,ZEROES      IF P&H RATE OVERRIDE INPUT                   
         BE    *+8                                                              
SUP10    OI    TCOPTS,TCOPNHR      LET RATE CALC. KNOW                          
         SPACE 1                                                                
         TM    TAECSTAT,TAECSNET   IF OVERRIDE AMOUNT INPUT                     
         BZ    SUPX                                                             
         MVC   TCPAY,TAECNET       SET IT NOW                                   
         OI    TCINPUT,TCINPAY     LET RATE CALC. KNOW                          
         SPACE 1                                                                
         CLI   TAECTYPE,TAECTYPU   AND IF PROCESSING MUSICIAN                   
         BNE   SUPX                                                             
         MVC   TCSUBPNH,TAECNET    NEED TO OVERRIDE SUBJ. TO P&H                
         OI    TCINPUT,TCINPNH     LET RATE CALC. KNOW                          
         SPACE 1                                                                
SUPX     B     XIT                                                              
         EJECT                                                                  
*              SET USE TYPE DETAILS FOR PAYMENT                                 
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
SETUSE   NTR1                                                                   
         MVC   TGTHREE,=C'BSS'     SET FOR TV SESSION                           
         MVI   TGBYTE,0                                                         
         SPACE 1                                                                
         CLC   TEOADST,ZEROES      IF ADDENDUM STATE DEFINED                    
         BE    *+14                                                             
         MVC   TGTHREE,=C'ADT'     SET TO PAY ADDENDUM TV 13W SESSION           
         MVI   TGBYTE,UADT13W                                                   
         SPACE 1                                                                
         CLI   TEMEDIA,TEMEDRAD    IF MEDIA IS RADIO                            
         BNE   SUS20                                                            
         MVC   TGTHREE,=C'BSR'     SET TO PAY RADIO SESSION                     
         SPACE 1                                                                
         CLC   TEOADST,ZEROES      IF ADDENDUM STATE DEFINED                    
         BE    *+14                                                             
         MVC   TGTHREE,=C'ADO'     SET TO PAY ADDENDUM RADIO 13W SESS.          
         MVI   TGBYTE,UADO13W                                                   
         SPACE 1                                                                
SUS20    CLC   TEOUSE,ZEROES       USE OVERRIDE USE TYPE IF DEFINED             
         BE    *+16                                                             
         MVC   TGTHREE,TEOUSE                                                   
         MVC   TGBYTE,TEOUTYP                                                   
         SPACE 1                                                                
         TM    TAECSTAT,TAECSDEM   TEST FORCE PAY DEMO RATES                    
         BZ    *+14                                                             
         MVC   TGTHREE,=C'DEM'                                                  
         MVI   TGBYTE,0                                                         
         SPACE 1                                                                
         CLI   TAECTYPE,TAECTYPU   IF TYPE IS MUSIC                             
         BE    *+12                                                             
         TM    TAECSTAT,TAECSHNW   OR IF H&W INPUT                              
         BZ    *+14                                                             
         MVC   TGTHREE,=C'BSM'     SET MUSIC SESSION USE TYPE                   
         MVI   TGBYTE,0                                                         
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,TGTHREE,TGBYTE  SET USE TYPE DETAILS                 
         B     XIT                                                              
         EJECT                                                                  
*              SET SESSION DETAILS ELEMENT FOR PAYMENT                          
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
SETTASD  NTR1                                                                   
         CLI   TAECTYPE,TAECTYPM   GET OUT IF MISCELLANEOUS                     
         BE    SSDX                                                             
         LA    R4,TETASD           R4=A(SESSION DETAILS ELEMENT)                
         ST    R4,TCATASD          PASS ITS ADDRESS TO SYSCALC                  
         USING TASDD,R4                                                         
         MVI   TASDEL,TASDELQ      BUILD THE ELEMENT                            
         MVI   TASDLEN,TASDLNQ                                                  
         MVC   TASDEQU,TGUSEQU     SET SESSION TYPE                             
         SPACE 1                                                                
         CLI   TGUSEQU,UBSM        IF USE TYPE IS MUSIC SESSION                 
         BNE   SSD20                                                            
         MVC   TASDMSP,TAECSP      SPOTS                                        
         MVC   TASDMHM,TAECHRM     HOURS/MINUTES                                
         B     SSDX                                                             
         SPACE 1                                                                
SSD20    CLI   TEMEDIA,TEMEDTV     ELSE IF MEDIA IS TV                          
         BNE   SSD40                                                            
         MVC   TASDSP,TAECSP       SPOTS                                        
         MVC   TASDDAY,TAECDAY     DAYS                                         
         MVC   TASDOT,TAECOT       OVERTIME                                     
         MVC   TASDDT,TAECDT       DOUBLETIME                                   
         MVC   TASDTRV,TAECTRV     TRAVEL TIME                                  
         MVC   TASDPDW,TAECPDW     PRIOR-DAY WARDROBE TIME                      
         MVC   TASDTAG,TAECTAG     TAGS                                         
         B     SSDX                                                             
         SPACE 1                                                                
SSD40    MVC   TASDRSP,TAECSP      RADIO - SPOTS                                
         MVC   TASDRHM,TAECHRM             HOURS/MINUTES                        
         MVC   TASDRTG,TAECTAG             TAGS                                 
         SPACE 1                                                                
SSDX     B     XIT                                                              
         EJECT                                                                  
*              SET UP CAST DETAILS FOR PAYMENT                                  
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
SETCAST  NTR1                                                                   
         MVC   TGCAT,=C'P  '       SET PRINCIPAL PERFORMER                      
         CLC   TAECCAT,ZEROES      IF CATEGORY OVERRIDE DEFINED                 
         BE    *+10                                                             
         MVC   TGCAT,TAECCAT       USE IT                                       
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,TGCAT   SET CATEGORY DETAILS                         
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(1,TCCAFCYC) SET CAST FFC TO TODAY TO          
*                  (TGTODAY1 NOT SET)         GET LATEST RATES FOR              
*                                             CONTRACTS WITH EFF DATES          
         MVC   TCCAONOF,=C'OFF'    ASSUME OFF CAMERA                            
         CLI   TAECTYPE,TAECTYPO   UNLESS ELEMENT TYPE IS ON CAMERA             
         BE    SETC20                                                           
         TM    TAECSTAT,TAECSONC   OR FORCE ON-CAMERA                           
         BO    SETC20                                                           
         CLI   TAECTYPE,TAECTYPX   OR EXTRA                                     
         BNE   *+10                                                             
SETC20   MVC   TCCAONOF,=C'ON '    THEN SET ON CAMERA                           
         SPACE 1                                                                
         MVC   TGONOF,TCCAONOF     SAVE IN GLOBAL FOR TAINTER                   
         SPACE 1                                                                
         MVC   TGUNI,=C'AFT'       DEFAULT UNION IS AFT                         
         CLI   TAECTYPE,TAECTYPU   IF THIS IS MUSIC                             
         BE    *+12                                                             
         TM    TAECSTAT,TAECSHNW   OR IF H&W AMOUNT INPUT                       
         BZ    *+10                                                             
         MVC   TGUNI,=C'AFM'       UNION IS AFM                                 
         TM    TAECSTAT,TAECSNON   IF FORCE NON-UNION                           
         BZ    *+10                                                             
         MVC   TGUNI,=C'NON'       UNION IS NON                                 
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,TGUNI   SET UNION DETAILS                            
         SPACE 1                                                                
         MVC   BYTE,TEOYEAR        SET OVERRIDE CONTRACT YEAR                   
         CLI   BYTE,0                                                           
         BNE   SETC40                                                           
         LA    R1,TGUNYR+L'TGUNYR-1  R1=A(VALID YEARS FOR THIS UNION)           
         CLI   0(R1),0                                                          
         BNE   *+8                 SHUFFLE BACK TO FIND LATEST ONE              
         BCT   R1,*-8                                                           
         MVC   BYTE,0(R1)          SAVE CONTRACT YEAR                           
         SPACE 1                                                                
SETC40   GOTO1 YRVAL,DMCB,(X'80',BYTE)  SET YEAR DETAILS                        
         SPACE 1                                                                
         MVC   TCCADBL,TAECDBL     N'DOUBLES                                    
         MVI   TCW4TYPE,TAW4TYIN   SET INDIVIDUAL                               
         MVC   TCOV1+2(2),TAECOV1  OVERSCALE RATE 1                             
         MVC   TCOV2+2(2),TEOMULT  MOVE MULTIPLIER TO 2ND OVSCALE RATE          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES RATES AND SAVES IN EST. CAST EL.              
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
CALC     NTR1                                                                   
         GOTO1 TASYSCLC,DMCB,(RC),TCD,SYSCOMM  CALCULATE RATES                  
         SPACE 1                                                                
         TM    TAECSTAT,TAECSPNH   IF P&H WAS OVERRIDDEN                        
         BZ    *+10                                                             
         MVC   TCPNH,TAECPNH       USE IT                                       
         SPACE 1                                                                
         TM    TAECSTAT,TAECSHNW   IF H&W WAS OVERRIDDEN                        
         BZ    *+14                                                             
         MVC   TCHNW,TAECHNW       USE IT                                       
         B     CLC20                                                            
         SPACE 1                                                                
         CLI   TAECTYPE,TAECTYPU   ELSE IF THIS IS MUSIC                        
         BNE   CLC20                                                            
         TM    TAECSTAT,TAECSNET   AND NET WAS OVERRIDDEN                       
         BZ    CLC20                                                            
         ZIC   R1,TAECNUM          NEED TO CALCULATE ADDL FIXED H&W             
         BCTR  R1,0                BECAUSE WON'T LOOP LATER FOR N'MUS.          
         MH    R1,TCFXDHNW                                                      
         A     R1,TCHNW            ADD TO H&W AMOUNT FROM SYSCALC               
         ST    R1,TCHNW                                                         
         SPACE 1                                                                
CLC20    BAS   RE,CALCTNH          CALCULATE T&H                                
*                                  RETURNS TAX=DUB(4), HND=DUB+4(4)             
*                                          EMSFEE=FULL                          
         SPACE 1                                                                
         XC    TAECAMTS(TAECAMTL),TAECAMTS   CLEAR ACCUMS IN ELEMENT            
         SPACE 1                                                                
         ZIC   R2,TAECNUM          R2=N'CAST OF THIS TYPE                       
CLC40    AF    TAECNET,TCPAY       SET NET AMOUNT                               
         AF    TAECPNH,TCPNH           PENSION AND HEALTH                       
         AF    TAECHNW,TCHNW           HEALTH AND WELFARE                       
         AF    TAECTAX,DUB             TAX                                      
         AF    TAECHND,DUB+4           HANDLING                                 
         AF    TAECEMSF,FULL           EMS FEE                                  
         SPACE 1                                                                
         TM    TAECSTAT,TAECSNET   IF OVERRIDE AMOUNT INPUT                     
         BO    *+8                 DON'T BOTHER LOOPING                         
         BCT   R2,CLC40            ELSE LOOP FOR N'CAST                         
         SPACE 1                                                                
         CLI   TAECTYPE,TAECTYPU   IF THIS IS MUSIC                             
         BNE   CLCX                                                             
         L     R1,TAECNET          ADD CARTAGE (BACK) TO NET FOR                
         ICM   R0,15,TAECCART      DISPLAY PURPOSES                             
         AR    R1,R0                                                            
         ST    R1,TAECNET                                                       
         SPACE 1                                                                
CLCX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES TAX AND HANDLING                              
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
CALCTNH  NTR1                                                                   
         MVC   DUB(4),TAECTAX      SAVE TAX AMOUNT                              
         MVC   DUB+4(4),TAECHND    SAVE HANDLING AMOUNT                         
         MVC   FULL,TAECEMSF       SAVE EMS FEE                                 
         SPACE 1                                                                
         TM    TAECSTAT,TAECSTAX   IF TAX NOT OVERRIDDEN                        
         BO    CTNH10                                                           
         XR    R1,R1               CALCULATE TAX                                
         SPACE 1                                                                
         CLI   TGBTYPE,TABRTY11    NO TAX FOR TYPE 11                           
         BE    CTNH5                                                            
         A     R1,TCPAY            BASE ON GROSS                                
         CLI   TGBTYPE,TABRTYE     IF TYPE E (EMS)                              
         BE    *+8                 DON'T INCLUDE H&W UNTIL HANDLING             
         A     R1,TCHNW                                                         
         LH    R0,TETAXR           R0=TAX RATE                                  
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
CTNH5    ST    R1,DUB              RETURN TAX IN DUB                            
         SPACE 1                                                                
CTNH10   TM    TAECSTAT,TAECSHND   IF HANDLING NOT OVERRIDDEN                   
         BO    CTNHX                                                            
         XR    R1,R1               CALCULATE HANDLING                           
         CLI   TGBTYPE,TABRTY1     NO HANDLING FOR TYPE 1                       
         BE    CTNH40                                                           
         CLI   TGBTYPE,TABRTY2                  OR TYPE 2                       
         BE    CTNH40                                                           
         CLI   TGBTYPE,TABRTY20                 OR TYPE 20                      
         BE    CTNH40                                                           
         A     R1,TCPAY            BASE ON GROSS                                
         A     R1,TCHNW                                                         
         CLI   TGBTYPE,TABRTY6     IF TYPE 6                                    
         BE    CTNH20                                                           
         CLI   TGBTYPE,TABRTY8     OR TYPE 8                                    
         BE    CTNH20                                                           
         CLI   TGBTYPE,TABRTY18    OR TYPE 18                                   
         BE    CTNH25                                                           
         CLI   TGBTYPE,TABRTY21    OR TYPE 21                                   
         BE    CTNH20                                                           
         CLI   TGBTYPE,TABRTY11    OR TYPE 11                                   
         BE    CTNH20                                                           
         CLI   TGBTYPE,TABRTY23    OR TYPE 23                                   
         BE    CTNH25                                                           
         CLI   TGBTYPE,TABRTYE     OR TYPE E (EMS)                              
         BNE   CTNH30                                                           
CTNH20   A     R1,DUB              INCLUDE TAX                                  
CTNH25   A     R1,TCPNH                    P&H                                  
*                                                                               
CTNH30   LH    R0,TEHNDR           R0=HANDLING RATE                             
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
CTNH40   ST    R1,DUB+4            RETURN HANDLING IN DUB+4                     
         SPACE 1                                                                
CTNH50   CLI   TGBTYPE,TABRTY20    IF BILLING TYPE 20                           
         BNE   CTNH60                                                           
         L     R1,TCPAY            BASE ON GROSS                                
         XR    R0,R0                                                            
         LHI   RE,750              TAKE 7.5% TALENT MASTERS FEE                 
         MR    R0,RE                                                            
         LHI   RF,5000                                                          
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,DUB+4            AND STORE IN HANDLING                        
         SPACE 1                                                                
CTNH60   CLI   TGBTYPE,TABRTYE     IF TYPE E (EMS)                              
         BNE   CTNHX               CALCULATE EMS FEE                            
         XR    R1,R1                                                            
         A     R1,TCPAY            GROSS                                        
         A     R1,TCHNW            HNW                                          
         A     R1,TCPNH            P&H                                          
         A     R1,DUB              TAX                                          
         A     R1,DUB+4            HANDLING                                     
         LH    R0,TETLEMSR         R0=EMS RATE                                  
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,FULL             RETURN EMS FEE IN FULL                       
         SPACE 1                                                                
CTNHX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES UPDATING INTERFACE BLOCK FOR PRODUCTION          
         SPACE 1                                                                
         USING TAECD,R5            R5=A(ESTIMATE CAST ELEMENT)                  
INTFACE  NTR1                                                                   
         CLC   TEIFEL,ZEROES       ONLY IF INTERFACE DETAILS DEFINED            
         BE    INTX                                                             
         LA    R2,BLOCK            R2=A(TAINTER PARAMETER BLOCK)                
         USING TND,R2                                                           
         XC    TND(TNLNQ),TND                                                   
         ST    RC,TNRC             A(GEND)                                      
         LA    R1,SYSCOMM          A(SYSTEM COMMON ROUTINES)                    
         ST    R1,TNASYCOM                                                      
         LA    R1,TEWCTAB          A(WORK-CODE TABLE)                           
         ST    R1,TNAWCTAB                                                      
         LA    R1,TEIFEL           A(INTERFACE ELEMENT)                         
         ST    R1,TNAIFEL                                                       
         SPACE 1                                                                
         L     R1,TAECNET          PAYMENT                                      
         A     R1,TAECHNW          + H&W                                        
         ST    R1,TNPAY                                                         
         MVC   TNPNH,TAECPNH       P&H                                          
         MVC   TNTAX,TAECTAX       PAYROLL TAXES                                
         MVC   TNHAND,TAECHND      HANDLING                                     
         SPACE 1                                                                
         GOTOR =V(TAINTER),DMCB,TND,RR=TEARELO UPDATE WORK-CODE TABLE           
         SPACE 1                                                                
INTX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS/ADDS ESTIMATE TOTALS ELEMENT                      
         SPACE 1                                                                
*                                R0=NET, R1=P&H, R2=H&W, R3=TAX, R4=HND         
         DROP  R4                  (NORMALLY USE R4 FOR TAETD)                  
BLDTOTS  NTR1                                                                   
         LA    R5,TEETEL           R5=A(LOCAL ESTIMATE TOTALS ELEMENT)          
         USING TAETD,R5                                                         
         MVC   TAETEL,TEETELCD     SET ELEMENT CODE                             
         MVI   TAETLEN,TAETLNQ     AND LENGTH                                   
         SPACE 1                                                                
         TM    TAETSTAT,TAETSNET   UNLESS OVERRIDDEN                            
         BO    *+8                                                              
         ST    R0,TAETNET          SAVE TOTAL NET IN ELEMENT                    
         SPACE 1                                                                
         TM    TAETSTAT,TAETSPNH   UNLESS OVERRIDDEN                            
         BO    *+8                                                              
         ST    R1,TAETPNH          SAVE TOTAL P&H IN ELEMENT                    
         SPACE 1                                                                
         TM    TAETSTAT,TAETSHNW   UNLESS OVERRIDDEN                            
         BO    *+8                                                              
         ST    R2,TAETHNW          SAVE TOTAL H&W IN ELEMENT                    
         SPACE 1                                                                
         TM    TAETSTAT,TAETSTNH   UNLESS OVERRIDDEN                            
         BO    *+12                                                             
         ST    R3,TAETTAX          SAVE TOTAL TAX IN ELEMENT                    
         ST    R4,TAETHND          SAVE TOTAL HANDLING IN ELEMENT               
*                                                                               
         CLI   TGBTYPE,TABRTYE     IF BILLING TYPE E                            
         BNE   *+10                                                             
         MVC   TAETEMSF,TEEMSACC    SAVE EMS FEE IN ELEMENT                     
         SPACE 1                                                                
**       TM    TAETSTAT,TAETSTOT   UNLESS OVERRIDDEN                            
**       BO    BTOT40                                                           
**       L     RF,TAETNET          ADD UP ALL ACCUMS                            
**       A     RF,TAETPNH                                                       
**       A     RF,TAETHNW                                                       
**       A     RF,TAETTNH                                                       
**       ST    RF,TAETTOT          SAVE TOTAL GROSS IN ELEMENT                  
         SPACE 1                                                                
BTOT40   MVC   ELEMENT(TAETLNQ),TAETEL                                          
         BAS   RE,MYADDL           ADD TO RECORD                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS/ADDS WORK-CODE SUMMARY ELEMENT                    
         SPACE 1                                                                
BLDWC    NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          R4=A(WORK-CODE SUMMARY ELEMENT)              
         USING TAWCD,R4                                                         
         MVC   TAWCEL,TEWCELCD     SET ELEMENT CODE                             
         LA    R3,TAWCSBEL                                                      
         USING TAWCSBEL,R3         R3=A(FIRST SUB-ELEMENT)                      
         SPACE 1                                                                
         XR    R2,R2               R2=SUB-ELEMENT COUNT                         
         LA    R5,TEWCTAB          R5=A(WORK-CODE TABLE)                        
         USING WCD,R5                                                           
         LA    R0,L'TEWCTAB/WCLNQ  R0=MAX N'ENTRIES IN TABLE                    
         SPACE 1                                                                
WC20     CLC   WCD(WCLNQ),ZEROES   TEST REACHED END OF TABLE                    
         BE    WC30                                                             
         MVC   TAWCWC,WCCODE       SAVE WORK-CODE                               
         MVC   TAWCAMT,WCAMT       AND AMOUNT                                   
         SPACE 1                                                                
         LA    R2,1(R2)            BUMP SUB-ELEMENT COUNT                       
         LA    R5,WCNEXT           BUMP TO NEXT TABLE ENTRY                     
         LA    R3,L'TAWCSBEL(R3)   AND TO NEXT SUB-ELEMENT                      
         BCT   R0,WC20                                                          
         SPACE 1                                                                
WC30     LTR   R2,R2               GET OUT IF NO SUB-ELEMENTS                   
         BZ    WCX                                                              
         STC   R2,TAWCNSUB         SAVE N'SUB-ELEMENTS                          
         MH    R2,=Y(L'TAWCSBEL)                                                
         LA    R2,TAWCLNQ(R2)                                                   
         STC   R2,TAWCLEN          SET L'ELEMENT                                
         SPACE 1                                                                
         BAS   RE,MYADDL           ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
WCX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 1                                                                
PREP     NTR1                                                                   
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R0,MYSPECS          SET A(SPECS)                                 
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R0,HEADHOOK                                                      
         SPACE 1                                                                
PR10     BAS   RE,DISPLAY          DISPLAY RECORD TO SCREEN                     
         SPACE 1                                                                
         L     R2,ATWA                                                          
         AH    R2,TEDSPTOT         R2=A(TOTALS LINE)                            
         SPACE 1                                                                
         MVI   BYTE,C'P'           PRINT ALL LINES UP TO TOTALS                 
         GOTO1 PRTSCRN,DMCB,TEAOVLYH,(R2),P-1                                   
         SPACE 1                                                                
         TM    TESTAT,TESTMORE     IF THERE'S MORE TO DISPLAY                   
         BO    PR10                GO DO IT                                     
         SPACE 1                                                                
         BAS   RE,PRNTIT           SKIP A LINE                                  
         SPACE 1                                                                
         MVI   BYTE,C'P'                                                        
         GOTO1 PRTSCRN,DMCB,(R2),0,P-1   PRINT TOTALS                           
         SPACE 1                                                                
         L     R1,TEAOVLYH                                                      
         TWAXC (R1),PROT=Y         CLEAR THE SCREEN                             
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVC   ELCODE,TEWCELCD     LOOK FOR WORK-CODE SUMMARY ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   PRX                                                              
         USING TAWCD,R4                                                         
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         ZIC   R2,TAWCNSUB         R2=N'SUB-ELEMENTS                            
         LA    R3,TAWCSBEL         R3=A(SUB-ELEMENTS)                           
         USING TAWCSBEL,R3                                                      
PR50     MVC   P(L'TAWCWC),TAWCWC                   PRINT WORK-CODE             
         EDIT  (4,TAWCAMT),(12,P+4),2,ZERO=NOBLANK  AND AMOUNT                  
         BAS   RE,PRNTIT                                                        
         LA    R3,L'TAWCSBEL(R3)                                                
         BCT   R2,PR50                                                          
         SPACE 1                                                                
PRX      B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK FOR REPORT PRINTING                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         L     R5,ASPOOLD          R5=SPOOL DSECT                               
         USING SPOOLD,R5                                                        
         L     R2,EFHTAG           SET TO PRINT HEADS FROM TAG FIELD            
         CLI   RCPROG,C'T'         IF THIS IS TALENT SYSTEM                     
         BNE   *+8                                                              
         BAS   RE,BUMP2            NEED TO BUMP PAST DESTINATION FIELD          
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,(R2),TEAOVLYH,H4-1                                  
         MVI   BYTE,C'P'           RESET TO PRINTING STATUS                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO ADD ELEMENTS FOR 'BIG' RECORDS                        
         SPACE 1                                                                
MYADDL   NTR1                                                                   
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,0                           
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         CLI   12(R1),5            TEST ERROR IS RECORD TOO LONG                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   14(2,R1),TEMAXLEN   INSURE MORE THAN OVERRIDE LEN, TOO           
         BH    ERRLONG                                                          
         B     XIT                                                              
         SPACE 3                                                                
*              FINISHING-UP ROUTINES                                            
         SPACE 1                                                                
FINAL    DS    0H                                                               
         MVC   USEIO,SVUSEIO                                                    
         MVC   LKEY,SVLKEY         RESTORE DETAILS OF DIRECTORY AND KEY         
         MVC   LSTATUS,TESVLSTA                                                 
         MVC   KEY,TESVKEY                                                      
         MVC   ASTARTSV,TESVSTSV   RESTORE A(SAVED STORAGE)                     
         BR    RE                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREND                                                           
*                                                                               
ERRLONG  MVI   ERROR,TOOLONG       RECORD TOO LONG                              
         B     ERRREC                                                           
*                                                                               
ERRNOMOR MVI   ERROR,ERNOMORE      NO MORE LINES AVAILABLE                      
         L     R2,TEAOVLYH                                                      
         BAS   RE,BUMPUN           CURSOR TO 1ST FIELD FOR TYPE                 
         MVC   TESCRN,TESVSCRN     RESTORE EXISTING SCREEN TABLE                
         B     ERREND                                                           
*                                                                               
ERRPFK   MVI   ERROR,ERINVPFK      PFKEY INVALID FOR THIS SCREEN                
         B     *+8                                                              
ERRPFKF  MVI   ERROR,ERINVPFF      PFKEY INVALID FOR THIS FIELD                 
         L     R2,ATWA                                                          
         AR    R2,R5               RETURN CURSOR TO SPOT WHERE IT WAS           
         B     ERREND                                                           
*                                                                               
ERRSW    MVI   ERROR,ERSWTAL       CAN'T SWITCH TO TAL SYSTEM                   
         B     ERRREC                                                           
*                                                                               
ERRAGY   MVI   OKNO,ERBADAGY       CAN'T FIND TALENT AGENCY                     
         MVI   BYTE,70             SET TALENT MESSAGE                           
         MVI   BLOCK,L'TETALAGY+1                                               
         MVC   BLOCK+1(L'TETALAGY),TETALAGY  DISPLAY AGENCY FOR USER            
         MVI   BLOCK+1+L'TETALAGY,0                                             
         L     R2,EFHREC           CURSOR TO RECORD TYPE FIELD                  
         B     ERRTXT                                                           
*                                                                               
ERRAMT   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     ERREND                                                           
*                                                                               
ERRUSEM  MVI   ERROR,ERUSEMED      INVALID USE FOR MEDIA                        
         B     ERREND                                                           
*                                                                               
PLSENTER MVI   OKNO,2              PLEASE ENTER REQUIRED FIELDS                 
         MVI   BYTE,X'FF'          SET GENERAL MESSAGE                          
         B     INFEND                                                           
*                                                                               
         USING T702FFD,R3          R3=A(TWA)                                    
PARTDISP MVI   BYTE,70             SET TALENT MESSAGE                           
         MVI   OKNO,81             RECORD CHANGED - HIT ENTER FOR MORE          
         CLI   MODE,DISPREC                                                     
         BNE   PDISPX                                                           
         MVI   OKNO,36             PARTIAL RECORD DISPLAYED                     
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTCHA                                                    
         BE    *+12                                                             
         CLI   THISLSEL,C'C'                                                    
         BNE   INFEND                                                           
         MVI   OKNO,82                          ... - NOW ENTER CHANGES         
PDISPX   L     R2,AFRSTREC                                                      
         B     INFEND                                                           
*                                                                               
ERRTXT   MVI   TGBYTE,GTMERR       SET ERROR TYPE                               
         B     *+8                                                              
INFEND   MVI   TGBYTE,GTMINF       SET INFORMATION TYPE                         
         OI    GENSTAT2,USGETTXT   SET USE GETTXT FOR MESSAGE                   
         XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMTYP,TGBYTE       SET MESSAGE TYPE                             
         MVC   GTMSGNO+1(1),OKNO   AND MESSAGE NUMBER                           
         MVC   GTMSYS,BYTE         AND MESSAGE SYSTEM                           
         LA    RE,BLOCK            IN CASE IT'S DEFINED                         
         STCM  RE,7,GTASUBST       SET A(SUBSTITUTION BLOCK)                    
         B     THEEND                                                           
         SPACE 1                                                                
ERRREC   L     R2,EFHREC           CURSOR TO RECORD TYPE FIELD                  
ERREND   CLI   TEERRDSP,0          DO I NEED TO OVERRIDE CURSOR POS.            
         BE    THEEND                                                           
         L     RE,SYSPARMS                                                      
         L     R1,0(RE)            R1=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,TEERRDSP   DISPLACMENT INTO FIELD                       
         MVI   TEERRDSP,0                                                       
         SPACE 1                                                                
THEEND   MVI   GETMSYS,70          SET TO USE TALENT SYSTEM MESSAGES            
         BAS   RE,FINAL            PROCESS FINISHING-UP ROUTINES                
         GOTO1 ERREX                                                            
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR3    XIT1  REGS=(R3)                                                        
XITR5    XIT1  REGS=(R5)                                                        
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         GETEL2 R5,DATADISP,ELCODE                                              
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
TAL1SE   EQU   X'10'               SE NUMBER OF TAL1                            
TAL2SE   EQU   X'20'               SE NUMBER OF TAL2                            
         SPACE 1                                                                
CORETAB  DC    AL1(QTASYSCA,QTASYSVL,QTASYSTB)                                  
NCORES   EQU   *-CORETAB                                                        
         SPACE 1                                                                
         LTORG                                                                  
ZEROES   DC    0XL255                                                           
         DC    255X'00'                                                         
         EJECT                                                                  
*              OPTIONS TABLE                                                    
         SPACE 1                                                                
OPTTAB   DS    0C                                                               
         DC    AL2(VALPNHR-T00A8C,DISPNHR-T00A8C),CL4'P&&HR'                    
         DC    AL2(VALTAXR-T00A8C,DISTAXR-T00A8C),CL4'TAXR'                     
         DC    AL2(VALHNDR-T00A8C,DISHNDR-T00A8C),CL4'HNDR'                     
         DC    AL2(VALTNHR-T00A8C,DISTNHR-T00A8C),CL4'BOTH'                     
         DC    AL2(VALYEAR-T00A8C,DISYEAR-T00A8C),CL4'YEAR'                     
         DC    AL2(VALUSE-T00A8C,DISUSE-T00A8C),CL4'USE'                        
         DC    AL2(VALMULT-T00A8C,DISMULT-T00A8C),CL4'MULT'                     
         DC    AL2(VALADST-T00A8C,DISADST-T00A8C),CL4'STA'                      
         DC    AL2(VALAFM-T00A8C,DISAFM-T00A8C),CL4'AFM'                        
         DC    X'FF'                                                            
         SPACE 3                                                                
*              SPECS FOR REPORT                                                 
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1                                                              
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,32,C'Session Estimate'                                        
         SSPEC H2,32,C'----------------'                                        
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H9,1,C'W/C       Amount'                                         
         SSPEC H10,1,C'---       ------'                                        
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              TWABLD ELEMENT TABLE FOR TOTALS LINE                             
*                                                                               
*        (TOTAL FIELDS CURRENTLY PROTECTED AS PER USER REQUEST -                
*        CODE SUPPORTS UNPROTECTING THEM, ALLOWING OVERRIDES.)                  
*                                                                               
TOTEL    DC    AL1(1,TWAELLNQ+TOTDATQ,23+X'80',2,TOTDATQ),X'28',AL1(0)          
TOTDAT   EQU   *                                                                
         DC    C'Net'                                                           
TOTDATQ  EQU   *-TOTDAT                                                         
         DC    AL1(1,TWAELLNQ,0,06,TE#TNETL),X'22',AL1(TE#TNET)  NET            
*                                                                               
         DC    AL1(1,TWAELLNQ+PNHDATQ,0,19,PNHDATQ),X'28',AL1(0)                
PNHDAT   EQU   *                                                                
         DC    C'P&&H'                                                          
PNHDATQ  EQU   *-PNHDAT                                                         
         DC    AL1(1,TWAELLNQ,0,23,TE#TPNHL),X'22',AL1(TE#TPNH)  P&H            
*                                                                               
         DC    AL1(1,TWAELLNQ+HNWDATQ,0,34,HNWDATQ),X'28',AL1(0)                
HNWDAT   EQU   *                                                                
         DC    C'H&&W'                                                          
HNWDATQ  EQU   *-HNWDAT                                                         
         DC    AL1(1,TWAELLNQ,0,38,TE#THNWL),X'22',AL1(TE#THNW)  H&W            
*                                                                               
         DC    AL1(1,TWAELLNQ+TNHDATQ,0,48,TNHDATQ),X'28',AL1(0)                
TNHDAT   EQU   *                                                                
         DC    C'T&&H'                                                          
TNHDATQ  EQU   *-TNHDAT                                                         
         DC    AL1(1,TWAELLNQ,0,52,TE#TTNHL),X'22',AL1(TE#TTNH)  T&H            
*                                                                               
         DC    AL1(1,TWAELLNQ+GRSDATQ,0,63,GRSDATQ),X'28',AL1(0)                
GRSDAT   EQU   *                                                                
         DC    C'Total'                                                         
GRSDATQ  EQU   *-GRSDAT                                                         
         DC    AL1(1,TWAELLNQ,0,69,TE#TGRSL),X'22',AL1(TE#TGRS)  GROSS          
         DC    X'00'                                                            
*                                                                               
TOTELLNQ EQU   *-TOTEL                                                          
         EJECT                                                                  
*              TV PFKEYS AND THEIR CORRESPONDING TYPES                          
         SPACE 1                                                                
PFKTTAB  DS    0C                                                               
         DC    AL1(TAECTYPO,L'PFKON+PFKLNQ,TEPFON-TEPFKEYS)                     
PFKON    DC    C'On'                                                            
         DC    AL1(TAECTYPF,L'PFKOFF+PFKLNQ,TEPFOFF-TEPFKEYS)                   
PFKOFF   DC    C'Off'                                                           
         DC    AL1(TAECTYPS,L'PFKSING+PFKLNQ,TEPFSING-TEPFKEYS)                 
PFKSING  DC    C'Singer'                                                        
         DC    AL1(TAECTYPX,L'PFKEXT+PFKLNQ,TEPFEXT-TEPFKEYS)                   
PFKEXT   DC    C'Extra'                                                         
         DC    AL1(TAECTYPU,L'PFKMUS+PFKLNQ,TEPFMUS-TEPFKEYS)                   
PFKMUS   DC    C'Music'                                                         
         DC    AL1(TAECTYPM,L'PFKMISC+PFKLNQ,TEPFMISC-TEPFKEYS)                 
PFKMISC  DC    C'Misc'                                                          
         DC    AL1(255,L'PFKCMNT+PFKLNQ,TEPFCMNT-TEPFKEYS)                      
PFKCMNT  DC    C'Comment'                                                       
         DC    AL1(255,L'PFKREP+PFKLNQ,TEPFREP-TEPFKEYS)                        
PFKREP   DC    C'Print'                                                         
         DC    X'00'                                                            
         SPACE 3                                                                
*              RADIO PFKEYS AND THEIR CORRESPONDING TYPES                       
         SPACE 1                                                                
PFKRTAB  DS    0C                                                               
         DC    AL1(TAECTYPR,L'PFKRAD+PFKLNQ,TEPFRAD-TEPFKEYS)                   
PFKRAD   DC    C'Anncr'                                                         
         DC    AL1(TAECTYPT,L'PFKRSNG+PFKLNQ,TEPFRSNG-TEPFKEYS)                 
PFKRSNG  DC    C'Singer'                                                        
         DC    AL1(TAECTYPU,L'PFKRMUS+PFKLNQ,TEPFMUS-TEPFKEYS)                  
PFKRMUS  DC    C'Music'                                                         
         DC    AL1(TAECTYPM,L'PFKRMISC+PFKLNQ,TEPFMISC-TEPFKEYS)                
PFKRMISC DC    C'Misc'                                                          
         DC    AL1(255,L'PFKRCMNT+PFKLNQ,TEPFCMNT-TEPFKEYS)                     
PFKRCMNT DC    C'Comment'                                                       
         DC    AL1(255,L'PFKRREP+PFKLNQ,TEPFREP-TEPFKEYS)                       
PFKRREP  DC    C'Print'                                                         
         DC    X'00'                                                            
         EJECT                                                                  
*              DEFAULT TV SCREEN CONFIGURATION TABLE                            
         SPACE 1                                                                
DEFSCTV  DS    0H                                                               
         DC    AL1(TAECTYPO,4),AL2(0),AL1(0)  4 ON-CAMERA                       
         DC    AL1(TAECTYPF,3),AL2(0),AL1(0)  3 OFF-CAMERA                      
         DC    AL1(TAECTYPS,2),AL2(0),AL1(0)  2 SINGERS                         
         DC    AL1(TAECTYPX,1),AL2(0),AL1(0)  1 EXTRA                           
         DC    AL1(0)                                                           
DEFSCTVQ EQU   *-DEFSCTV                                                        
         SPACE 3                                                                
*              DEFAULT RADIO SCREEN CONFIGURATION TABLE                         
         SPACE 1                                                                
DEFSCRA  DS    0H                                                               
         DC    AL1(TAECTYPR,4),AL2(0),AL1(0)  4 ANNOUNCERS                      
         DC    AL1(TAECTYPT,4),AL2(0),AL1(0)  4 RADIO SINGERS                   
         DC    AL1(TAECTYPU,3),AL2(0),AL1(0)  3 MUSICIANS                       
         DC    AL1(0)                                                           
DEFSCRAQ EQU   *-DEFSCRA                                                        
         EJECT                                                                  
*              DEFAULT LINE CONFIGURATION TABLE                                 
         SPACE 1                                                                
DEFLINE  DS    0C                                                               
*                                                                               
DEFON    DC    AL1(TAECTYPO,DEFONQ,FXATHREV+FXATCPNK)                           
         DC    AL1(TE#ON,TE#OV,TE#DEM,TE#SPT,TE#DAY,TE#OT,TE#DT)                
         DC    AL1(TE#TAG,TE#TRV,TE#PDW,TE#CCMT,TE#NET,0)                       
DEFONQ   EQU   *-DEFON                                                          
*                                                                               
DEFOFF   DC    AL1(TAECTYPF,DEFOFFQ,FXATHREV+FXATCGRN)                          
         DC    AL1(TE#OFF,TE#OV,TE#DEM,TE#SPT,TE#DAY,TE#2,TE#2)                 
         DC    AL1(TE#TAG,TE#TRV,TE#CCMT,TE#NET,0)                              
DEFOFFQ  EQU   *-DEFOFF                                                         
*                                                                               
DEFSING  DC    AL1(TAECTYPS,DEFSINGQ,FXATHREV+FXATCYEL)                         
         DC    AL1(TE#SING,TE#OV,TE#DEM,TE#SPT,TE#DAY,TE#CAT,TE#ONC)            
         DC    AL1(TE#CCMT,TE#NET,0)                                            
DEFSINGQ EQU   *-DEFSING                                                        
*                                                                               
DEFEXT   DC    AL1(TAECTYPX,DEFEXTQ,FXATHREV+FXATCBLU)                          
         DC    AL1(TE#EXT,TE#OV,TE#DEM,TE#SPT,TE#DAY,TE#OT,TE#DT)               
         DC    AL1(TE#CAT,TE#NON,TE#CCMT,TE#NET,0)                              
DEFEXTQ  EQU   *-DEFEXT                                                         
*                                                                               
DEFRAD   DC    AL1(TAECTYPR,DEFRADQ,FXATHREV+FXATCTUR)                          
         DC    AL1(TE#RAD,TE#OV,TE#SPT,TE#HRM,TE#TAG,TE#DEM,TE#CCMT)            
         DC    AL1(TE#NET,0)                                                    
DEFRADQ  EQU   *-DEFRAD                                                         
*                                                                               
DEFRSNG  DC    AL1(TAECTYPT,DEFRSNGQ,FXATHREV+FXATCYEL)                         
         DC    AL1(TE#RSNG,TE#OV,TE#SPT,TE#HRM,TE#TAG,TE#DEM,TE#CAT)            
         DC    AL1(TE#CCMT,TE#NET,0)                                            
DEFRSNGQ EQU   *-DEFRSNG                                                        
*                                                                               
DEFMUS   DC    AL1(TAECTYPU,DEFMUSQ,FXATHREV+FXATCPNK)                          
         DC    AL1(TE#MUS,TE#OV,TE#SPT,TE#HRM,TE#CAT,TE#DBL,TE#CART)            
         DC    AL1(TE#CCMT,TE#NET,0)                                            
DEFMUSQ  EQU   *-DEFMUS                                                         
*                                                                               
DEFMISC  DC    AL1(TAECTYPM,DEFMISCQ,FXATHREV+FXATCWHI)                         
         DC    AL1(TE#MISC,TE#NET,TE#PNH,TE#HNW,TE#TAX,TE#HND)                  
         DC    AL1(TE#CCMT,0)                                                   
DEFMISCQ EQU   *-DEFMISC                                                        
*                                                                               
         DC    X'00'                                                            
DEFLINEQ EQU   *-DEFLINE                                                        
         EJECT                                                                  
*              FIELD DEFINITION TABLE                                           
         SPACE 1                                                                
DEFFLD   DS    0C                                                               
         DC    AL1(TE#ON,FLDLNQ+L'LTON,L'LTON,TE#ONL)                           
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTON     DC    C'On-Cam '                                                       
*                                                                               
         DC    AL1(TE#OFF,FLDLNQ+L'LTOFF,L'LTOFF,TE#OFFL)                       
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTOFF    DC    C'Off-Cam'                                                       
*                                                                               
         DC    AL1(TE#SING,FLDLNQ+L'LTSING,L'LTSING,TE#SINGL)                   
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTSING   DC    C'Singers'                                                       
*                                                                               
         DC    AL1(TE#EXT,FLDLNQ+L'LTEXT,L'LTEXT,TE#EXTL)                       
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTEXT    DC    C'Extras '                                                       
*                                                                               
         DC    AL1(TE#RAD,FLDLNQ+L'LTRAD,L'LTRAD,TE#RADL)                       
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTRAD    DC    C'Anncrs '                                                       
*                                                                               
         DC    AL1(TE#RSNG,FLDLNQ+L'LTRSNG,L'LTRSNG,TE#RSNGL)                   
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTRSNG   DC    C'Singers'                                                       
*                                                                               
         DC    AL1(TE#MUS,FLDLNQ+L'LTMUS,L'LTMUS,TE#MUSL)                       
         DC    AL2(DISNUM-T00A8C,VALNUM-T00A8C)                                 
LTMUS    DC    C'Music  '                                                       
*                                                                               
         DC    AL1(TE#MISC,FLDLNQ+L'LTMISC,L'LTMISC,TE#MISCL)                   
         DC    AL2(DISMISC-T00A8C,VALMISC-T00A8C)                               
LTMISC   DC    C'Description'                                                   
*                                                                               
         DC    AL1(TE#OV,FLDLNQ+L'LTOV,L'LTOV,TE#OVL)                           
         DC    AL2(DISOV-T00A8C,VALOV-T00A8C)                                   
LTOV     DC    C'Ov%'                                                           
*                                                                               
         DC    AL1(TE#SPT,FLDLNQ+L'LTSPT,L'LTSPT,TE#SPTL)                       
         DC    AL2(DISSPT-T00A8C,VALSPT-T00A8C)                                 
LTSPT    DC    C'Sp'                                                            
*                                                                               
         DC    AL1(TE#DAY,FLDLNQ+L'LTDAY,L'LTDAY,TE#DAYL)                       
         DC    AL2(DISDAY-T00A8C,VALDAY-T00A8C)                                 
LTDAY    DC    C'Dy'                                                            
*                                                                               
         DC    AL1(TE#OT,FLDLNQ+L'LTOT,L'LTOT,TE#OTL)                           
         DC    AL2(DISOT-T00A8C,VALOT-T00A8C)                                   
LTOT     DC    C'Ot'                                                            
*                                                                               
         DC    AL1(TE#DT,FLDLNQ+L'LTDT,L'LTDT,TE#DTL)                           
         DC    AL2(DISDT-T00A8C,VALDT-T00A8C)                                   
LTDT     DC    C'Dt'                                                            
*                                                                               
         DC    AL1(TE#TRV,FLDLNQ+L'LTTRV,L'LTTRV,TE#TRVL)                       
         DC    AL2(DISTRV-T00A8C,VALTRV-T00A8C)                                 
LTTRV    DC    C'Travl'                                                         
*                                                                               
         DC    AL1(TE#PDW,FLDLNQ+L'LTPDW,L'LTPDW,TE#PDWL)                       
         DC    AL2(DISPDW-T00A8C,VALPDW-T00A8C)                                 
LTPDW    DC    C'Pd-Wd'                                                         
*                                                                               
         DC    AL1(TE#TAG,FLDLNQ+L'LTTAG,L'LTTAG,TE#TAGL)                       
         DC    AL2(DISTAG-T00A8C,VALTAG-T00A8C)                                 
LTTAG    DC    C'Tag'                                                           
*                                                                               
         DC    AL1(TE#HRM,FLDLNQ+L'LTHRM,L'LTHRM,TE#HRML)                       
         DC    AL2(DISHRM-T00A8C,VALHRM-T00A8C)                                 
LTHRM    DC    C'Hr.Mn'                                                         
*                                                                               
         DC    AL1(TE#CAT,FLDLNQ+L'LTCAT,L'LTCAT,TE#CATL)                       
         DC    AL2(DISCAT-T00A8C,VALCAT-T00A8C)                                 
LTCAT    DC    C'Cat'                                                           
*                                                                               
         DC    AL1(TE#DBL,FLDLNQ+L'LTDBL,L'LTDBL,TE#DBLL)                       
         DC    AL2(DISDBL-T00A8C,VALDBL-T00A8C)                                 
LTDBL    DC    C'Dbl'                                                           
*                                                                               
         DC    AL1(TE#DEM,FLDLNQ+L'LTDEM,L'LTDEM,TE#DEML)                       
         DC    AL2(DISDEM-T00A8C,VALDEM-T00A8C)                                 
LTDEM    DC    C'Dem'                                                           
*                                                                               
         DC    AL1(TE#ONC,FLDLNQ+L'LTONC,L'LTONC,TE#ONCL)                       
         DC    AL2(DISONC-T00A8C,VALONC-T00A8C)                                 
LTONC    DC    C'On?'                                                           
*                                                                               
         DC    AL1(TE#NON,FLDLNQ+L'LTNON,L'LTNON,TE#NONL)                       
         DC    AL2(DISNON-T00A8C,VALNON-T00A8C)                                 
LTNON    DC    C'Non'                                                           
*                                                                               
         DC    AL1(TE#CART,FLDLNQ+L'LTCART,L'LTCART,TE#CARTL)                   
         DC    AL2(DISCART-T00A8C,VALCART-T00A8C)                               
LTCART   DC    C'   Cartage'                                                    
*                                                                               
         DC    AL1(TE#CCMT,FLDLNQ+L'LTCCMT,L'LTCCMT,TE#CCMTL)                   
         DC    AL2(DISCCMT-T00A8C,VALCCMT-T00A8C)                               
LTCCMT   DC    C'Comment'                                                       
*                                                                               
         DC    AL1(TE#NET,FLDLNQ+L'LTNET,L'LTNET,TE#NETL)                       
         DC    AL2(DISNET-T00A8C,VALNET-T00A8C)                                 
LTNET    DC    C'Net Amount'                                                    
*                                                                               
         DC    AL1(TE#PNH,FLDLNQ+L'LTPNH,L'LTPNH,TE#PNHL)                       
         DC    AL2(DISPNH-T00A8C,VALPNH-T00A8C)                                 
LTPNH    DC    C'      P&&H'                                                    
*                                                                               
         DC    AL1(TE#HNW,FLDLNQ+L'LTHNW,L'LTHNW,TE#HNWL)                       
         DC    AL2(DISHNW-T00A8C,VALHNW-T00A8C)                                 
LTHNW    DC    C'    H&&W'                                                      
*                                                                               
         DC    AL1(TE#TAX,FLDLNQ+L'LTTAX,L'LTTAX,TE#TAXL)                       
         DC    AL2(DISTAX-T00A8C,VALTAX-T00A8C)                                 
LTTAX    DC    C'      Tax'                                                     
*                                                                               
         DC    AL1(TE#HND,FLDLNQ+L'LTHND,L'LTHND,TE#HNDL)                       
         DC    AL2(DISHND-T00A8C,VALHND-T00A8C)                                 
LTHND    DC    C' Handling'                                                     
*                                                                               
         DC    AL1(TE#CMNT,FLDLNQ+L'LTCMNT,L'LTCMNT,TE#CMNTL)                   
         DC    AL2(DISCMNT-T00A8C,VALCMNT-T00A8C)                               
LTCMNT   DC    C'Comment'                                                       
*                                                                               
         DC    AL1(TE#TNET,FLDLNQ,0,TE#TNETL)                                   
         DC    AL2(0,VALTNET-T00A8C)                                            
*                                                                               
         DC    AL1(TE#TPNH,FLDLNQ,0,TE#TPNHL)                                   
         DC    AL2(0,VALTPNH-T00A8C)                                            
*                                                                               
         DC    AL1(TE#THNW,FLDLNQ,0,TE#THNWL)                                   
         DC    AL2(0,VALTHNW-T00A8C)                                            
*                                                                               
         DC    AL1(TE#TTNH,FLDLNQ,0,TE#TTNHL)                                   
         DC    AL2(0,VALTTNH-T00A8C)                                            
*                                                                               
         DC    AL1(TE#TGRS,FLDLNQ,0,TE#TGRSL)                                   
         DC    AL2(0,VALTGRS-T00A8C)                                            
*                                                                               
         DC    AL1(TE#0,FLDLNQ,0,TE#0L)                                         
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(TE#2,FLDLNQ,0,TE#2L)                                         
         DC    AL2(0,0)                                                         
*                                                                               
         DC    X'00'                                                            
DEFFLDQ  EQU   *-DEFFLD                                                         
         EJECT                                                                  
UNITEST  NTR1  BASE=*,LABEL=*                                                   
       ++INCLUDE TAUNITEST                                                      
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TASYSESTD                                                      
         EJECT                                                                  
*              DSECT TO COVER OPTIONS TABLE                                     
         SPACE 1                                                                
OPTD     DSECT                                                                  
OPTDVAL  DS    H                   DISPLACMENT TO VALIDATION ROUTINE            
OPTDDIS  DS    H                   DISPLACMENT TO DISPLAY ROUTINE               
OPTLHS   DS    CL4                 LITERAL FOR LHS OF OPTION                    
OPTNEXT  EQU   *                                                                
         EJECT                                                                  
*              TALENT SYSTEM WORKING STORAGE                                    
         SPACE 1                                                                
TAWORKD  DSECT                                                                  
COREFACS DS    0V                  * CORE-RESIDENT PHASES *                     
TASYSCLC DS    V                   TALENT SYSTEM RATE CALCULATION RTNS.         
TASYSVAL DS    V                   TALENT SYSTEM VALIDATION ROUTINES            
TASYSTAB DS    V                   TALENT SYSTEM TABLES                         
         SPACE 1                                                                
       ++INCLUDE TASYSCALCD                                                     
         EJECT                                                                  
       ++INCLUDE TASYSVALD                                                      
         EJECT                                                                  
       ++INCLUDE TASYSWORKD                                                     
         SPACE 2                                                                
TAWORKLQ EQU   *-TAWORKD           L'TALENT SYSTEM WORKING STORAGE              
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         EJECT                                                                  
* TAGENFFD   (ANY SCREEN TO ALLOW DDGENTWA)                                     
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TAGENFILE  (USED FOR LABELS OF ELEMENTS)                                      
* TASYSEQUS  (USED FOR ERROR NUMBER EQUATES)                                    
* TASYSDSECT (USED FOR EQUATES OF TALENT GLOBAL STORAGE)                        
* FAGETTXTD                                                                     
* FATIOB                                                                        
* DDCOREQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060TASYSEST  11/22/13'                                      
         END                                                                    
