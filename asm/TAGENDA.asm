*          DATA SET TAGENDA    AT LEVEL 007 AS OF 07/20/12                      
*PHASE T702DAC                                                                  
         TITLE 'T702DA - PAGENT LIST'                                           
T702DA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702DA                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING PAGENTD,R7                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,0                                                   
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    PAGL10                                                           
         MVC   SPASHED(7),=C'Pid Num'                                           
         OI    SPASHEDH+6,X'80'                                                 
         SPACE 2                                                                
PAGL10   CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   PAGL20                                                           
         BAS   RE,VKEY             AND INITIALIZE INFO FOR SYSIO                
         B     XIT                                                              
         SPACE 3                                                                
PAGL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   PAGL30                                                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN IN CASE                   
         LA    R2,LISTAR           SELECT CREAMED                               
         B     PAGL40                                                           
         SPACE 3                                                                
PAGL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   PAGLX                                                            
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         MVI   LISTSW,C'F'                                                      
         XC    AGTTAB,AGTTAB                                                    
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         LA    R2,P                                                             
PAGL40   BAS   RE,LSTREC                                                        
         SPACE                                                                  
PAGLX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE                                                                  
LSTREC   NTR1                                                                   
         LA    R0,LRHOOK           SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         OI    TIQFLAGS,TIQFDIR    SET WANT DIRECTORY HOOK                      
         OI    TIQFLAG2,TIQFNLIM   SET DON'T WANT TO CHECK LIMIT ACCESS         
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVI   TIREAD,TLCKECDQ                                                  
         SPACE                                                                  
         GOTO1 PGCNTL,DMCB,PGTBL,TIKEY,TIQSKEY  SET KEY                         
         SPACE                                                                  
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE                                                                  
         XC    AGTTAB,AGTTAB       CLEAR AGENT TABLE                            
         CLI   TIERROR,TINOTOLN                                                 
         BNE   LSTR3                                                            
         LA    R2,CONRECH                                                       
         B     ONLINERR            CANNOT RUN ONLINE                            
         SPACE 1                                                                
LSTR3    XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LSTRX                                                            
         CP    COUNTER,=P'0'       AND IF ANYTHING REPORTED                     
         BE    LSTRX                                                            
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(15,R1),=C'AGENT RECORD(S)'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LSTRX                                                            
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE                                                                  
LSTRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE                                                                  
VKEY     NTR1                                                                   
         TM    SPASSNH+4,X'20'     VALIDATE SS NUMBER                           
         BO    VK5                                                              
         LA    R2,SPASSNH                                                       
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    VK1                                                              
         CLI   SPASSNH+5,0                                                      
         BE    FLDMISS                                                          
         CLI   SPASSNH+5,9         SSN ENTERED?                                 
         BE    VK1                                                              
         CLI   SPASSNH+5,6         PID ENTERED?                                 
         BNE   FLDINV                                                           
         MVC   TGPID,SPASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SPASSN,TGSSN                                                     
         MVI   SPASSNH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SPASSNH),SPASSNNH                     
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    VK2                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SPASSN,SPACES                                                    
         MVC   SPASSN(L'TGPID),TGPID                                            
         MVI   SPASSNH+5,L'TGPID                                                
         OI    SPASSNH+6,X'80'                                                  
*                                                                               
VK2      MVC   TIFSSN,TGSSN                                                     
         MVC   SVTGSSN,TGSSN       SAVE GLOBAL SSN - SELECT MIGHT CREAM         
         NI    SPAPDH+4,X'DF'                                                   
*                                                                               
VK5      LA    R2,SPAPDH           R2=A(PERIOD FIELD)                           
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         XC    STRTPD(6),STRTPD                                                 
         LA    R3,BLOCK            R3=A(PERVAL BLOCK)                           
         USING PERVALD,R3                                                       
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VK10                                                             
         MVC   BYTE,5(R2)          SET L'INPUT FIELD                            
         OI    BYTE,X'40'          SET TO VALIDATE AS MM/DD                     
         SPACE 1                                                                
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),('PVINSGLS',(R3))                       
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BO    STRTINV                                                          
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BO    ENDINV                                                           
         TM    4(R1),PVRCONE       IF BOTH DATES INPUT                          
         BO    VK7                                                              
         CLI   4(R1),PVRCOK        THEN TEST GOOD RETURN CODE                   
         BNE   FLDINV                                                           
         TM    PVALASSM,X'77'      IF ANY ASSUMPTIONS MADE                      
         BNZ   VK16                RE-DISPLAY GENERATED DATES                   
         B     VK18                                                             
         SPACE 1                                                                
VK7      OC    PVALCSTA,PVALCSTA   ONE DATE INPUT - IF NOT START                
         BZ    STRTMISS            GIVE START MISSING ERROR                     
         B     ENDMISS             ELSE END MISSING                             
         SPACE                                                                  
VK10     XC    DUB,DUB             NO PERIOD INPUT - FORCE LAST 6 MNTHS         
         MVC   DUB(7),=C'(-6M)-T'  BUILD FAKE INPUT FIELD                       
         GOTO1 PERVAL,DMCB,(7,DUB),(R3)                                         
         CLI   4(R1),PVRCOK        TEST GOOD RETURN CODE                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
VK16     MVC   8(17,R2),PVALCPER   RE-DISPLAY GENERATED DATES                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),17            SIMULATE L'INPUT                             
VK18     MVC   STRTPD(6),PVALPSTA  SAVE PWOS START AND END                      
         MVC   DPERIOD,PVALCPER    AND DISPLAYABLE PERIOD                       
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         NI    SPAAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
VK20     LA    R2,SPAAGYH          AGENCY FILTER                                
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         XC    TIFAGY,TIFAGY                                                    
         XC    SPAAGYN,SPAAGYN     CLEAR AGENCY NAME                            
         OI    SPAAGYNH+6,X'80'                                                 
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VK25                                                             
         GOTO1 RECVAL,DMCB,(X'80',TLAYCDQ),(X'08',SPAAGYH),SPAAGYNH             
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK25     OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         XC    KEY,KEY             KEY FIELD CHANGED - START OVER               
         MVI   LISTSW,C'F'                                                      
*                                                                               
VKX      MVI   NLISTS,16           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
*                                                                               
         CLI   LISTSW,C'F'         IF STARTING LIST OVER                        
         BNE   *+10                                                             
         XC    AGTTAB,AGTTAB       CLEAR TABLE OF DISPLAYED AGENTS              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LINED,R2                                                         
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROC DIR ONLY - DON'T NEED CHECK REC         
         BNE   XIT                                                              
         BAS   RE,FILTKEY          FILTER KEYS                                  
         BNE   NO                  SET CC FOR SYSIO                             
         SPACE                                                                  
         BAS   RE,DISPLAY          DISPLAY TO SCREEN                            
         BNE   LRHX                                                             
         CLI   MODE,PRINTREP       SKIP IF MODE IS PRINT REPORT                 
         BE    LRH20                                                            
         SPACE                                                                  
         GOTO1 LISTMON             DMDSKADD SET TO AGENT D/A                    
         B     LRHX                                                             
         SPACE                                                                  
LRH20    GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT RECORDS OUTPUT                         
         SPACE                                                                  
LRHX     ICM   R1,15,AENTRY        GET ADDRESS OF NEXT ENTRY                    
         BZ    *+10                                                             
         MVC   0(2,R1),AGENT       SAVE THIS AGENT IN AGTTAB                    
         B     NO                  SET CC NO FOR SYSIO - DON'T NEED CHK         
         EJECT                                                                  
*              ROUTINE FILTERS ON KEYS AT TIKEY.  RETURNS CC EQUAL              
*              IF OK TO PASS THIS RECORD, ELSE RETURNS CC NOT EQUAL.            
         SPACE                                                                  
         USING TLCKPD,R3                                                        
FILTKEY  NTR1                                                                   
         LA    R3,TIKEY                                                         
         CLI   TLCKEDTE,0          TEST HAVE CHECK DATE                         
         BE    FLTKNO                                                           
         MVC   FULL(3),TLCKEDTE                                                 
         XC    FULL(3),HEXFFS                                                   
         CLC   FULL(3),ENDPD       TEST CHECK DATE IS WITHIN PERIOD             
         BH    FLTKNO                                                           
         CLC   FULL(3),STRTPD                                                   
         BL    FLTKNO                                                           
         OC    TIAGT,TIAGT         TEST HAVE AGENT                              
         BZ    FLTKNO                                                           
         PACK  DUB,TIAGT           CONVERT TO BINARY                            
         CVB   R1,DUB                                                           
         STH   R1,AGENT            AND SAVE                                     
         BAS   RE,CHKDUP           CHECK FOR DUPLICATES                         
         BNE   FLTKNO                                                           
         B     YES                                                              
         SPACE                                                                  
FLTKNO   B     NO                                                               
         EJECT                                                                  
*              ROUTINE CHECKS TO SEE IF AGENT HAS ALREADY BEEN                  
*              DISPLAYED.  SETS CC EQUAL IF IT NEEDS TO BE DISPLAYED            
*              THIS TIME, ELSE SETS CC NOT EQUAL.  SETS AENTRY TO               
*              A(NEXT AVIALABLE AGTTAB ENTRY) OR X'00'S IF NO MORE ROOM         
         SPACE                                                                  
CHKDUP   NTR1                                                                   
         LA    R0,MAXAGT           R0=MAX NUMBER OF AGENTS                      
         LA    R1,AGTTAB           R1=A(TABLE OF DISPLAYED AGENTS)              
         SPACE                                                                  
CHKD5    OC    0(2,R1),0(R1)       IF NOT END OF ENTRIES                        
         BZ    CHKD20                                                           
         CLC   AGENT,0(R1)         IF AGENT MATCHES                             
         BE    NO                  SET CC NO                                    
         LA    R1,2(R1)            ELSE BUMP TO NEXT AGENT ENTRY                
         BCT   R0,CHKD5            KEEP LOOKING TILL END OF TABLE               
         XC    AENTRY,AENTRY       CLEAR AENTRY IF NO MORE ROOM                 
         B     YES                 ALWAYS DISPLAY                               
         SPACE                                                                  
CHKD20   ST    R1,AENTRY           SAVE A(NEXT AVAIL ENTRY)                     
         B     YES                 AND DISPLAY IT                               
         EJECT                                                                  
*              ROUTINE GETS AGENT RECORD AND DISPLAYS INFO                      
         USING LINED,R2            R2=A(OUTPUT AREA)                            
DISPLAY  NTR1                                                                   
         MVC   LINAGT,TIAGT        AGENT                                        
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A0',LINAGT)  GET AGENT RECORD             
         BNE   NO                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0   GET NAME                                
         MVC   LINAGTN,TGNAME                                                   
         MVC   LINTEL,SPACES       PRE-CLEAR TELEPHONE                          
         L     R4,AIO                                                           
         USING TAAND,R4                                                         
         MVI   ELCODE,TAANELQ      POINT TO AGENT ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DISPX                                                            
         OC    TAANTEL,TAANTEL     IF TELEPHONE GIVEN, DISPLAY IT               
         BZ    DISPX                                                            
         MVI   LINTEL,C'('                                                      
         MVC   LINTEL+1(3),TAANTEL AREA CODE                                    
         MVI   LINTEL+4,C')'                                                    
         MVC   LINTEL+5(3),TAANTEL+3                                            
         MVI   LINTEL+8,C'-'                                                    
         MVC   LINTEL+9(4),TAANTEL+6                                            
         SPACE                                                                  
DISPX    B     YES                                                              
         EJECT                                                                  
*              HEADHOOK ROUTINE                                                 
         SPACE 2                                                                
HDHOOK   NTR1                                                                   
         MVI   BYTE,C'H'           PRINT HEADLINES                              
         GOTO1 PRTSCRN,DMCB,CONTAGH,SPAHDH,H4-1                                 
         GOTO1 (RF),(R1),SPAHDH,SPASELH,H6-5                                    
         MVC   H6-5(5),SPACES      CLEAR SELECT FIELD                           
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
ONLINERR MVI   ERROR,NOTONLIN                                                   
         B     THEEND                                                           
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
STRTINV  MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
STRTMISS MVI   ERROR,ERMISSDT      MISSING START DATE                           
         B     THEEND                                                           
ENDMISS  MVI   ERROR,ERMISEDT      MISSING END DATE                             
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SPASELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 3                                                                
HEXFFS   DC    6X'FF'                                                           
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'PAGENT LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H7,1,C'----'                                                     
         SSPEC H7,7,C'----'                                                     
         SSPEC H7,45,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              LOCAL SAVED STORAGE                                              
PAGENTD  DSECT                                                                  
AENTRY   DS    A                   A(NEXT AVAILABLE ENTRY IN AGTTAB)            
COUNTER  DS    PL4                 COUNTER OF NUM OF RECORDS OUTPUT             
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
STRTPD   DS    XL3                 START AND                                    
ENDPD    DS    XL3                 END OF PERIOD                                
DPERIOD  DS    CL17                DISPLAYABLE PERIOD                           
PGTBL    DS    CL(16*L'TLRCKEY)    SAVED KEYS FOR PAGE CONTROL RTN.             
AGENT    DS    XL2                 BINARY AGENT CODE                            
MAXAGT   EQU   100                 MAX NUMBER OF AGENTS IN AGTTAB               
AGTTAB   DS    XL(MAXAGT*L'AGENT)  TABLE OF BINARY AGENT CODES ALREADY          
*                                  DISPLAYED ON LIST                            
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LINED    DSECT                                                                  
LINAGT   DS    CL4                                                              
         DS    CL2                                                              
LINAGTN  DS    CL36                                                             
         DS    CL2                                                              
LINTEL   DS    CL13                                                             
         SPACE                                                                  
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDAD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGENDA   07/20/12'                                      
         END                                                                    
