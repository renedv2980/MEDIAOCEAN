*          DATA SET TAGENAE    AT LEVEL 004 AS OF 04/08/14                      
*PHASE T702AEA,*                                                                
         TITLE 'T702AE - ADJUSTMENT MAINTENANCE'                                
T702AE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702AE                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES FOR ADJUSTMENT MAINTENANCE              
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE     INITIALIZE                              
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    INVRAC              INVALID RECORD ACTION                        
         CLI   ACTNUM,ACTREST      DON'T ALLOW RESTORE - IF DELETE              
         BE    INVRAC              THEN ADD WITH A DIFFERENT SSN                
*                                  THEN DELETE - & RESTORE WILL END UP          
*                                  RESTORING BOTH DELETED RECORDS               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,DISPKEY                                                     
         BE    DK                                                               
         CLI   THISLSEL,C'D'       IF DELETE FROM LIST                          
         BE    M05                 DON'T DISPLAY RECORD FIRST                   
         CLI   MODE,DISPREC                                                     
         BE    M20                                                              
*                                                                               
M05      CLI   MODE,RECDEL                                                      
         BE    M40                                                              
         CLI   MODE,RECREST                                                     
         BE    M40                                                              
         CLI   MODE,XRECPUT        IF MODE IS RECORD CHANGED                    
         BE    M10                                                              
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    M10                                                              
         CLI   MODE,XRECDEL        OR DELETED                                   
         BNE   M30                                                              
*                                                                               
M10      GOTO1 ADDPTRS,DMCB,INVPTRS  UPDATE PASSIVE POINTERS                    
*                                                                               
M20      BAS   RE,DISPLAY          (RE)DISPLAY RECORD                           
         CLI   MODE,XRECREST       IF RESTORED                                  
         BE    M25                                                              
         CLI   MODE,XRECDEL        OR DELETED                                   
         BNE   MX                                                               
*                                                                               
M25      BAS   RE,DELREST          DELETE/RESTORE ALL CHECKS TOO                
         B     MX                                                               
*                                                                               
M30      CLI   MODE,VALREC         TEST MODE IS VALIDATE RECORD                 
         BNE   MX                                                               
*                                                                               
M40      GOTO1 SAVPTRS,DMCB,INVPTRS  SAVE CURRENT PASSIVE POINTERS              
         L     R4,AIO                                                           
         MVC   SVKEY,0(R4)         SAVE INVOICE KEY                             
         CLI   MODE,RECDEL         IF DELETING                                  
         BNE   M70                                                              
         BAS   RE,CHKDEL           CHECK IF ALLOWED TO DELETE THIS ADJ          
         B     MX                                                               
*                                                                               
M70      CLI   MODE,VALREC                                                      
         BNE   MX                                                               
         BAS   RE,CHGREC                                                        
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALKEY                                                                 
*                                                                               
VK       MVC   SVAGY,TGAGY                                                      
         MVC   TGAGY,=C'999999'                                                 
         LA    R2,SADREFH                                                       
         CLI   5(R2),0             MUST INPUT REFERENCE NUMBER                  
         BNE   VK10                                                             
         MVC   TGAGY,SVAGY         RESTORE TGAGY                                
         B     MISSERR                                                          
*                                                                               
VK10     GOTO1 TINVCON,DMCB,SADREF,INV,DATCON CONVERT INV INPUT FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BNE   VK20                                                             
         MVC   TGAGY,SVAGY         RESTORE TGAGY                                
         B     INVERR                                                           
*                                                                               
VK20     XC    INV,ALLFF           COMPLEMENT INVOICE NUMBER                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'C0',INV) BUILD THE KEY                    
*                                                                               
VKX      MVC   TGAGY,SVAGY         RESTORE TGAGY                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY KEY FOR SELECT                                           
*                                                                               
         USING TLIND,R3                                                         
DK       L     R3,AIO                                                           
         MVC   INV,TLININV                                                      
         XC    INV,ALLFF           UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,INV,SADREF,DATCON CONVERT FOR DISPLAY               
         OI    SADREFH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*              CHANGE THE RECORD IN AIO - ONLY ERROR #                          
*                                                                               
         SPACE 1                                                                
         USING TAIND,R4                                                         
CHGREC   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   INVSTAT,TAINSTAT                                                 
*                                                                               
         LA    R2,SADERRTH         VALIDATE ERROR TYPE                          
         MVI   TAINTERR,0          CLEAR ERROR TYPE                             
         NI    TAINSTAT,X'FF'-TAINSERR  TURN OFF IN ERROR BIT                   
*                                                                               
         CLI   5(R2),0             IF THERE'S INPUT                             
         BE    CHGRX                                                            
         GOTO1 VALINUM                                                          
         MVC   TAINTERR,ACTUAL     SAVE ERROR TYPE IN INVOICE STAT EL           
         OI    TAINSTAT,TAINSERR   TURN ON IN ERROR BIT                         
*                                                                               
CHGRX    GOTO1  ACTVIN,DMCB,SADLCHGH  LAST CHANGED                              
         B     XIT                    DISPLAY WILL SET PREV VAL.                
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SADTYPEH                                                         
         XC    SADERRN,SADERRN                                                  
         OI    SADERRNH+6,X'80'                                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TAOIELQ      GET OLD AGY/INV ELEMENT                      
         USING TAOID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   D10                                                              
         MVC   SADOAGY,TAOIAGY                                                  
         GOTO1 TINVCON,DMCB,TAOIINV,SADOINV,DATCON CONVERT FOR DISPLAY          
*                                                                               
D10      L     R4,AIO                                                           
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS EL.                      
         BAS   RE,GETEL                                                         
         BNE   D20                 FOR ADJUSTMENT TYPE                          
         GOTO1 ADJOUT,DMCB,(TAPDADJS,SADTYPE)                                   
*                                                                               
D20      L     R4,AIO                                                           
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DX                                                               
         TM    TAINSTA2,TAINSFRC   IF FORCE TO LAST YEAR SET                    
         BZ    *+10                                                             
         MVC   SADCDTE,=C'LASTYEAR' DISPLAY SPECIAL LITERAL IN CHK DATE         
         OC    TAINCDTE,TAINCDTE                                                
         BZ    D30                                                              
         GOTO1 DATCON,DMCB,(1,TAINCDTE),(8,SADCDTE) CHECK DATE                  
*                                                                               
D30      OC    TAINCKRN,TAINCKRN                                                
         BZ    D40                                                              
         GOTO1 DATCON,DMCB,(1,TAINCKRN),(8,SADCRUN) CHECK RUN DATE              
*                                                                               
D40      CLI   TAINTERR,0          IF THERE'S AN ERROR                          
         BE    D50                                                              
         EDIT  (1,TAINTERR),(3,SADERRT),ALIGN=LEFT  SHOW ERROR NUMBER           
         GOTO1 ERROUT,DMCB,(TAINTERR,SADERRN)       AND DESCRIPTION             
*                                                                               
D50      LA    R2,TAINIID                                                       
         MVC   SADASST,TAINIST     ADJUSTMENT STAFF ID                          
         OC    TAINIDTE,TAINIDTE                                                
         BZ    D60                                                              
         GOTO1 DATCON,DMCB,(1,TAINIDTE),(8,SADASDA) ASSIGNMENT DATE             
*                                                                               
D60      OC    TAINITIM,TAINITIM   ASSIGNMENT TIME                              
         BZ    DX                                                               
         GOTO1 TIMECON,DMCB,TAINITIM,TAINIDTE,(8,SADASTI)                       
*                                                                               
DX       GOTO1 ACTVOUT,DMCB,SADLCHGH                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO DELETE/RESTORE AND ALL CHECK ASSOCIATED               
*              WITH AN ADJUSTMENT ( GENCON WILL PROCESS INVOICE)                
*                                                                               
DELREST  NTR1                                                                   
         NI    GENSTAT1,ALL-RDUPAPPL   WILL ALLOW RDUPDATE TO STAY C'Y'         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       SET READ DELETED RECORDS                     
*                                                                               
         LA    R4,SVKEY                                                         
         USING TLIND,R4            ADJUSTMENT RECORD                            
         LA    R3,KEY                                                           
         USING TLCKD,R3            GET ALL CHECKS FOR ADJUSTMENT                
         BAS   RE,SETCHK           SET CHECK FILE                               
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ      SET KEY                                      
         MVC   TLCKAGY,=C'999999'                                               
         MVC   TLCKINV,TLININV                                                  
         XC    TLCKINV,ALLFF                                                    
         GOTO1 HIGH                                                             
         B     DEL20                                                            
*                                                                               
DEL10    GOTO1 SEQ                                                              
*                                                                               
DEL20    CLC   TLCKKEY(TLCKSORT-TLCKD),KEYSAVE                                  
         BNE   DELX                                                             
         MVC   CHKEY,TLCKKEY       SAVE CHECK RECORD'S KEY TO GET NEXT          
         LA    R4,KEY                   ONE - SAV/ADDPTR CHANGES KEY            
         USING TLDRD,R4                                                         
         TM    MODE,XRECDEL        IF DELETE & RECORD IS                        
         BNO   DEL25                                                            
         TM    TLDRSTAT,X'80'      ALREADY DELETED                              
         BO    DEL10               CONTINUE                                     
*                                                                               
DEL25    OI    TLDRSTAT,X'80'      MARK IT DELETED                              
         CLI   MODE,XRECREST       IF RESTORING                                 
         BNE   DEL30                                                            
         NI    TLDRSTAT,X'7F'      UNMARK IT                                    
*                                                                               
DEL30    GOTO1 WRITE                                                            
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 GETREC                GET CHECK RECORD                           
         XC    CHKPTRS,CHKPTRS                                                  
         CLI   MODE,XRECREST         IF RESTORING                               
         BE    DEL40                                                            
         GOTO1 SAVPTRS,DMCB,CHKPTRS  DON'T SAVE CURRENT PASSIVE PTRS            
*                                                                               
DEL40    L     R2,AIO                                                           
         USING TLRCD,R2                                                         
         OI    TLRCSTAT,X'80'      MARK RECORD DELETED                          
         CLI   MODE,XRECREST       IF RESTORING                                 
         BNE   DEL60                                                            
         NI    TLRCSTAT,X'7F'      UNMARK IT                                    
*                                                                               
DEL60    GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,CHKPTRS DELETE/RESTORE ALL PASSIVE POINTERS         
         BAS   RE,UNVOID           IF VOID - 'UNVOID' ORIGINAL CHECK            
         MVC   KEY,CHKEY                                                        
         GOTO1 HIGH                RESTORE READ SEQ                             
         B     DEL10                                                            
*                                                                               
DELX     BAS   RE,SETTAL           SET TALENT FILE                              
         OI    GENSTAT1,RDUPAPPL   RESET STATUS FOR READ UPDATE                 
         NI    DMINBTS,X'F7'       & READ DELETED                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TESTS WHETHER THIS IS A VOIDED CHECK                     
*              IF SO - ON DELETE IT UNVOIDS THE ORIGINAL                        
*                    - ON RESTORE IT VOIDS THE ORIGINAL                         
*                                                                               
UNVOID   NTR1                                                                   
         L     R4,AIO1                                                          
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      FIND PAYMENT DETAILS                         
         BAS   RE,GETEL                                                         
         BNE   UVX                                                              
         TM    TAPDADJS,TAPDADVD   MAKE SURE THIS IS A VOID                     
         BNO   UVX                                                              
*                                                                               
         L     R4,AIO1                                                          
         USING TAOID,R4                                                         
         MVI   ELCODE,TAOIELQ      FIND ORIGINAL AGENCY/INVOICE                 
         BAS   RE,GETEL                                                         
         BNE   UVX                                                              
         LA    R3,KEY                                                           
         USING TLCKD,R3                                                         
         MVC   TLCKKEY,CHKEY       SET KEY                                      
         MVC   TLCKAGY,TAOIAGY     OVERWRITE ORIGINAL AGENCY/INVOICE            
         MVC   TLCKINV,TAOIINV                                                  
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'       RESET READ DELETED                           
         CLC   TLCKKEY(TLCKCAT-TLCKD),KEYSAVE                                   
         BNE   UVX                                                              
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET ORIGINAL CHECK RECORD                    
*                                                                               
         L     R4,AIO                                                           
         USING TACDD,R4                                                         
         MVI   ELCODE,TACDELQ      FIND CHECK DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   UVX                                                              
         CLI   MODE,XRECDEL        IF DELETING                                  
         BNE   UV20                                                             
         NI    TACDSTAT,X'FF'-TACDSVOI  TURN OFF VOID BIT                       
         B     UV30                                                             
*                                                                               
UV20     OI    TACDSTAT,TACDSVOI  ELSE TURN ON VOID BIT                         
*                                                                               
UV30     GOTO1 PUTREC                                                           
*                                                                               
UVX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TESTS WHETHER DELETE ALLOWED FOR AN INVOICE              
*                                                                               
CHKDEL   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAIND,R4                                                         
         TM    TAINSTAT,TAINSCHK   DON'T ALLOW DELETE IF CHECKS ARE             
         BO    NODELETE            WRITTEN                                      
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        SET SYSFIL/DIR TO CHECK FILE                                           
*                                                                               
         SPACE 1                                                                
SETCHK   MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*        SET SYSFIL/DIR TO TALENT FILE                                          
*                                                                               
         SPACE 1                                                                
SETTAL   MVC   SYSFIL,=C'TALFIL'                                                
         MVC   SYSDIR,=C'TALDIR'                                                
         BR    RE                                                               
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
INVRAC   MVI   ERROR,INVRCACT      INVALID RECORD ACTION                        
         B     THEEND                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
NODELETE MVI   ERROR,ERINVDEL      CAN'T DELETE                                 
         B     THEEND                                                           
*                                                                               
RECEND   LA    R2,CONRECH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
ALLFF    DC    6X'FF'              SHOULD BE SAME LENGTH AS LONGEST             
*                                  FIELD BEING COMPLEMENTED                     
PFTABLE  DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'ADJUST',CL8'LIST    '                                 
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRAED                                                       
         SPACE 3                                                                
         ORG   SADWORK                                                          
SVAGY    DS    CL6                 SAVED TGAGY                                  
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
INV      DS    CL6                 FIRST INVOICE NUM FOR RECORD TO ADD          
INVSTAT  DS    CL1                 INVOICE STATUS                               
DELSTAT  DS    XL1                 DELETE STATUS                                
PFTODEL  EQU   X'80'               HIT PFKEY TO DELETE                          
FRSTINV  DS    CL6                 FIRST INVOICE TO DELETE                      
MYKEY    DS    CL(L'KEY)           LAST KEY FOUND FOR DELETE                    
CHKEY    DS    CL(L'KEY)           LAST CHECK RECORD KEY                        
INVPTRS  DS    CL((6*L'TLDRREC)+1) INVOICE POINTER BLOCK                        
CHKPTRS  DS    CL((6*L'TLDRREC)+1) CHECK POINTER BLOCK                          
         DS    CL((6*L'TLDRREC)+1)                                              
         DS    CL((6*L'TLDRREC)+1)                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*                                                                               
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGENAE   04/08/14'                                      
         END                                                                    
