*          DATA SET TAGENDC    AT LEVEL 022 AS OF 07/12/05                      
*PHASE T702DCA,*                                                                
         TITLE 'T702DC - RCHECK MAINTENANCE (RETURNED CHECKS) '                 
T702DC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702DC                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    *+14                                                             
         MVC   SCKSHED(7),=C'Pid Num'                                           
         OI    SCKSHEDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,SETFILE        SET ALTERNATE FILE                           
         BNE   *+12                                                             
         BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    *+12                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
         BAS   RE,DISPLAY          (RE-)DISPLAY RECORD                          
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE A KEY                                        
*                                                                               
VKEY     NTR1                                                                   
         MVI   STATUS,0            PRE CLEAR PFKEY STATUS                       
         LA    R2,SCKCHKH          VALIDATE CHECK NUMBER                        
         CLI   5(R2),0             IF NOTHING INPUT                             
         BNE   VK10                                                             
         OC    TGCHK,TGCHK         AND THERE'S A GLOBAL NUMBER                  
         BZ    VK10                                                             
         MVC   8(L'TLCKCCHK,R2),TGCHK  USE IT                                   
         OI    6(R2),X'80'                                                      
         MVI   5(R2),L'TLCKCCHK                                                 
*                                                                               
VK10     GOTO1 ANY                 INPUT REQUIRED                               
         CLI   5(R2),L'TLCKCCHK    INSURE FULL CHECK NUMBER INPUT               
         BNE   ERRINV                                                           
         MVC   TGCHK,WORK          SAVE UNCOMPLEMENTED IN GLOBAL                
*                                                                               
         LA    R4,KEY              R4 = A(CHECK KEY)                            
         USING TLCKPD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKCCDQ    RECORD CODE                                  
         MVC   TLCKCCHK,TGCHK      CHECK NUMBER                                 
         XC    TLCKCCHK,ALLFF      COMPLEMENTED                                 
*                                                                               
         BAS   RE,SETCHK           SET FILE OVERRIDES                           
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BNE   ERRNOFND                                                         
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS ELEMENT)                
*                                                                               
         TM    TAPDADJS,TAPDADVD   IF CHECK IS A VOID CHECK                     
         BZ    VK90                                                             
         GOTO1 SEQ                 THEN BUMP TO NORMAL CHECK                    
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    VK80                                                             
         MVC   KEY,KEYSAVE         IF CAN'T FIND (I.E. ORIGINAL PURGED)         
         GOTO1 HIGH                SHOW VOID                                    
VK80     GOTO1 GETREC                                                           
*                                                                               
VK90     DS    0H                                                               
**NO-OP* CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
**NO-OP* BNE   VKX                                                              
**NO-OP* BAS   RE,TSTCHA           ERROR EXITS IF NO CHANGE ALLOWED             
*                                                                               
VKX      OI    SCKCHKH+4,X'20'     SET CHECK NUMBER FIELD VALID                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE GIVES ERROR IF NO CHANGE ALLOWED                         
*                                                                               
TSTCHA   NTR1                                                                   
         L     R4,AIO              R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4            R4=A(CHECK DETAILS ELEMENT)                  
*                                                                               
         OC    TACDCSH,TACDCSH     IF ALREADY CASHED                            
         BNZ   ERRCASH             ERROR                                        
         TM    TACDSTAT,TACDSVOI   IF ALREADY VOIDED                            
         BO    ERRVOI              ERROR                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE A RECORD                                     
*                                                                               
VREC     NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
*                                                                               
**NO-OP* BAS   RE,TSTCHA           ERROR EXITS IF NO CHANGE ALLOWED             
*                                                                               
         MVI   ELCODE,TARNELQ      DELETE CURRENT ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   PFAID,14            IF PRESS PFKEY TO DELETE                     
         BE    PFTDEL              GIVE PROMPT                                  
         TM    STATUS,WAITDPFK     IF WAITING FOR PROMPTED PFKEY                
         BZ    VREC1                                                            
         CLI   PFAID,20            AND IF PFKEY NOT PRESSED                     
         BNE   PFTDEL              GIVE MESSAGE AGAIN                           
         B     VREC40              ELSE, CONTINUE                               
*                                                                               
VREC1    XC    ELEMENT,ELEMENT     ADD NEW ELEMENT                              
         LA    R4,ELEMENT                                                       
         USING TARND,R4                                                         
         MVI   TARNEL,TARNELQ      SET ELEMENT CODE                             
         MVI   TARNLEN,TARNLNQ     SET ELEMENT LENGTH                           
*                                                                               
         LA    R2,SCKRDTEH         R2=A(CHECK RETURNED DATE)                    
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VREC2                                                            
         MVC   8(5,R2),=C'TODAY'   DEFAULT TO TODAY                             
         MVI   5(R2),5                                                          
         OI    6(R2),X'80'                                                      
VREC2    GOTO1 DTVAL,DMCB,TARNRDTE VALIDATE THE DATE                            
*                                                                               
         LA    R2,SCKUIDH          VALIDATE USER ID                             
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         MVC   AIO,AIO2                                                         
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         GOTO1 USERVAL,DMCB,SCKUIDH                                             
         MVC   AIO,AIO1                                                         
         BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         MVC   TARNUID,TGUSER                                                   
         B     VREC10                                                           
*                                                                               
VREC5    CLI   SCKSTAFH+5,0        VALIDATE BILLER STAFF ID                     
         BE    VREC15                                                           
VREC10   MVC   TGUSER,TARNUID      SET USER-ID FROM ELEMENT                     
         OC    TGUSER,TGUSER       IF NOT DEFINED                               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
         MVC   TARNUID,TGUSER      SAVE USER-ID IN ELEMENT                      
         NI    SCKSTAFH+4,X'DF'    INSURE WE ALWAYS RE-VALIDATE                 
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         GOTO1 RECVAL,DMCB,TLSTCDQ,SCKSTAFH                                     
         BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         MVC   TARNSTAF,TGSTAF     SET STAFF ID IN ELEMENT                      
*                                                                               
VREC15   LA    R2,SCKDDTEH         R2=A(DISPOSITION DATE)                       
         GOTO1 DTVAL,DMCB,(X'80',TARNDDTE)  VALIDATE THE DATE                   
*                                                                               
         LA    R2,SCKFILEH         R2=A(FILED FIELD)                            
         CLI   5(R2),0             IF INPUT                                     
         BE    VREC20                                                           
         CLI   8(R2),C'N'                                                       
         BE    VREC20                                                           
         CLI   8(R2),C'Y'          AND IT IS A YES                              
         BNE   ERRINV                                                           
         OC    TARNDDTE,TARNDDTE   IF NO DISPOSITION DATE INPUT                 
         BNZ   *+10                                                             
         MVC   TARNDDTE,TGTODAY1   DEFAULT TO TODAYS                            
         OI    TARNSTAT,TARNFILE   SET FILED                                    
*                                                                               
VREC20   LA    R2,SCKMAILH         R2=A(MAILED FIELD)                           
         CLI   5(R2),0             IF INPUT                                     
         BE    VREC30                                                           
         CLI   8(R2),C'N'                                                       
         BE    VREC30                                                           
         CLI   8(R2),C'Y'          AND IT IS A YES                              
         BNE   ERRINV                                                           
         TM    TARNSTAT,TARNFILE   MUST NOT BE FILED                            
         BO    ERRINV                                                           
         OC    TARNDDTE,TARNDDTE   IF NO DISPOSITION DATE INPUT                 
         BNZ   *+10                                                             
         MVC   TARNDDTE,TGTODAY1   DEFAULT TO TODAYS                            
         OI    TARNSTAT,TARNMAIL   SET MAILED                                   
*                                                                               
VREC30   GOTO1 ADDELEM                                                          
*                                  ADD RETURNED CHECK COMMENT IF ANY            
VREC40   GOTO1 NAMIN,DMCB,(2,TACMELQ),(X'80',SCKCOMH),TACMTYPR                  
*                                                                               
         GOTO1 ACTVIN,DMCB,(X'80',SCKLCHGH)     ADD ACTIVITY BY SCRN            
*                                                                               
VRECX    MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY RECORD                                        
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SCKSSNH             CLEAR THE SCREEN                             
         XC    SCKSTFN,SCKSTFN     PRE-CLEAR STAFF NAME                         
         OI    SCKSTFNH+6,X'80'                                                 
*                                                                               
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
*                                                                               
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         MVC   SCKSSN,TLCKSSN      GET SS NUMBER FROM CHECK KEY                 
         MVC   TGSSN,TLCKSSN                                                    
         TM    TGSYSTAT,TASYSPID   USING PID#?                                  
         BZ    DISP1                                                            
         MVC   SCKSSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TLCKSSN,SCKSSN   CONVERT SSN TO PID                 
         MVC   TGPID,SCKSSN                                                     
DISP1    OI    SCKSSNH+6,X'80'                                                  
*                                                                               
         MVC   AIO,AIO2            DISPLAY PERF NAME                            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TGSSN),SCKSSNNH                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   TGSSN,TLCKSSN                                                    
*                                                                               
         MVC   SCKCSH,=CL8'N/A'    DEFAULT-CASHED INFO NOT AVAILABLE            
         OI    SCKCSHH+6,X'80'                                                  
*                                                                               
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP5                                                            
         USING TACDD,R4            R4=A(CHECK DETAILS ELMENT)                   
         OC    TACDCSH,TACDCSH     IF CASHED DATE SPECIFIED                     
         BZ    DISP2                                                            
         GOTO1 DATCON,DMCB,(1,TACDCSH),(8,SCKCSH)  SHOW IT                      
         B     DISP5                                                            
*                                                                               
DISP2    TM    TACDSTAT,TACDSVOI   ELSE CHECK VOID                              
         BZ    *+14                                                             
         MVC   SCKCSH,=CL8'VOIDED'                                              
         B     DISP5                                                            
         TM    TACDSTAT,TACDSSTA   OR STALE                                     
         BZ    DISP5                                                            
         MVC   SCKCSH,=CL8'STALE'                                               
*                                                                               
         USING TACND,R4                                                         
DISP5    MVI   SCKSTOP,C'N'       DEFAULT-NOT STOPPED                           
         OI    SCKSTOPH+6,X'80'                                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TACNELQ                                                   
         BAS   RE,GETEL           IF STOP CANCEL ELEMENT                        
         B     *+8                EXISTS-NOT STOPPED                            
DISP6    BAS   RE,NEXTEL                                                        
         BNE   DISP7                                                            
         CLI   TACNTYPE,TACNTYPS                                                
         BE    DISP8                                                            
         B     DISP6                                                            
DISP7    L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ     OTHERWISE, IF STOP ELEMENT                    
         BAS   RE,GETEL           EXISTS-INDICATE STOPPED                       
         BNE   DISP8                                                            
         MVI   SCKSTOP,C'Y'                                                     
         DROP  R4                                                               
*                                                                               
DISP8    L     R4,AIO                                                           
         MVI   ELCODE,TARNELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP40                                                           
         USING TARND,R4            R4=A(RETURNED CHECK DETAILS)                 
*                                                                               
         OC    TARNRDTE,TARNRDTE   DISPLAY CHECK RETURNED DATE                  
         BZ    DISP10                                                           
         GOTO1 DATCON,DMCB,(1,TARNRDTE),(5,SCKRDTE)                             
         OI    SCKRDTEH+6,X'80'                                                 
*                                                                               
DISP10   OC    TARNUID,TARNUID     DISPLAY USER ID                              
         BZ    DISP15                                                           
         XC    WORK,WORK                                                        
         MVC   WORK+8(L'TARNUID),TARNUID                                        
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
         MVC   SCKUID,TGUSERID                                                  
         OI    SCKUIDH+6,X'80'                                                  
*                                                                               
DISP15   MVC   SCKSTAF,TARNSTAF    DISPLAY BILLER STAFF CODE                    
         OI    SCKSTAFH+6,X'80'                                                 
*                                  AND NAME                                     
         OC    TGUSER,TGUSER       IF USER NOT DEFINED FROM ABOVE               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'88',SCKSTAF),SCKSTFNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    TARNDDTE,TARNDDTE   DISPLAY DISPOSITION DATE                     
         BZ    DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TARNDDTE),(5,SCKDDTE)                             
         OI    SCKDDTEH+6,X'80'                                                 
*                                                                               
DISP20   MVI   SCKFILE,0                                                        
         TM    TARNSTAT,TARNFILE   DISPLAY FILED?                               
         BZ    *+8                                                              
         MVI   SCKFILE,C'Y'                                                     
         OI    SCKFILEH+6,X'80'                                                 
*                                                                               
         MVI   SCKMAIL,0                                                        
         TM    TARNSTAT,TARNMAIL   DISPLAY MAILED?                              
         BZ    *+8                                                              
         MVI   SCKMAIL,C'Y'                                                     
         OI    SCKMAILH+6,X'80'                                                 
*                                  DISPLAY RETURNED CHECK COMMENT               
DISP40   GOTO1 CHAROUT,DMCB,TACMELQ,(2,SCKCOMH),TACMTYPR                        
*                                  DISPLAY LAST CHANGED INFO                    
         GOTO1 ACTVOUT,DMCB,(X'80',SCKLCHGH)                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET SYSDIR/SYSFIL FOR NON-CHECKS                      
         SPACE                                                                  
SETTAL   DS    0H                                                               
         MVC   SYSDIR,SVSYSDIR     RESET FILE OVERRIDES                         
         MVC   SYSFIL,SVSYSFIL                                                  
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL TO CHECKS                           
         SPACE                                                                  
SETCHK   DS    0H                                                               
         MVC   SYSDIR,=CL8'CHKDIR' SET FILE OVERRIDES                           
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         SPACE 2                                                                
         GETEL  R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRVOI   MVI   ERROR,ERVOIDED      CHECK HAS ALREADY BEEN VOIDED                
         B     THEEND                                                           
*                                                                               
ERRCASH  MVI   ERROR,ERCASHED      CHECK HAS ALREADY BEEN CASHED                
         B     THEEND                                                           
*                                                                               
ERRMISSD LA    R2,SCKDDTEH         R2=A(DISPOSITION DATE FIELD)                 
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRNOFND MVI   ERROR,NOTFOUND      NOT FOUND                                    
         B     THEEND                                                           
*                                                                               
PFTDEL   MVI   MYMSGNO1,56         CONFIRM DELETE                               
         OI    STATUS,WAITDPFK     SET WAITING FOR DELETE PFKEY                 
         B     RECEND                                                           
*                                                                               
RECEND   L     R2,EFHREC                                                        
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CHECK   ',CL8'DISPLAY'                              
PF13     DC    AL1(KEYTYTWA,L'SCKCHK-1),AL2(SCKCHK-T702FFD)                     
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3'   ',CL8'        ',CL8'        '                             
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3'   ',CL8'        ',CL8'        '                             
PF20X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
ALLFF    DC    8X'FF'                                                           
         SPACE 2                                                                
*              LITERAL POOL                                                     
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDCD                                                       
         EJECT                                                                  
         ORG   SCKWORK                                                          
*                                                                               
SVTGSTAF DS    CL(L'TGSTAF)        SAVED STAFF ID                               
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
STATUS   DS    XL1                 PFKEY STATUS                                 
WAITDPFK EQU   X'80'               WAITING FOR DELETE PFKEY                     
*                                                                               
WSEND    EQU   *                                                                
         EJECT                                                                  
* TAGENFILE                                                                     
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022TAGENDC   07/12/05'                                      
         END                                                                    
