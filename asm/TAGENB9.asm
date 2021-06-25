*          DATA SET TAGENB9    AT LEVEL 004 AS OF 04/27/04                      
*PHASE T702B9A,*                                                                
         TITLE 'T702B9 - DUE COMPANY TRACKING MANUAL ADJUSTMENTS'               
T702B9   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B9                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    GT10                                                             
         MVC   SDTSHED(6),=C'Pid Num'                                           
         OI    SDTSHEDH+6,X'80'                                                 
*                                                                               
GT10     CLI   MODE,SETFILE        SET ALTERNATE FILE                           
         BNE   *+12                                                             
         BAS   RE,SETCHK                                                        
         B     XIT                                                              
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    GT20                                                             
         CLI   MODE,XRECADD        AFTER ADDING RECORD                          
         BE    GT20                                                             
         CLI   MODE,XRECPUT        AFTER CHANGING RECORD                        
         BNE   XIT                                                              
GT20     BAS   RE,DISPLAY          (RE-)DISPLAY IT                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        IF NOT JUST DISPLAYING                       
         BE    GTX                                                              
         GOTO1 AADDPTRS,DMCB,PTRBLK  UPDATE TRACKING RECORD POINTERS            
         SPACE 1                                                                
         BAS   RE,UPDATE           UPDATE DUE COMPANY RECORD                    
GTX      B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   ACTNUM,ACTDEL       DELETE NOW ALLOWED                           
         BE    ACTINV                                                           
         SPACE 1                                                                
         BAS   RE,SETTAL           SET TALENT FILES                             
*                                                                               
         LA    R2,SDTSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK1                                                              
         CLI   SDTSSNH+5,0                                                      
         BE    MISSERR                                                          
         CLI   SDTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK1                 RECVAL CALL DOES NOT CHECK FOR               
         CLI   SDTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SDTSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SDTSSN,TGSSN                                                     
         MVI   SDTSSNH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SDTSSNH),SDTSSNNH  S/S NUMBER         
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK5                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDTSSN,SPACES                                                    
         MVC   SDTSSN(L'TGPID),TGPID                                            
         MVI   SDTSSNH+5,6                                                      
         OI    SDTSSNH+6,X'80'                                                  
*                                                                               
VK5      MVC   AIO,AIO2                                                         
         LA    R2,SDTDUCH          DUE COMPANY CODE                             
         CLI   5(R2),0             IF NO INPUT,USE GLOBALS                      
         BNE   VK6                                                              
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'30',(R2))  TO READ REC FOR UPDATE         
         B     VK10                                                             
*                                                                               
VK6      GOTO1 ANY                                                              
         CLI   5(R2),6             TEST LENGTH IS 6                             
         BNE   VK8                                                              
         MVC   DUB(2),WORK+2       TEST IF VALID DATE                           
         MVC   DUB+2(2),=C'01'     CHANGE TO M/D/Y FORMAT                       
         MVC   DUB+4(2),WORK                                                    
         GOTO1 DATVAL,DMCB,(0,DUB),WORK+6                                       
         CLI   3(R1),0                                                          
         BE    VK8                 NOT VALID DATE                               
         MVC   WORK(4),WORK+6      YES - SO USE INTERNAL FORMAT                 
*                                                                               
VK8      GOTO1 RECVAL,DMCB,TLDUCDQ,(X'B0',WORK) GET REC TO UPD,NON-SCRN         
         BE    VK10                                                             
         NI    4(R2),X'DF'         INVALIDATE FLD IF ERROR                      
         B     ERRNOFND                                                         
VK10     OI    4(R2),X'20'         ELSE SET FIELD  PREVIOUSLY VALIDATED         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R3,AIO2                                                          
         MVI   ELCODE,TADUELQ      SET TO GET DUE COMP. DETAILS EL.             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R3            R3=A(DUE COMPANY DETAILS EL.)                
         L     R1,TADUDUE                                                       
         S     R1,TADUCOL                                                       
         ST    R1,DUCBAL           SAVE CURRENT BALANCE                         
         SPACE 1                                                                
         BAS   RE,DISPBAL          DISPLAY BALANCE                              
         SPACE 1                                                                
         BAS   RE,SETCHK           SET CHECK FILES                              
         BAS   RE,GETSEQ           GET LAST/NEXT SEQUENCE NUMBER                
         SPACE 1                                                                
         LA    R4,KEY              BUILD KEY                                    
         USING TLDTD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLDTCD,TLDTCDQ      RECORD CODE                                  
         MVC   TLDTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLDTDUC,TGDUC       DUE COMPANY CODE                             
         MVC   TLDTDTE,TGTODAY1    TODAY'S DATE                                 
         XC    TLDTDTE,HEXFFS      (COMPLEMENTED)                               
         MVC   TLDTSEQ,NEXTSEQ     NEXT SEQUENCE NUMBER (COMPLEMENTED)          
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       FINISHED IF ADDING                           
         BE    VKX                                                              
         MVC   TLDTDTE,PREVDTE     ELSE SET PREV DATE (COMP)                    
         MVC   TLDTSEQ,PREVSEQ          AND PREV SEQUENCE NUMBER (COMP)         
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         CLC   TLDTKEY,KEYSAVE     GET LAST TRACKING RECORD                     
         BNE   NOTADJ                                                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DUE COMPANY BALANCE                           
         SPACE 1                                                                
DISPBAL  DS    0H                                                               
         EDIT  (4,DUCBAL),(12,SDTBAL+8),2,MINUS=YES,ALIGN=LEFT                  
         OI    SDTBALH+6,X'80'                                                  
         NI    SDTBALH+1,X'F3'     SET TO NORMAL INTENSITY                      
         TM    DUCBAL,X'80'        IF BALANCE IS NEGATIVE                       
         BZ    *+8                                                              
         OI    SDTBALH+1,X'08'     SET TO HIGH INTENSITY                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE DETERMINES NEXT SEQUENCE NUMBER                          
         SPACE 1                                                                
GETSEQ   NTR1                                                                   
         MVC   NEXTSEQ,HEXFFS      SET DEFAULT NEXT SEQUENCE NUMBER             
         XC    PREVDTE,PREVDTE     CLEAR PREV. FIELDS                           
         XC    PREVSEQ,PREVSEQ                                                  
         SPACE 1                                                                
         LA    R4,KEY              R4=A(KEY)                                    
         USING TLCKPD,R4                                                        
         XC    KEY,KEY             BUILD INITIAL KEY                            
         MVI   TLCKPCD,TLCKDCDQ    RECORD CODE                                  
         MVC   TLCKDSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCKDDUC,TGDUC      DUE COMPANY CODE                             
         GOTO1 HIGH                GET LAST LOGICAL TRACKING RECORD             
         SPACE 1                                                                
         CLC   TLCKPKEY(TLCKDDTE-TLCKPD),KEYSAVE IF WE DIDN'T FIND IT           
         BNE   GTRKX                             THEN DONE                      
         SPACE 1                                                                
         MVC   PREVDTE,TLCKDDTE    SAVE PREVIOUS DATE                           
         MVC   PREVSEQ,TLCKDSEQ              AND SEQUENCE NUMBER                
         SPACE 1                                                                
         MVC   TGTHREE,TGTODAY1    SET COMPLEMENTED DATE                        
         XC    TGTHREE,HEXFFS                                                   
         CLC   TLCKDDTE,TGTHREE    IF DATE IS NOT TODAY                         
         BNE   GTRKX               THEN DONE                                    
         SPACE 1                                                                
         XC    PREVSEQ,HEXFFS      ELSE BASE NEXT SEQ# ON PREV. SEQ#            
         SPACE 1                                                                
*                                  HANDLE HOBS AND LOBS SEPARATELY              
         LH    RF,PREVSEQ          LAST SEQUENCE NUMBER                         
         LA    RF,1(RF)            ADD 1                                        
         STH   RF,NEXTSEQ          SAVE NEXT NUMBER                             
         SPACE 1                                                                
         LH    RF,PREVSEQ+2        LAST SEQUENCE NUMBER+2                       
         LA    RF,1(RF)            ADD 1                                        
         STH   RF,NEXTSEQ+2        SAVE NEXT NUMBER+2                           
         SPACE 1                                                                
         XC    PREVSEQ,HEXFFS      COMPLEMENT SAVED SEQUENCE NUMBERS            
         XC    NEXTSEQ,HEXFFS                                                   
GTRKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 ASAVPTRS,DMCB,PTRBLK  SAVE PASSIVE POINTERS                      
         SPACE 1                                                                
         MVI   ELCODE,TADWELQ      REMOVE EXISTING ELEMENT (IF AROUND)          
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD DUE COMPANY WITHHOLDING EL.            
         LA    R3,ELEMENT                                                       
         USING TADWD,R3            R3=A(DUE COMPANY WITHHOLDING EL.)            
         MVI   TADWEL,TADWELQ                                                   
         MVI   TADWLEN,TADWLNQ                                                  
         MVC   TADWDUC,TGDUC       DUE COMPANY CODE                             
         SPACE 1                                                                
         LA    R2,SDTAMTH          COLLECTED AMOUNT                             
         GOTO1 ANY                                                              
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R4)                                          
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV                                                           
         MVC   TADWREC,4(R1)                                                    
         SPACE 1                                                                
         L     R1,DUCBAL           PREVIOUS BALANCE                             
         S     R1,TADWREC          + NEW APPLIED CREDITS                        
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    *+8                                                              
         A     R1,PREVREC          SUBTRACT PREV RECOVERED AMOUNT               
         ST    R1,DUCBAL           SAVE NEW BALANCE                             
         ST    R1,TADWBAL                                                       
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD IT                                       
         SPACE 1                                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SDTCMNTH),TACMTYPC COMMENT             
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY INFO                            
         SPACE 1                                                                
         BAS   RE,SETCHK           INSURE CHECK FILES SET                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         XC    PREVREC,PREVREC                                                  
         L     R3,AIO                                                           
         MVI   ELCODE,TADWELQ      GET DUE COMP. WITHHOLDING EL.                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADWD,R3            R3=A(DUE COMPANY WITHHOLDING EL.)            
         SPACE 1                                                                
         MVC   PREVREC,TADWREC     SAVE RECOVERED AMOUNT                        
         SPACE 1                                                                
         EDIT  (4,TADWREC),(12,SDTAMT),2,ALIGN=LEFT,FLOAT=-                     
         OI    SDTAMTH+6,X'80'                                                  
         SPACE 1                                                                
DISP6    GOTO1 CHAROUT,DMCB,TACMELQ,SDTCMNTH,TACMTYPC  COMMENT                  
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SDTLCHGH                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES DUE COMPANY RECORD BASED ON MANUAL ADJ.          
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILE                              
         SPACE 1                                                                
         L     R3,AIO2             READ DUE COMPANY RECORD FOR UPDATE           
         ST    R3,AIO                                                           
         GOTO1 RECVAL,DMCB,TLDUCDQ,(X'30',0)                                    
         SPACE 1                                                                
         MVI   ELCODE,TADUELQ      GET DUE COMP. DETAILS EL.                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R3            R3=A(DUE COMPANY DETAILS EL.)                
         SPACE 1                                                                
         L     R1,TADUDUE          AMOUNT DUE                                   
         S     R1,DUCBAL           LESS NEW BALANCE                             
         ST    R1,TADUCOL          IS NEW COLLECTED AMOUNT                      
         SPACE 1                                                                
         BAS   RE,DISPBAL          RE-DISPLAY IT                                
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE IT BACK FROM I/O 2                     
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              FILE SETTING ROUTINES                                            
         SPACE 2                                                                
SETTAL   DS    0H                  SET TALENT FILES                             
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         BR    RE                                                               
         SPACE 2                                                                
SETCHK   DS    0H                  SET CHECK FILES                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
ACTINV   LA    R2,CONACTH          INVALID ACTION                               
         MVI   ERROR,INVRCACT                                                   
         B     THEEND                                                           
         SPACE 1                                                                
NOTADJ   MVI   ERROR,ERNOTADJ      ONLY MANUAL ADJS. ALLOWED                    
         B     THEEND                                                           
         SPACE 1                                                                
ERRNOFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'DTRACK  ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'DUECOMP ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP ',CL8'LIST    '                               
PF15     DC    AL1(KEYTYGLB,L'TGSSN),AL2(TGSSN-TGD)                             
PF15X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB9D                                                       
         SPACE 3                                                                
DUCBAL   DS    F                   DUE COMPANY BALANCE                          
PREVREC  DS    F                   PREV RECOVERED AMT FOR THIS TRK REC          
PREVDTE  DS    XL3                 LAST DATE (COMPLEMENTED)                     
PREVSEQ  DS    F                   LAST SEQUENCE NUMBER (COMPLEMENTED)          
NEXTSEQ  DS    F                   NEXT SEQUENCE NUMBER (COMPLEMENTED)          
PTRBLK   DS    CL(2*L'TLDRREC+1)   POINTER BLOCK FOR TRACKING RECORD            
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* PERVALD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGENB9   04/27/04'                                      
         END                                                                    
