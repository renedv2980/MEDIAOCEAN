*          DATA SET TAGENC4    AT LEVEL 038 AS OF 04/28/15                      
*PHASE T702C4A,*                                                                
         TITLE 'T702C4 - ECAST LIST'                                            
T702C4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C4,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
         MVC   ECSTAG(7),=C'Sel^Pid'                                            
         OI    ECSTAGH+6,X'80'                                                  
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   ECS10                                                            
         BAS   RE,VKEY                                                          
         B     ECSX                                                             
*                                                                               
ECS10    CLI   MODE,LVALKEY        VALIDATE KEY OF LISTED RECORD                
         BNE   *+12                                                             
         BAS   RE,LVKEY                                                         
         B     ECSX                                                             
*                                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     ECSX                                                             
*                                                                               
         CLI   MODE,XRECADD        IF JUST ADDED RECORD                         
         BNE   ECS12                                                            
         XC    PTRBLK,PTRBLK       CLEAR SAVED POINTER BLOCK                    
         L     R3,ATHISLST                                                      
         USING LINED,R3                                                         
         OI    LSSNH+1,X'20'       PROTECT SSN                                  
         OI    LSSNH+6,X'80'       AND TRANSMIT                                 
         OI    LCATH+1,X'20'       PROTECT CATEGORY                             
         OI    LCATH+6,X'80'       AND TRANSMIT                                 
         B     ECS15               AND GO ADD PASSIVE PTRS                      
         DROP  R3                                                               
*                                                                               
ECS12    CLI   MODE,XRECPUT        IF JUST CHANGED RECORDS                      
         BNE   ECS20                                                            
ECS15    GOTO1 AADDPTRS,DMCB,PTRBLK ADD PASSIVE PTRS                            
         B     ECSX                                                             
*                                                                               
ECS20    CLI   MODE,LISTRECS                                                    
         BNE   ECS30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R0,LRHOOK           SET A(SYSIO HOOK)                            
         B     ECS40                                                            
         SPACE 1                                                                
ECS30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       REPORT COUNTER                               
         XC    KEY,KEY             INSURE START AT TOP OF LIST                  
         LA    R2,HOOK             SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS          SET A(SPECS)                                 
         ST    R2,SPECS                                                         
         LA    R0,PRHOOK           SET A(SYSIO HOOK)                            
         SPACE 1                                                                
ECS40    BAS   RE,LREC             GO LIST THE RECORDS                          
ECSX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
*                                  DISP/RTRN XTRA/NEED VLD/OK TO ADD            
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+CHNGLIST+OKTOADD                      
         MVC   LLIST,=Y(LLNQ)      SET L'DATA LINE                              
         MVI   NLISTS,7            AND N'RECORDS PER SCREEN                     
*                                                                               
         LA    R2,ECSAGYH                                                       
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   VK5                                                              
         TM    4(R2),X'20'         OR NOT PREVIOUSLY VALIDATED                  
         BO    VK10                                                             
VK5      NI    ECSCIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         TM    ECSCIDH+4,X'20'     VALIDATE DUE COMPANY CODE                    
         BO    VK20                                                             
         NI    ECSEPIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',ECSCIDH),0                           
         LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMM NUMBER                    
         MVC   TIFCOM,TGCOM        SET SYSIO FILTER                             
         DROP  R3                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         CLI   TACOTYPE,CTYSOAP    INSURE SOAP COMMERCIAL TYPE                  
         BNE   BADCTYPE                                                         
         DROP  R4                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,ECSCMTH,TACMTYPG   DISP COMML COMM          
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,ECSEPIH          VALIDATE EPISODE NUMBER                      
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    ECSSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLEPCDQ,ECSEPIH                                      
         MVC   TIFEPI,TGEPI        SET SYSIO FILTER                             
*                                                                               
VK30     LA    R2,ECSSTRH          START AT FIELD                               
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVC   TIQSTART(9),8(R2)                                                
*                                                                               
         OI    ECSSTRH+4,X'20'     SET VALIDATED                                
         XC    KEY,KEY             RE-INITIALIZE LIST                           
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         ST    R0,TIHOOK           HOOK TO SYSIO                                
         BAS   RE,SETSCRN          SET CLR/NORM/VER/UNPROTECT                   
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVI   TIREAD,TLECCDQ      READ ECAST RECORDS                           
         MVC   TIKHOOK,SETLSTK     A(KEY SETTING ROUTINE)                       
*                                                                               
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   LR20                                                             
         CLI   PFAID,14            AND USER WANTS TO ADD                        
         BNE   LR20                                                             
         BAS   RE,SETADD           SET SCREEN FOR ADD(SHOW LAST ECAST)          
         B     LR40                                                             
*                                                                               
LR20     GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
LR40     XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   LR50                                                             
         CLI   PFAID,14            AND USER PRESSED 'ADD' PFKEY                 
         BE    PLSENTER            DISPLAY PLEASE ENTER REQ'D FIELDS            
         B     LRX                 ELSE RETURN                                  
*                                                                               
LR50     CP    COUNTER,=P'0'       MUST BE PRINTREP - IF SPOOLING NOW           
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(8,P+1),ALIGN=LEFT                                       
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(20,R1),=C'EPISODE CAST RECORDS'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    LRX                                                              
         TWAXC ECSSSNH,PROT=Y      CLEAR THE SCREEN                             
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
LRX      B     XIT                                                              
         EJECT                                                                  
*               UNPROTECT SSN&CAT AND CLEAR,NORM,VER FOR WHOLE SCRN             
         SPACE                                                                  
SETSCRN  NTR1                                                                   
         ZIC   R0,NLISTS                                                        
         LA    R3,ECSSSNH                                                       
         USING LINED,R3                                                         
SETSC5   NI    LSSNH+1,X'DF'       UNPROTECT SSN                                
         NI    LCATH+1,X'DF'       UNPROTECT CAT                                
         LA    R3,LINNEXT(R3)                                                   
         BCT   R0,SETSC5                                                        
         DROP  R3                                                               
*                                  CLR/NORM/VERF WHOLE SCREEN                   
         GOTO1 FLDVAL,DMCB,(X'23',ECSL1H),(X'10',ECSLSTH)                       
         B     XIT                                                              
         SPACE 2                                                                
*               ROUTINE TO DISPLAY LAST ECAST RECORD                            
         SPACE 1                                                                
SETADD   NTR1                                                                   
         MVI   SVFREC,C'N'         NO READ TO SHOW                              
         OI    TIQFLAGS,TIQFDIR    SET TO READ DIRECTORY                        
         XC    DMDSKADD,DMDSKADD                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         NI    TIQFLAGS,ALL-TIQFDIR TURN OFF INDICATOR                          
         OC    DMDSKADD,DMDSKADD   IF THERE IS A D/A                            
         BZ    SETADDX                                                          
         XC    KEY,KEY             GET IT                                       
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,DMDSKADD                                                  
         MVC   AIO,TIAREC          SET IO FOR DISPLAY ROUTINE                   
         GOTO1 GETREC                                                           
         BAS   RE,DISPLAY          DISPLAY IT                                   
         MVC   AIO,AIO1            RESET AIO                                    
         MVI   SVFREC,C'Y'         FOUND RECORD                                 
SETADDX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*               PROCESS SYSIO RECORDS WHEN LISTING RECORDS                      
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESSING DIRECTORY HOOKS                   
         BNE   LRH30                                                            
         MVC   DMDSKADD,TIDSKADD   SAVE D/A OF ECAST RECORD                     
         B     NO                  SET HAVEN'T FOUND RECORD                     
*                                                                               
LRH30    CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH60               GO BACK TO LISTMON                           
*                                                                               
         BAS   RE,DISPLAY          OTHERWISE GO DISPLAY IT                      
*                                                                               
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
LRH60    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRHX     B     XIT                                                              
         SPACE 2                                                                
*               PROCESS SYSIO RECORDS WHEN PRINTING                             
         SPACE 1                                                                
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
*                                                                               
         LA    R3,ECSSSNH          SET A(CURRENT LINE TO 1ST LINE)              
         ST    R3,ATHISLST                                                      
         USING LINED,R3                                                         
*                                                                               
         BAS   RE,DISPLAY          DISP REC TO SCREEN                           
         MVI   BYTE,C'P'           PRINT LINES                                  
         GOTO1 PRTSCRN,DMCB,(R3),LNAMES,P                                       
         AP    COUNTER,=P'1'       ADD TO COUNTER                               
*                                                                               
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
PRHX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*               DISPLAY FIRST LINE FROM RECORD IN TIAREC                        
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS LINE)                              
         USING LINED,R3                                                         
         L     R6,TIAREC                                                        
         USING TLECD,R6                                                         
         CLI   0(R6),TLECCDQ       ECAST RECORD?                                
         BNE   DSPX                                                             
         MVC   LSSN,TLECSSN        SHOW S/S NUMBER                              
         OI    LSSNH+1,X'20'       PROTECT IT                                   
         GOTO1 SSNPACK,DMCB,TLECSSN,TGPID                                       
         MVC   LSSN,SPACES                                                      
         MVC   LSSN(L'TGPID),TGPID                                              
         MVI   LSSNH+5,6                                                        
         OI    LSSNH+6,X'80'                                                    
         OI    LSSNH+1,X'20'       PROTECT IT                                   
*                                                                               
DISP02   MVC   LCAT,TLECCAT        SHOW CATEGORY                                
         MVI   LCATH+5,L'TLECCAT   AND ITS LENGTH                               
         OI    LCATH+1,X'20'       PROTECT IT                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TACAELQ      CAST DETAILS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP15                                                           
         USING TACAD,R4                                                         
         MVC   LONOF,TACAONOF      ON/OFF CAMARA                                
         MVC   LUNIT,TACAUNIT      TAX UNIT CODE                                
         XC    LACDE,LACDE                                                      
         OC    TACANCDE,TACANCDE   AGENT CODE                                   
         BZ    DISP10                                                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),LACDE                              
*                                                                               
DISP10   MVC   LUN,TACAUN          UNION                                        
         MVC   LLOCL,TACALOCL      LOCAL                                        
         MVC   LYEAR,TACAYEAR      YEAR                                         
         MVC   LCORP,TACACORP      CORPORATION NUMBER                           
         OC    TACAWPCT,TACAWPCT   WRITER ROLE PERCENTAGE                       
         BZ    DISP15                                                           
         EDIT  TACAWPCT,(6,LPCT),3,ALIGN=RIGHT                                  
         DROP  R4                                                               
*                                                                               
DISP15   BAS   RE,DRPERF           DISPLAY PERFORMANCE RATE                     
         BAS   RE,DRROLE           DISPLAY WRITER ROLE                          
         MVC   AIO,TIAREC                                                       
         BAS   RE,DISP2L           DISPLAY SECOND LINE                          
DSPX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY PERFORMANCE RATE                              
DRPERF   NTR1                                                                   
         LR    R4,R6               DISPLAY PERFORMANCE RATE                     
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRPERFX                                                          
         USING TACRD,R4                                                         
         OC    TACRSCAL,TACRSCAL                                                
         BZ    DRPERFX                                                          
         EDIT  TACRSCAL,(10,LPERF),2                                            
DRPERFX  B     XIT                                                              
         DROP  R4                                                               
         SPACE                                                                  
*              ROUTINE TO DISPLAY WRITER ROLE                                   
DRROLE   NTR1                                                                   
         LR    R4,R6               DISPLAY WRITER ROLE                          
         MVI   ELCODE,TASOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRROLEX                                                          
         USING TASOD,R4                                                         
         CLI   TASOSTAT,0          NO WRITER ROLE EQUATE                        
         BE    DRROLEX                                                          
         LA    RE,WROLTAB          LOOK UP CHARACTER CODE FOR DISPLAY           
         USING WROLTABD,RE                                                      
DRROLE10 CLC   WROLEQU,TASOSTAT                                                 
         BE    DRROLE20                                                         
         LA    RE,WROLTABL(RE)                                                  
         CLI   0(RE),X'FF'         IF END OF TABLE                              
         BNE   DRROLE10                                                         
         DC    H'0'                DIE - MUST FIND WRITER ROLE                  
*                                                                               
DRROLE20 MVC   LROLE,WROLCHAR      DISPLAY WRITER ROLE TO SCREEN                
DRROLEX  B     XIT                                                              
         DROP  R6,R4,RE                                                         
         EJECT                                                                  
*               DISPLAY SECOND LINE FROM RECORD IN TIAREC                       
         SPACE 1                                                                
DISP2L   NTR1                                                                   
         MVC   AIO,AIO3                                                         
         MVC   LSSNN,LTMISS        SSN NAME NOT FOUND                           
         CLI   LSSNH+5,6                                                        
         BH    DISP2L2                                                          
         MVC   TGPID,LSSN                                                       
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   DISP2L2                                                          
         MVC   LSSN,TGSSN                                                       
DISP2L2  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',LSSN),0                               
         BNE   DISP2L40                                                         
         L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF RECORD IS LOCKED                          
         BNO   *+14                                                             
         MVC   LSSNN,=CL16'** W4 LOCKED **'                                     
         B     *+10                                                             
         MVC   LSSNN,TGNAME                                                     
*                                  LOOK FOR CORPORATION NAME                    
         MVC   LCORPN,SPACES                                                    
         CLI   LCORP,C'1'          IF THERE IS A CODE                           
         BL    DISP2L10                                                         
         MVC   LCORPN,LTMISS                                                    
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO                                                    
         MVC   HALF+1(1),LCORP                                                  
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   DISP2L40                                                         
         L     R2,TGELEM           R2=A(TAX ID ELEMENT FOR CORP)                
         USING TATID,R2                                                         
         MVC   AIO,AIO2                                                         
         MVC   SVSSN,TGSSN                                                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',TATIID),0                             
         BNE   *+10                                                             
         MVC   LCORPN,TGNAME                                                    
                                                                                
         L     R4,AIO2                                                          
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS CRP IS LOCKED                        
         BNO   DISP2L05                                                         
         MVC   0(16,R2),=C'** CRP LOCKED **'                                    
                                                                                
DISP2L05 MVC   TGSSN,SVSSN                                                      
         B     DISP2L40                                                         
DISP2L10 L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ      CHECK ANY CORP ID EXISTS                     
         BAS   RE,GETEL                                                         
         BNE   DISP2L40                                                         
         MVC   LCORPN,LTCRP        DISPLAY 'PERF HAS CORP'                      
*                                  LOOK FOR AGENT NAME                          
DISP2L40 BRAS  RE,FIXSSN                                                        
         MVC   LACDEN,SPACES                                                    
         OC    LACDE,LACDE         IF NO AGENT CODE                             
         BNZ   DISP2L50                                                         
         MVI   ELCODE,TAPEELQ      LOOK FOR PAYEE ELEMENT ON W4                 
         L     R4,AIO3                                                          
         BAS   RE,GETEL                                                         
         BNE   DISP2LX                                                          
         MVC   LACDEN,=CL16'* PAYEE ON W4 *'                                    
         B     DISP2LX                                                          
DISP2L50 MVC   LACDEN,LTMISS                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'8C',LACDE),0                              
         BNE   *+10                                                             
         MVC   LACDEN,TGNAME                                                    
*                                                                               
DISP2LX  MVC   AIO,AIO1            RESTORE AIO                                  
         OI    LNAMEH+1,X'08'      MAKE SURE THIS LINE IS HIGHLIGHTED           
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE VALIDATES KEY OF LISTED RECORDS                          
         SPACE 2                                                                
LVKEY    NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
         USING LINED,R3                                                         
*                                                                               
         LA    R2,LSSNH            SS NUMBER REQUIRED                           
         GOTO1 ANY                                                              
         CLI   LSSNH+5,6                                                        
         BH    LVK10                                                            
         MVC   TGPID,LSSN                                                       
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   LVK10                                                            
         MVC   LSSN,TGSSN                                                       
         MVI   LSSNH+5,9                                                        
LVK10    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),0                               
         BRAS  RE,FIXSSN                                                        
*                                                                               
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         BO    ERW4LCK             EXIT WITH ERROR MESSAGE                      
         MVC   LSSNN,TGNAME                                                     
*                                                                               
         LA    R2,LCATH            CATEGORY REQUIRED                            
         BAS   RE,CPYDWN           COPY FROM ABOVE IF NECESSARY                 
         GOTO1 ANY                                                              
         OC    WORK(L'TGCAT),SPACES                                             
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY CODE                       
         BNE   FLDINV                                                           
         OI    4(R2),X'20'         SET PREV. VALIDATED                          
*                                                                               
         MVC   TGCSORT(1),TGCASORT SET CATEGORY SORT CODE FOR KEY               
         XC    TGINV,TGINV         CLEAR GLOBAL INVOICE NUMBER                  
*                                  CHECK NO DUPLICATE UP TO INV NUMBER          
         GOTO1 RECVAL,DMCB,TLECCDQ,0                                            
         CLC   KEY(TLECINV-TLECKEY),KEYSAVE                                     
         BE    *+10                DUPLICATE KEY                                
         MVC   KEY,KEYSAVE         KEY OKAY TO ADD                              
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE VALIDATES LISTED RECORDS                                 
         SPACE 1                                                                
LVREC    NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         GOTO1 ASAVPTRS,DMCB,PTRBLK                                             
*                                                                               
         L     R3,ATHISLST         R3=A(THIS DATA LINE)                         
         USING LINED,R3                                                         
         MVI   ELCODE,TACAELQ      REMOVE EXISTING CAST DETAILS EL.             
         GOTO1 REMELEM                                                          
*                                                                               
         XC    CAELEM,CAELEM       BUILD NEW CAST DETAILS ELEMENT               
         LA    R4,CAELEM                                                        
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ      ELEMENT CODE                                 
         MVI   TACALEN,TACALNQ     ELEMENT LENGTH                               
*                                                                               
         BAS   RE,VRONOF           VALIDATE CAMERA (ON/OFF)                     
         BAS   RE,VRTAX            VALIDATE TAX UNIT FIELD                      
         BAS   RE,VRAGCD           VALIDATE AGENT CODE                          
         BAS   RE,VRUNLO           VALIDATE UNION/LOCAL FIELD                   
         BAS   RE,VRYEAR           VALIDATE YEAR                                
         BAS   RE,VRCORP           VALIDATE CORPORATION NUMBER                  
*                                                                               
         BAS   RE,VRPERF           TAKE CARE OF TACRD ELEMENT                   
         BAS   RE,VRROLE           TAKE CARE OF TASOD ELEMENT                   
*                                                                               
         BAS   RE,VRPCT            VALIDATE WRITER PERCENTAGE                   
         MVC   ELEMENT,CAELEM                                                   
         GOTO1 ADDELEM             ADD CAST DETAILS ELEMENT                     
*                                                                               
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'20',LSSNH),LCORPH  SET ALL FLDS VER.              
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         TM    LSSNH+1,X'20'       IF CHANGE - REREAD RECORD                    
         BZ    LVRX                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
LVRX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VAL ON/OFF CAMERA FLD                                 
         SPACE 1                                                                
VRONOF   NTR1                                                                   
         L     R6,AIO                                                           
         USING TLECD,R6                                                         
         GOTO1 CATVAL,DMCB,TLECCAT SET TGCASTAT                                 
         LA    R2,LONOFH           R2=A(ON/OFF FIELD)                           
         BAS   RE,CPYDWN           COPY FROM ABOVE IF NECESSARY                 
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRONOF50                                                         
         TM    TGCASTAT,OKON       THEN IF OK ON CAMERA ONLY                    
         BZ    VRONOF10                                                         
         TM    TGCASTAT,OKOFF                                                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'ON '     THEN MOVE 'ON' TO FIELD                      
         B     VRONOF40                                                         
*                                                                               
VRONOF10 TM    TGCASTAT,OKON       ELSE IF OK OFF CAMERA ONLY                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
         B     VRONOF40                                                         
*                                                                               
*RONOF20 TM    TGCAUNI,AFM         ELSE IF CATEGORY IS MUSICIAN                 
VRONOF20 GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    VRONOF50                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
VRONOF40 MVI   5(R2),L'TACAONOF    AND SET ITS LENGTH                           
*                                                                               
VRONOF50 GOTO1 ANY                 MOVE AND SPACE PAD FIELD TO WORK             
         CLC   =C'ON ',WORK        IF FIELD CONTAINS 'ON'                       
         BNE   VRONOF60                                                         
         TM    TGCASTAT,OKON       THEN MUST BE OK FOR CATEGORY                 
         BZ    FLDINV                                                           
         B     VRONOF90                                                         
*                                                                               
VRONOF60 CLC   =C'OFF',WORK        ELSE FIELD MUST CONTAIN 'OFF'                
         BNE   FLDINV                                                           
         TM    TGCASTAT,OKOFF      AND MUST BE VALID FOR CATEGORY               
         BZ    FLDINV                                                           
*                                                                               
VRONOF90 MVC   TACAONOF,WORK       SAVE IN ELEMENT                              
VRONOFX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO TAX UNIT FIELD                                        
         SPACE 1                                                                
VRTAX    NTR1                                                                   
         LA    R2,LUNITH           R2=A(TAX UNIT)                               
         BAS   RE,CPYDWN           COPY FROM ABOVE IF NECESSARY                 
         GOTO1 ANY                                                              
         CLI   5(R2),2             MUST BE AT LEAST 2 CHARS                     
         BL    FLDINV                                                           
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   FLDINV                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    FLDINV              GOING FORWARD                                
         CLC   =C'FD',8(R2)        IF UNIT STARTS W/ FD                         
         BE    FLDINV                                                           
         CLI   WORK+2,C'0'         OR ENDS WITH 0 THEN ERROR                    
         BE    FLDINV                                                           
*                                                                               
         MVC   TACAUNIT,WORK       SAVE IN ELEMENT                              
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TAX UNIT FIELD (OPTIONAL)                             
         SPACE 1                                                                
VRAGCD   NTR1                                                                   
         XC    TACANCDE,TACANCDE                                                
         CLI   LACDEH+5,0          AGENT CODE (OPTIONAL)                        
         BE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLANCDQ,LACDEH                                       
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE UNION/LOCAL FIELD                                                    
*                                                                               
VRUNLO   NTR1                                                                   
         LA    R2,LUNH                                                          
         BAS   RE,CPYDWN           COPY FROM ABOVE IF NECESSARY                 
         GOTO1 ANY                                                              
         CLI   5(R2),3             FIELD MUST BE LONGER THEN 3 BYTES            
         BNH   FLDINV                                                           
         OC    WORK(7),SPACES                                                   
         GOTO1 UNIVAL,DMCB,WORK    VALIDATE UNION                               
         BNE   FLDINV                                                           
*                                                                               
*        ZIC   RF,TGUNEQU          TEST UNION VALID FOR THIS CATEGORY           
*        EX    RF,*+8                                                           
*        B     *+8                                                              
*        TM    TGCAUNI,0                                                        
         GOTO1 UNITEST,DMCB,(X'80',TGUNEQUS),TGCAUNIS                           
         BZ    FLDINV              TEST UNION VALID FOR THIS CATEGORY           
         MVC   TACAUN,WORK         SAVE UNION IN ELEMENT                        
*                                                                               
         LA    R3,WORK+4           R0 = A(LOCAL CODE WITHIN FIELD)              
         CLI   WORK+3,C' '                                                      
         BE    VRUNLO20                                                         
         CLI   WORK+3,C'/'                                                      
         BE    VRUNLO20                                                         
         LA    R3,WORK+3                                                        
*                                                                               
VRUNLO20 MVC   TGLCL,0(R3)         VALIDATE LOCAL CODE                          
         MVC   TGUNI,TACAUN                                                     
         GOTO1 RECVAL,DMCB,TLLOCDQ,0                                            
         BNE   THEEND                                                           
         MVC   TACALOCL,TGLCL      SAVE LOCAL IN ELEMENT                        
VRUNLOX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE CONTRACT YEAR FIELD                                                  
*                                                                               
VRYEAR   NTR1                                                                   
         LA    R2,LYEARH                                                        
         BAS   RE,CPYDWN           COPY FROM ABOVE IF NECESSARY                 
         GOTO1 ANY                 IF FIELD IS EMPTY                            
         GOTO1 YRVAL,DMCB,WORK     VALIDATE YEAR                                
         BNE   FLDINV                                                           
*                                                                               
         LA    R3,TGUNYR           R3 = A(VALID UNION YEARS)                    
VRYEAR10 CLI   0(R3),0             IF END OF TABLE THEN ERROR                   
         BE    FLDINV                                                           
         CLC   0(1,R3),TGYREQU     IF MATCH THEN FOUND                          
         BE    VRYEAR20                                                         
         LA    R3,1(R3)            ELSE TRY NEXT TABLE ENTRY                    
         B     VRYEAR10                                                         
*                                                                               
VRYEAR20 CLC   TACAUN,=C'SAG'      IF UNION IS SAG                              
         BE    VRYEAR25                                                         
         CLC   TACAUN,=C'AFT'      OR UNION IS AFTRA                            
         BNE   VRYEAR30                                                         
VRYEAR25 CLC   =C'01',WORK         YEAR CANNOT BE 2001 FOR SOAPS                
         BE    FLDINV                                                           
*                                                                               
VRYEAR30 MVC   TACAYEAR,WORK       SAVE YEAR IN ELEMENT                         
VRYEARX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CORP FIELD (OPTIONAL)                        
         SPACE 1                                                                
VRCORP   NTR1                                                                   
         MVI   TACACORP,0                                                       
         LA    R2,LCORPH           R2=A(CORPORATION FIELD)                      
         CLI   5(R2),0                                                          
         BE    VCRPX                                                            
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         BNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
*                                                                               
         MVC   AIO,AIO2            SEARCH W4 RECORD FOR TAX ID EL.              
         CLI   LSSNH+5,6                                                        
         BH    VRCRP10                                                          
         MVC   TGPID,LSSN                                                       
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VRCRP10                                                          
         MVC   LSSN,TGSSN                                                       
         MVI   LSSNH+5,9                                                        
VRCRP10  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',LSSN)   GET W4 REC                    
         BRAS  RE,FIXSSN                                                        
*                                                                               
         MVI   ELCODE,TATIELQ                                                   
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   FLDINV              ERROR IF NOT FOUND                           
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         BNE   VCRPX                                                            
         L     R4,TGELEM                                                        
         BAS   RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         BE    ERRCRP              NEED PRECISE CORP CODE                       
*                                                                               
VCRPX    CLI   5(R2),0                                                          
         BE    XIT                                                              
         L     RF,TGELEM                                                        
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         BE    XIT                                                              
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERCRPLK             EXIT WITH ERROR MESSAGE                      
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE PERFORMANCE RATE (OPTIONAL)                             
VRPERF   NTR1                                                                   
         XC    FULL,FULL                                                        
         LA    R2,LPERFH                                                        
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          ANY PERFORMANCE RATE?                        
         BZ    VRPERF10                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE NOT ALLOWED                         
         BO    FLDINV                                                           
         MVC   FULL,4(R1)                                                       
*                                                                               
VRPERF10 L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         USING TACRD,R4                                                         
         BAS   RE,GETEL            IF ELEMENT EXISTS- CHANGE IT                 
         BE    VRPERF20                                                         
         OC    FULL,FULL           OTHERWISE, IF PERF RATE                      
         BZ    VRPERFX                                                          
         XC    ELEMENT,ELEMENT     ADD ELEMENT                                  
         LA    R4,ELEMENT                                                       
         MVI   TACREL,TACRELQ                                                   
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSCAL,FULL                                                    
         GOTO1 ADDELEM                                                          
         B     VRPERFX                                                          
*                                                                               
VRPERF20 MVC   TACRSCAL,FULL       SET NEW PERF RATE                            
VRPERFX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE WRITER ROLE                                             
VRROLE   NTR1                                                                   
         L     R4,AIO              DELETE OLD TASOD ELEMENT                     
         MVI   ELCODE,TASOELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   SVSOSTAT,0          SAVED WRITER ROLE EQUATE                     
         LA    R2,LROLEH                                                        
         CLI   5(R2),0             IF INPUT                                     
         BE    VRROLE10                                                         
         TM    TGCATYPE,WRITER     MUST BE WRITER ROLE                          
         BNO   FLDINV                                                           
         CLI   TGCAEQU,CTW         IF CATEGORY IS NOT W, W2, OR W3              
         BE    VRROLE15                                                         
         CLI   TGCAEQU,CTW2                                                     
         BE    VRROLE15                                                         
         CLI   TGCAEQU,CTW3                                                     
         BE    VRROLE15                                                         
*                                                                               
VRROLE10 TM    TGCATYPE,WRITER     MAKE SURE CATEGORY IS WRITER                 
         BNO   VRROLEX                                                          
         BAS   RE,SETROLE          THEN TRY TO DEDUCE WRITER ROLE               
*                                                                               
VRROLE15 MVC   HALF,8(R2)                                                       
         OC    HALF,SPACES                                                      
         XC    ELEMENT,ELEMENT     ADD NEW TASOD ELEMENT                        
         LA    R4,ELEMENT                                                       
         USING TASOD,R4                                                         
         MVI   TASOEL,TASOELQ      ELEMENT CODE                                 
         MVI   TASOLEN,TASOLNQ+L'TASOSEPI ELEMENT LENGTH                        
         MVI   TASONUM,1           NUMBER OF EPISODES                           
         PACK  DUB,TGEPI           SET BINARY EPISODE NUMBER                    
         CVB   RE,DUB                                                           
         STH   RE,TASOEPI                                                       
         LA    RE,WROLTAB          TABLE OF VALID WRITER ROLES                  
         USING WROLTABD,RE                                                      
VRROLE20 CLC   WROLCHAR,HALF       CHECK INPUT ROLE                             
         BE    VRROLE30                                                         
         LA    RE,WROLTABL(RE)                                                  
         CLI   0(RE),X'FF'         IF END OF TABLE                              
         BNE   VRROLE20                                                         
         B     FLDINV              INVALID WRITER ROLE INPUT                    
*                                                                               
VRROLE30 MVC   TASOSTAT,WROLEQU    SET WRITER ROLE EQUATE IN ELEMENT            
         MVC   SVSOSTAT,TASOSTAT   SAVE WRITER EQUATE                           
         GOTO1 ADDELEM                                                          
VRROLEX  B     XIT                                                              
         DROP  R4,RE                                                            
         EJECT                                                                  
SETROLE  NTR1                                                                   
         LA    RE,WEQUTAB          TRY TO DEDUCE WRITER ROLE                    
         USING WEQUTABD,RE                                                      
SETROL5  CLC   WEQUCAT,TGCAEQU                                                  
         BE    SETROL8                                                          
         LA    RE,WEQUTABL(RE)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(RE),X'FF'                                                      
         BNE   SETROL5             LOOP                                         
         B     FLDMISS             MISSING INPUT FIELD                          
*                                                                               
SETROL8  XC    8(2,R2),8(R2)                CLEAR ROLE FIELD                    
         MVC   8(L'WEQUROLE,R2),WEQUROLE    SET ROLE TO SCREEN                  
         OI    6(R2),X'80'                  TRANSMIT                            
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              THIS ROUTINE VALIDATES PERCENTAGE                                
VRPCT    NTR1                                                                   
         LA    R2,LPCTH            IF WRITER PERCENT INPUT                      
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VRPCTX                                                           
         CLI   SVSOSTAT,0          MUST HAVE WRITER ROLE INPUT                  
         BE    FLDINV                                                           
         CLI   SVSOSTAT,TASOSH     AND ROLE MUST BE H,S,OR B                    
         BE    VRPCT10                                                          
         CLI   SVSOSTAT,TASOSS                                                  
         BE    VRPCT10                                                          
         CLI   SVSOSTAT,TASOSB                                                  
         BNE   FLDINV              NO COMBINATIONS ALLOWED                      
*                                                                               
VRPCT10  GOTO1 CASHVAL,DMCB,(3,8(R2)),(RF)                                      
         CLI   0(R1),0                                                          
         BNE   FLDINV                                                           
         LA    R4,CAELEM           UPDATE                                       
         USING TACAD,R4                                                         
         L     RE,4(R1)                                                         
         C     RE,=F'99999'                                                     
         BH    FLDINV              CAN'T BE MORE THAN 99.999                    
         STCM  RE,15,TACAWPCT      SET WRITER PERCENT                           
VRPCTX   B     XIT                                                              
         DROP  R4,R3                                                            
         EJECT                                                                  
*              COPIES DOWN VALUE FROM FIELD ABOVE (IF NECESSARY)                
         SPACE 2                                                                
CPYDWN   NTR1                                                                   
         CLI   5(R2),0             IF CURRENT FIELD EMPTY                       
         BNE   CPYDWNX                                                          
         LR    R1,R2                                                            
         SH    R1,=Y(LINNEXT)      R1 PTS TO PREV LINE                          
         LA    RF,ECSSSNH                                                       
         CR    R1,RF               FIRST LINE?                                  
         BNH   CPYDWNX             YES - NOTHING TO COPY DOWN?                  
         SR    RE,RE                                                            
         ICM   RE,1,5(R1)                                                       
         BZ    CPYDWNX             SHOULDN'T HAPPEN                             
         STC   RE,5(R2)            SET LENGTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R1)       SET DATA                                     
         OI    6(R2),X'80'         TRANSMIT                                     
CPYDWNX  B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'          MOVE KEY FIELDS TO HEADS                      
         GOTO1 PRTSCRN,DMCB,EFHTAG,ECSKEYXH,H5                                  
         MVI   BYTE,C'P'          RESET                                         
         B     XIT                                                              
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE                                                                  
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         B     THEEND                                                           
         SPACE                                                                  
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP- NEED EXACT CODE                   
         B     THEEND                                                           
         SPACE                                                                  
ERRINP   MVI   ERROR,ERNOINP       NO INPUT ALLOWED                             
         B     THEEND                                                           
         SPACE                                                                  
BADCTYPE MVI   ERROR,ERRECCTY      WRONG COMML TYPE FOR THIS SCREEN             
         B     THEEND                                                           
         SPACE                                                                  
DELMSG   MVI   MYMSGNO1,85         PRESS PF15 TO DELETE HIGHLIGHED REC          
         B     INFEND                                                           
         SPACE                                                                  
PFERROR  MVI   ERROR,ERINVPFF      PFKEY INVALID FOR THIS FIELD                 
         LA    R2,ECSSSNH                                                       
         B     THEEND                                                           
         SPACE                                                                  
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R1,ECSSSNH          CURSOR TO FIRST SSN FIELD                    
         USING LINED,R1                                                         
         LA    R2,LSSNH            R2=A(FIRST SSN FIELD)                        
         CLI   SVFREC,C'N'         IF DISPLAYED A RECORD                        
         BE    *+8                                                              
         LA    R2,LINNEXT(R2)      R2=A(SECOND SSN FIELD)                       
         B     INFEND                                                           
         DROP  R1                                                               
         SPACE 2                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK)    CORP RECORD LOCKED - NON-PAY             
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE                                                                  
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYCUR,L'LSSN-1),AL2(LSSN-LSSN)                            
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CEPISODE',CL8'LIST    '                               
PF15     DC    AL1(KEYTYCUR,L'LSSN-1),AL2(LSSN-LSSN)                            
PF15X    EQU   *                                                                
         SPACE 1                                                                
         DC    X'FF'                                                            
         SPACE                                                                  
LTMISS   DC    C' * NOT FOUND *  '                                              
LTCRP    DC    C'*PERF HAS CORP* '                                              
LTCRPL   DC    C'*CRP IS LOCKED* '                                              
COMPLM   DC    6X'FF'                                                           
         SPACE                                                                  
*          TABLE OF WRITER ROLE EQUATES                                         
*                                                                               
WEQUTAB  DS    0CL2                                                             
         DC    AL1(CTWB),C'B'                                                   
         DC    AL1(CTWS),C'S'                                                   
         DC    AL1(CTWH),C'H'                                                   
         DC    AL1(CTW2B),C'B'                                                  
         DC    AL1(CTW2S),C'S'                                                  
         DC    AL1(CTW2H),C'H'                                                  
         DC    AL1(CTW3B),C'B'                                                  
         DC    AL1(CTW3S),C'S'                                                  
         DC    AL1(CTW3H),C'H'                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
*          TABLE OF VALID WRITER ROLES                                          
*                                                                               
WROLTAB  DS    0CL3                                                             
         DC    AL1(TASOSH+TASOSS+TASOSB),CL2'A '                                
         DC    AL1(TASOSH+TASOSS),CL2'HS'                                       
         DC    AL1(TASOSH+TASOSB),CL2'HB'                                       
         DC    AL1(TASOSS+TASOSB),CL2'SB'                                       
         DC    AL1(TASOSH),CL2'H '                                              
         DC    AL1(TASOSS),CL2'S '                                              
         DC    AL1(TASOSB),CL2'B '                                              
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,32,C'EPISODE CAST REPORT'                                     
         SSPEC H2,32,C'-------------------'                                     
         SPACE 1                                                                
         SSPEC H8,2,C'SEL SS NUMBER  CAT  CAM  TAX  AGNT  UNI/LCL'              
         SSPEC H9,2,C'--- ---------  ---  ---  ---  ----  -------'              
         SSPEC H8,47,C'YR   CRP  INV #   LAST CHANGED'                          
         SSPEC H9,47,C'---  ---  ------  ------------'                          
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* CONVERT SSN TO PID                                                            
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
         USING LINED,R3                                                         
FIXSSN   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   LSSN,SPACES                                                      
         MVC   LSSN(L'TGPID),TGPID                                              
         MVI   LSSNH+5,6                                                        
         OI    LSSNH+6,X'80'                                                    
FIXSSNX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAW4LCRP                                                       
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 2                                                                
LINED    DSECT                                                                  
LSSNH    DS    CL8                                                              
LSSN     DS    CL9                 S/S NUMBER                                   
LCATH    DS    CL8                                                              
LCAT     DS    CL3                 CATEGORY                                     
LONOFH   DS    CL8                                                              
LONOF    DS    CL3                 ON/OFF CAMARA                                
LUNITH   DS    CL8                                                              
LUNIT    DS    CL3                 TAX UNIT CODE                                
LACDEH   DS    CL8                                                              
LACDE    DS    CL4                 AGENT CODE                                   
LUNH     DS    CL8                 UNION AND LOCAL FIELD                        
LUN      DS    CL3                 UNION                                        
         DS    CL1                                                              
LLOCL    DS    CL3                 LOCAL                                        
LYEARH   DS    CL8                                                              
LYEAR    DS    CL3                 YEAR                                         
LCORPH   DS    CL8                                                              
LCORP    DS    CL1                 CORPORATION NUMBER                           
LPERFH   DS    CL8                                                              
LPERF    DS    CL10                PERFORMANCE RATE                             
LROLEH   DS    CL8                                                              
LROLE    DS    CL2                 WRITER ROLE                                  
LPCTH    DS    CL8                                                              
LPCT     DS    CL6                 WRITER PERCENTAGE                            
*                                                                               
LNAMEH   DS    CL8                                                              
LNAMES   DS    CL62                                                             
         ORG   LNAMES                                                           
LSSNN    DS    CL16                W4 NAME                                      
         DS    CL10                                                             
LACDEN   DS    CL16                AGENT NAME                                   
         DS    CL4                                                              
LCORPN   DS    CL16                CORP NAME                                    
         ORG                                                                    
LLNQ     EQU   *-LINED                                                          
*                                                                               
LSELH    DS    CL8                 A(SELECT FIELD ON NEXT LINE)                 
LSEL     DS    CL3                                                              
LINNEXT  EQU   *-LINED                                                          
         EJECT                                                                  
*                WRITER ROLE TABLE DSECT                                        
WROLTABD DSECT                                                                  
WROLEQU  DS    XL1                 EQUATE FOR THIS WRITER ROLE                  
WROLCHAR DS    CL2                 CHAR WRITER ROLE                             
WROLTABL EQU   (*-WROLTABD)                                                     
         SPACE                                                                  
*                WRITER EQUATE TABLE DSECT                                      
WEQUTABD DSECT                                                                  
WEQUCAT  DS    XL1                 CATEGORY EQUATE                              
WEQUROLE DS    CL1                 CHAR WRITER ROLE                             
WEQUTABL EQU   (*-WEQUTABD)                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC4D                                                       
         ORG   ECSWORK                                                          
*              LOCAL SAVED STORAGE AT END OF TWA0                               
         SPACE 3                                                                
SVSSN    DS    XL(L'TGSSN)         SAVED SOCIAL SECURITY NUMBER                 
SVSOSTAT DS    XL1                 SAVED WRITER EQUATE                          
SVFREC   DS    CL1                 Y=DISPLAYED RECORD ON PF14                   
COUNTER  DS    PL4                 REPORT COUNTER                               
CAELEM   DS    CL(L'ELEMENT)       SAVED CAST DETAILS ELEMENT                   
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
PTRBLK   DS    CL(2*L'TLDRREC+1)   BLOCK FOR POINTER MAINTENANCE                
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038TAGENC4   04/28/15'                                      
         END                                                                    
