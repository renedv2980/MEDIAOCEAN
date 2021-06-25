*          DATA SET TAGENA3    AT LEVEL 006 AS OF 12/20/13                      
*PHASE T702A3A,*                                                                
         TITLE 'T702A3 - PMUSIC LIST/MAINTENANCE'                               
T702A3   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A3,R8,R7,R5                                                
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
         LA    RF,LPFTAB                                                        
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+8                                                              
         LA    RF,PFTAB                                                         
         GOTO1 INITIAL,DMCB,(RF)                                                
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+12                                                             
         BAS   RE,DKEY                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+12                                                             
         BAS   RE,DREC                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         JNE   PMUS10                                                           
*                                                                               
         GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY ELEMENT                      
         GOTO1 PUTREC                                                           
*                                                                               
         BAS   RE,CHKCMVER         CHECK IF COMMERCIAL/VERSION EXISTS           
         GOTO1 SAVPTRS,DMCB,PTRBLK HANDLE PASSIVE POINTERS                      
         J     XIT                                                              
*                                                                               
PMUS10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+12                                                             
         BAS   RE,VKEY                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         J     XIT                                                              
*                                                                               
         CLI   MODE,XRECDEL        RECORD DELETED                               
         JNE   PMUS20                                                           
         GOTO1 ADDPTRS,DMCB,PTRBLK HANDLE PASSIVE POINTERS                      
         J     XIT                                                              
*                                                                               
PMUS20   CLI   MODE,XRECREST       RECORD RESTORED                              
         JE    PMUS30                                                           
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         JE    PMUS30                                                           
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         JNE   PMUS40                                                           
PMUS30   GOTO1 ADDPTRS,DMCB,PTRBLK HANDLE PASSIVE POINTERS                      
         BAS   RE,DREC                                                          
         J     XIT                                                              
*                                                                               
PMUS40   CLI   MODE,LISTRECS       LIST RECORD                                  
         JNE   PMUS50                                                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         J     PMUS60                                                           
*                                                                               
PMUS50   CLI   MODE,PRINTREP       PRINT RECORD                                 
         JNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
*                                                                               
PMUS60   BAS   RE,LREC             GO LIST/PRINT THE RECORDS                    
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE THE KEY                                                   
*                                                                               
VKEY     NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   *+12                                                             
         BAS   RE,LVKEY                                                         
         J     XIT                                                              
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    VK10                                                             
         LA    R2,PMUMUSH          MUSIC #                                      
         CLI   5(R2),0                                                          
         JNE   VK00                                                             
         OC    TGMUS,TGMUS                                                      
         JZ    ERRMISS                                                          
         MVC   PMUMUS,TGMUS                                                     
         OI    4(R2),X'08'                                                      
         MVI   5(R2),8                                                          
VK00     CLI   5(R2),8                                                          
         JNE   ERRINV                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         JZ    ERRNUM                                                           
         J     VK20                                                             
*                                                                               
VK10     MVC   AIO,AIO2            GET SYSTEM REC FOR MUSIC #                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',TWAAGY)                               
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
                                                                                
         L     R4,AIO2             GET SYSTEM ELEMENT                           
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ICM   RF,15,TASYLPMU      GET NEXT AVAILABLE MUSIC #                   
         AHI   RF,1                                                             
         EDIT  (RF),PMUMUS,ALIGN=RIGHT,FILL=0                                   
         OI    PMUMUSH+6,X'80'                                                  
         DROP  R4                                                               
*                                                                               
VK20     XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLMUD,R3                                                         
         MVI   TLMUCD,TLMUCDQ      MUSIC RECORD                                 
         MVC   TLMUMUS,PMUMUS      FROM SCREEN                                  
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE THE KEY (LIST)                                            
*                                                                               
LVKEY    NTR1                                                                   
         XC    FLTVALS(FLTVALNQ),FLTVALS                                        
*                                                                               
         CLI   PMLAGYH+5,0         AGENCY                                       
         JE    LVK10                                                            
         GOTO1 RECVAL,DMCB,TLAYCDQ,PMLAGYH                                      
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         MVC   FLTAGY,TGAGY                                                     
*                                                                               
LVK10    CLI   PMLCLIH+5,0         CLIENT NAME                                  
         JE    LVK20                                                            
         MVC   FLTCLI,PMLCLI       SAVE CLIENT NAME FILTER                      
         OC    FLTCLI,SPACES                                                    
*                                                                               
LVK20    LA    R2,PMLFMTH          FORMAT                                       
         MVI   FLTFMT,C'C'         DEFAULT TO MUSIC CODE ORDER                  
         CLI   5(R2),0                                                          
         JE    LVK30                                                            
         CLI   8(R2),C'A'          LIST BY COMP NAME IN ALPHA ORDER             
         JE    *+12                                                             
         CLI   8(R2),C'C'          LIST BY MUSIC CODE ORDER                     
         JNE   ERRINV                                                           
         MVC   FLTFMT,8(R2)        SAVE FORMAT FILTER                           
*                                                                               
LVK30    XC    TIQSTART,TIQSTART                                                
         CLI   PMLSTRH+5,0         START                                        
         JE    LVK40                                                            
         MVC   TIQSTART(L'TLMUMUS),PMLSTR  SET START FILTER                     
*                                                                               
LVK40    MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF   STAFF                                        
         MVI   TIREAD,TLMUCDQ      READ PMUSIC RECORDS                          
         CLI   FLTFMT,C'A'         LIST BY COMPOSITION NAME?                    
         JNE   *+8                                                              
         MVI   TIREAD,TLMUPCDQ     READ PMUSIC PASSIVE                          
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD DISPLAY                                               
*                                                                               
DREC     NTR1                                                                   
         BAS   RE,CLEARSCR         CLEAR SCREEN FOR DISPLAY                     
*                                                                               
         L     R4,AIO                                                           
         USING TAPMD,R4                                                         
         MVI   ELCODE,TAPMELQ      LOOK FOR INFO/HISTORY ELEMENT                
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PMUCAGY,TAPMCAGY    CURRENT AGENCY                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',PMUCAGY),PMUCAGNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         L     RF,AIO                                                           
         MVC   KEY(L'TLMUKEY),0(RF)                                             
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
*                                                                               
         MVC   PMUPAGY,TAPMPAGY    PREVIOUS AGENCY                              
         MVC   PMUOAGY,TAPMOAGY    ORIGINAL AGENCY                              
         OC    TAPMOMUS,TAPMOMUS   ORIGINAL MUSIC CODE?                         
         JZ    DR04                                                             
         MVC   PMUOMUS(4),=C'Code'                                              
         MVC   PMUOMUS+6(L'TAPMOMUS),TAPMOMUS  ORIGINAL MUSIC CODE              
         DROP  R4                                                               
*                                                                               
DR04     L     R4,AIO                                                           
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ      LOOK FOR COMPOSITION NAME ELEMENT            
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PMUNAME,SPACES                                                   
         ZIC   R1,TANALEN          ELEMENT LENGTH                               
         SHI   R1,TANALNQ          R1 = L'NAME                                  
*                                                                               
         CHI   R1,L'PMUNAME                                                     
         JL    DR06                                                             
         MVC   PMUNAME(L'PMUNAME),TANANAME      COMPOSITION NAME 1              
         SHI   R1,L'PMUNAME                                                     
         LTR   R1,R1                                                            
         JZ    DR08                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   PMUNAM2(0),TANANAME+L'PMUNAME    COMPOSITION NAME 2              
         J     DR08                                                             
*                                                                               
DR06     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   PMUNAME(0),TANANAME    COMPOSITION NAME                          
*                                                                               
DR08     LA    RF,PMUPUB1H                                                      
         ST    RF,ADPUB                                                         
         LA    RF,PMUCOM1H                                                      
         ST    RF,ADCOM                                                         
         LA    RF,PMUAUT1H                                                      
         ST    RF,ADAUT                                                         
*                                                                               
         L     R4,AIO                                                           
         USING TAMUD,R4                                                         
         MVI   ELCODE,TAMUELQ      MUSIC ELEMENTS                               
         BAS   RE,GETEL                                                         
         JE    DR12                                                             
         DC    H'0'                                                             
DR10     BAS   RE,NEXTEL                                                        
DR12     JNE   DR50                                                             
*                                                                               
         L     R2,ADAUT                                                         
         CLI   TAMUTYPE,TAMUTAUT                                                
         JE    DR20                                                             
         L     R2,ADCOM                                                         
         CLI   TAMUTYPE,TAMUTCOM                                                
         JE    DR20                                                             
         L     R2,ADPUB                                                         
         CLI   TAMUTYPE,TAMUTPUB                                                
         JNE   DR10                                                             
*                                                                               
DR20     MVC   WORK,SPACES                                                      
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SHI   R1,TAMULNQ          R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   8(0,R2),TAMUNAME                                                 
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(1,R2),TAMULIC     DISPLAY LISCENSOR                            
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         CLI   TAMUTYPE,TAMUTAUT                                                
         JNE   *+12                                                             
         ST    R2,ADAUT                                                         
         J     DR10                                                             
*                                                                               
         CLI   TAMUTYPE,TAMUTCOM                                                
         JNE   *+12                                                             
         ST    R2,ADCOM                                                         
         J     DR10                                                             
*                                                                               
         CLI   TAMUTYPE,TAMUTPUB                                                
         JNE   *+12                                                             
         ST    R2,ADPUB                                                         
         J     DR10                                                             
         DROP  R4                                                               
*                                                                               
DR50     GOTO1 CHAROUT,DMCB,TAFNELQ,PMUCLIH,TAFNTCLI     CLIENT NAME            
         GOTO1 (RF),(R1),TAFNELQ,PMUPRDH,TAFNTPRD        PRODUCT                
         GOTO1 (RF),(R1),TACMELQ,(2,PMUCMT1H),TACMTYPG   COMMENTS               
         GOTO1 ACTVOUT,DMCB,PMULCHGH                     ACTIVITY               
                                                                                
         CLI   MODE,XRECADD                                                     
         JNE   XIT                                                              
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+6,X'80'                                                  
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS KEY DISPLAY                                                  
*                                                                               
DKEY     NTR1                                                                   
         LA    R3,KEY                                                           
         USING TLMUD,R3                                                         
*                                                                               
         MVC   PMUMUS,TLMUMUS      MUSIC CODE                                   
         OI    PMUMUSH+6,X'80'                                                  
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* CHECK IF COMMERCIAL/VERSION EXISTS                                            
*                                                                               
CHKCMVER NTR1                                                                   
         L     R4,AIO                                                           
         USING TLMUD,R4                                                         
*                                                                               
         LA    R3,KEY              CHECK IF CMML/VERS EXISTS                    
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOOCDQ    MUSIC CMML/VERS PASSIVE                      
         MVC   TLCOOMUS,TLMUMUS    MUSIC CODE                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOOCOM-TLCOPKEY),KEYSAVE                                   
         JE    CANTDEL                                                          
*                                                                               
         XC    KEY,KEY             RESTORE GETREC/PUTREC                        
         MVC   KEY(L'TLMUKEY),0(R4)                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD LISTING                                               
*                                                                               
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
*                                                                               
         MVI   NLISTS,17           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16           BACK AFTER 1 FULL PAGE                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         JNE   XIT                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         JE    XIT                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=C'MUSIC RECORDS'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         JZ    XIT                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* PROCESS SYSIO RECORDS (LIST)                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         JNE   XIT                                                              
*                                                                               
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
         MVC   LISTD(LISTLNQ),SPACES                                            
*                                                                               
         USING TAPMD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPMELQ      LOOK FOR INFO/HISTORY ELEMENT                
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   LSTAGY,TAPMCAGY     SAVE AGENCY                                  
*                                                                               
         OC    FLTAGY,FLTAGY       AGENCY FILTER?                               
         JZ    LRH10                                                            
         CLC   FLTAGY,TAPMCAGY                                                  
         JE    LRH10                                                            
         DROP  R4                                                               
*                                                                               
         L     R4,TIAREC                                                        
         USING TLMUD,R4                                                         
*                                                                               
         LA    RF,KEY              CHECK IF COMMERCIAL PASSIVE EXISTS           
         USING TLCOPD,RF           FOR FILTER AGENCY                            
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOMCDQ                                                 
         MVC   TLCOMAGY,FLTAGY                                                  
         MVC   TLCOMMUS,TLMUMUS                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOMCID-TLCOPKEY),KEYSAVE                                   
         JE    LRH05                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLMUKEY),TIKEY                                             
         GOTO1 HIGH                                                             
         J     XIT                                                              
*                                                                               
LRH05    XC    KEY,KEY                                                          
         MVC   KEY(L'TLMUKEY),TIKEY                                             
         GOTO1 HIGH                                                             
*                                                                               
LRH10    L     R4,TIAREC                                                        
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ      LOOK FOR CLIENT NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         J     *+8                                                              
LRH20    BAS   RE,NEXTEL                                                        
         JNE   LRH30                                                            
         CLI   TAFNTYPE,TAFNTCLI   GET CLIENT NAME ELEM                         
         JNE   LRH20                                                            
*                                                                               
         MVC   LSTCLI,SPACES                                                    
         ZIC   R1,TAFNLEN          ELEMENT LENGTH                               
         SHI   R1,TAFNLNQ          R1 = L'NAME                                  
         BCTR  R1,0                                                             
         CHI   R1,L'LSTCLI         MORE THAN SCREEN AREA?                       
         JL    *+14                                                             
         MVC   LSTCLI,TAFNNAME                                                  
         J     LRH25                                                            
*                                                                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   LSTCLI(0),TAFNNAME    CLIENT NAME                                
*                                                                               
LRH25    OC    FLTCLI,FLTCLI       ANY CLIENT NAME FILTER?                      
         JZ    *+14                                                             
         CLC   LSTCLI,FLTCLI                                                    
         JNE   XIT                                                              
*                                                                               
LRH30    L     R4,TIAREC                                                        
         USING TLMUD,R4                                                         
         MVC   LSTMUSIC,TLMUMUS    MUSIC CODE                                   
         DROP  R4                                                               
*                                                                               
         L     R4,TIAREC                                                        
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ      LOOK FOR COMPOSITION NAME ELEMENT            
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R1,TANALEN          ELEMENT LENGTH                               
         SHI   R1,TANALNQ          R1 = L'NAME                                  
         BCTR  R1,0                                                             
         CHI   R1,L'LSTNAME        MORE THAN SCREEN AREA?                       
         JL    *+14                                                             
         MVC   LSTNAME,TANANAME                                                 
         J     LRH35                                                            
*                                                                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   LSTNAME(0),TANANAME    COMPOSITION NAME                          
         DROP  R4                                                               
*                                                                               
LRH35    CLI   MODE,PRINTREP                                                    
         JNE   LRH40                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         J     XIT                                                              
*                                                                               
LRH40    CLI   LISTNUM,16          END OF 1 PAGE                                
         JNE   LRH50                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,PMLSELH                                                       
         GOTO1 EXIT,DMCB,0         AND DON'T COME BACK NO MORE, NO MORE         
*                                                                               
LRH50    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE CONTROLS RECORD VALIDATION & BUILDING                                 
*                                                                               
BLDREC   NTR1                                                                   
         LA    R2,PMUCAGYH         CURRENT AGENCY IS REQUIRED                   
         CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         USING TLMUD,R4                                                         
         CLI   ACTNUM,ACTADD                                                    
         JE    BR00                                                             
         L     R4,AIO1                                                          
         MVC   KEY(L'TLMUKEY),0(R4)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLMUKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
BR00     MVC   AIO,AIO1                                                         
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
                                                                                
         CLI   ACTNUM,ACTADD       IF ADDING RECORD                             
         JNE   BR10                                                             
         USING TAPMD,R4                                                         
         LA    R4,ELEMENT          INITIALIZE MUSIC INFO/HISTORY                
         XC    ELEMENT,ELEMENT     ELEMENT                                      
         MVI   TAPMEL,TAPMELQ                                                   
         MVI   TAPMLEN,TAPMLNQ                                                  
         MVC   TAPMOAGY,TGAGY                                                   
         GOTO1 ADDELEM             AND ADD IT TO RECORD                         
         J     BR20                                                             
         DROP  R4                                                               
                                                                                
BR10     MVI   ELCODE,TAFNELQ      IF CHANGING EXISTING RECORD                  
         GOTO1 REMELEM             REMOVE OLD ELEMENTS                          
         MVI   ELCODE,TANAELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAMUELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING TAPMD,R4                                                         
BR20     L     R4,AIO              GET MUSIC HISTORY INFO/ELEMENT               
         MVI   ELCODE,TAPMELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   ACTNUM,ACTADD       IF CHANGING EXISTING RECORD                  
         JE    BR30                                                             
         CLC   TAPMCAGY,TGAGY      AND CURRENT AGENCY IS CHANGING               
         JE    BR30                                                             
         MVC   TAPMPAGY,TAPMCAGY   SAVE PREVIOUS AGENCY                         
                                                                                
BR30     MVC   TAPMCAGY,TGAGY      ALWAYS, SAVE CURRENT AGENCY                  
         DROP  R4                                                               
                                                                                
BR40     XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TANAD,R3                                                         
         MVI   TANAEL,TANAELQ      NAME ELEMENT                                 
         MVI   TANALEN,TANALNQ                                                  
                                                                                
         LA    R2,PMUNAMEH         COMPOSITION NAME REQUIRED                    
         CLI   5(R2),0                                                          
         JNE   *+12                                                             
         CLI   PMUNAM2H+5,0                                                     
         JE    ERRMISS                                                          
                                                                                
         LA    R4,TANANAME                                                      
         CLI   PMUNAMEH+5,0        ANYTHING ON 1ST LINE?                        
         JE    BR50                                                             
         MVC   0(L'PMUNAME,R4),PMUNAME                                          
         OC    0(L'PMUNAME,R4),SPACES                                           
         ZIC   RF,TANALEN                                                       
         AHI   RF,L'PMUNAME                                                     
         STC   RF,TANALEN                                                       
         AHI   R4,L'PMUNAME                                                     
                                                                                
BR50     CLI   PMUNAM2H+5,0        ANYTHING ON 2ND LINE?                        
         JE    BR55                                                             
         MVC   0(L'PMUNAM2,R4),PMUNAM2                                          
         OC    0(L'PMUNAM2,R4),SPACES                                           
         ZIC   RF,TANALEN                                                       
         AHI   RF,L'PMUNAM2                                                     
         STC   RF,TANALEN                                                       
                                                                                
BR55     GOTO1 ADDELEM             ADD THE ELEMENT                              
                                                                                
BR60     MVI   CURNUM,1                                                         
         MVI   BYTE,0                                                           
         GOTO1 BLDTAMU,DMCB,('TAMUTPUB',PMUPUB1H)                               
         GOTO1 (RF),(R1),('TAMUTPUB',PMUPUB2H)                                  
         GOTO1 (RF),(R1),('TAMUTPUB',PMUPUB3H)                                  
                                                                                
         LA    R2,PMUPUB1H         PUBLISHER IS REQUIRED                        
         CLI   BYTE,0                                                           
         JE    ERRMISS                                                          
                                                                                
         MVI   CURNUM,1                                                         
         MVI   BYTE,0                                                           
         GOTO1 (RF),(R1),('TAMUTCOM',PMUCOM1H)                                  
         GOTO1 (RF),(R1),('TAMUTCOM',PMUCOM2H)                                  
         GOTO1 (RF),(R1),('TAMUTCOM',PMUCOM3H)                                  
         GOTO1 (RF),(R1),('TAMUTCOM',PMUCOM4H)                                  
                                                                                
         LA    R2,PMUCOM1H         COMPOSER IS REQUIRED                         
         CLI   BYTE,0                                                           
         JE    ERRMISS                                                          
                                                                                
         MVI   CURNUM,1                                                         
         GOTO1 BLDTAMU,DMCB,('TAMUTAUT',PMUAUT1H)                               
         GOTO1 BLDTAMU,DMCB,('TAMUTAUT',PMUAUT2H)                               
                                                                                
         GOTO1 NAMIN,DMCB,TAFNELQ,PMUCLIH,TAFNTCLI           CLIENT             
         GOTO1 (RF),(R1),TAFNELQ,(X'80',PMUPRDH),TAFNTPRD   PRODUCT             
         GOTO1 (RF),(R1),(2,TACMELQ),(X'80',PMUCMT1H),TACMTYPG                  
                                                                                
         GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY ELEMENT                      
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO2            GET SYSTEM RECORD                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',TWAAGY)                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO2             GET SYSTEM ELEMENT                           
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,TASYLPMU      GET NEXT AVAILABLE MUSIC #                   
         AHI   R2,1                                                             
         STCM  R2,15,TASYLPMU                                                   
         GOTO1 PUTREC              UPDATE SYSTEM REC W/ NEXT MUSIC #            
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TLMUD,R4                                                         
         L     R4,AIO                                                           
         EDIT  (R2),PMUMUS,ALIGN=RIGHT,FILL=0                                   
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO BUILD MUSIC ELEMENT                                                
* - ON ENTRY, P1=A(AUTHOR,COMPOSER/PUBLISHER FIELD)                             
*             P1 BYTE 0=TYPE(A/C/P)                                             
*                                                                               
BLDTAMU  NTR1                                                                   
         XR    R0,R0               INIT 1ST FIELD POPULATED STATUS              
                                                                                
         ZICM  R2,1(R1),3                                                       
         CLI   5(R2),0             IF 1ST FIELD IS POPULATED                    
         JE    BTAMU10                                                          
         LHI   R0,1                UPDATE 1ST FIELD POPULATED STATUS            
*                                                                               
         MVC   CURTYPE,0(R1)                                                    
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TAMUD,R3                                                         
         MVI   TAMUEL,TAMUELQ      MUSIC ELEMENT                                
         MVI   TAMULEN,X'2A'                                                    
         MVC   TAMUTYPE,CURTYPE    TYPE                                         
         MVC   TAMUTNUM,CURNUM     NUMBER                                       
         MVC   TAMUNAME,8(R2)                                                   
         OC    TAMUNAME,SPACES                                                  
*                                                                               
BTAMU10  ZIC   RF,0(R2)            BUMP TO LISCENSER                            
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         LTR   R0,R0               IF 1ST FIELD WAS NOT POPULATED               
         JNZ   BTAMU20                                                          
         CLI   5(R2),0             LICENSER FIELD CANNOT BE POPULATED           
         JNE   ERRINV                                                           
         J     XIT                                                              
                                                                                
BTAMU20  CLI   5(R2),0                                                          
         JE    ERRMISS                                                          
         GOTO1 LICVAL,DMCB,(X'80',8(R2))                                        
         JNE   ERRINV                                                           
         MVC   TAMULIC,TGLCCDE     LISCENSER                                    
         DROP  R3                                                               
                                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
                                                                                
         ZIC   RE,CURNUM                                                        
         AHI   RE,1                                                             
         STC   RE,CURNUM                                                        
         MVI   BYTE,1                                                           
         J     XIT                                                              
*                                                                               
* ROUTINE TO CLEAR SCREEN FOR DISPLAY                                           
*                                                                               
CLEARSCR NTR1                                                                   
         XC    PMUPAGY,PMUPAGY                                                  
         XC    PMUCAGN,PMUCAGN                                                  
         XC    PMUOAGY,PMUOAGY                                                  
         XC    PMUOMUS,PMUOMUS                                                  
         XC    PMUPL1N,PMUPL1N                                                  
         XC    PMUPL2N,PMUPL2N                                                  
         XC    PMUPL3N,PMUPL3N                                                  
         XC    PMUCL1N,PMUCL1N                                                  
         XC    PMUCL2N,PMUCL2N                                                  
         XC    PMUCL3N,PMUCL3N                                                  
         XC    PMUAL1N,PMUAL1N                                                  
         XC    PMUAL2N,PMUAL2N                                                  
*                                                                               
         GOTO1 FLDVAL,DMCB,(1,PMUCAGYH),(X'80',999)                             
         GOTO1 (RF),(R1),(2,PMUPAGYH),999                                       
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* ERROR/INFO MESSAGES                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     ERREXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     ERREXIT                                                          
*                                                                               
ERRNUM   MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         J     ERREXIT                                                          
*                                                                               
CANTDEL  MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         J     ERREXIT                                                          
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREXIT2                                                         
                                                                                
ERREXIT2 MVI   BLOCK,0                                                          
         MVI   MYMTYP,C'E'                                                      
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*                                                                               
* EXITS                                                                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
* CONSTANTS, ETC.                                                               
*                                                                               
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3'   ',CL8'PMUSIC',CL8'LIST'                                   
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'COM',CL8'LIST'                                      
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'PMUMUS-1),AL2(PMUMUS-T702FFD)                     
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
LPFTAB   DS    0C                                                               
         DC    AL1(LPF13X-*,13,0,(LPF13X-LPF13)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'COM',CL8'LIST'                                      
LPF13    DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LSTMUSIC-1),AL2(LSTMUSIC-LISTD)                   
LPF13X   EQU   *                                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* REPORT SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
*                                                                               
         SSPEC H1,33,C'MUSIC LIST'                                              
         SSPEC H2,33,C'----------'                                              
*                                                                               
         SSPEC H4,2,C'AGENCY  CLIENT NAME      MUSIC'                           
         SSPEC H5,2,C'------  -----------      -----'                           
         SSPEC H4,37,C'COMPOSITION NAME'                                        
         SSPEC H5,37,C'----------------'                                        
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LIST LINE                                                      
*                                                                               
LISTD    DSECT                                                                  
LSTAGY   DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LSTCLI   DS    CL15                CLIENT NAME                                  
         DS    CL2                                                              
LSTMUSIC DS    CL8                 MUSIC NUMBER                                 
         DS    CL2                                                              
LSTNAME  DS    CL31                COMPOSITION NAME                             
LISTLNQ  EQU   *-LISTD                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5BD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR5CD                                                       
*                                                                               
COUNTER  DS    PL4                 RECORD COUNTER                               
ADPUB    DS    A                   A(DISP PUBLISHER)                            
ADCOM    DS    A                   A(DISP COMPOSER)                             
ADAUT    DS    A                   A(DISP AUTHOR)                               
*                                                                               
CURTYPE  DS    CL1                                                              
CURNUM   DS    XL1                                                              
*                                                                               
FLTVALS  DS    0C                                                               
FLTAGY   DS    CL(L'PMLAGY)        AGENCY FILTER                                
FLTCLI   DS    CL(L'PMLCLI)        CLIENT NAME FILTER                           
FLTFMT   DS    CL(L'PMLFMT)        FORMAT FILTER                                
FLTVALNQ EQU   *-FLTVALS                                                        
*                                                                               
PTRBLK   DS    CL(2*L'TLDRREC+1)   1 PASSIVE                                    
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAGENA3   12/20/13'                                      
         END                                                                    
