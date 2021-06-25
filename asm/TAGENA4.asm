*          DATA SET TAGENA4    AT LEVEL 035 AS OF 11/13/14                      
*PHASE T702A4E,*                                                                
         TITLE 'T702A4 - DUE COMPANY TRACKING'                                  
T702A4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A4                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING TWAD,R7                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    DUC05                                                            
         MVC   SDTSHED(6),=C'Pid Num'                                           
         OI    SDTSHEDH+6,X'80'                                                 
*                                                                               
DUC05    CLI   MODE,SETFILE        SET FILE FOR PAGING                          
         BNE   DUC10                                                            
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         B     XIT                                                              
         SPACE 3                                                                
DUC10    CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   DUC30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     DUC40                                                            
         SPACE 3                                                                
DUC30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             INSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         SPACE 1                                                                
DUC40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         MVC   SYSDIR,SVSYSDIR     RESTORE DEFAULT FILE NAMES                   
         MVC   SYSFIL,SVSYSFIL                                                  
         SPACE 1                                                                
         TM    SCRSTAT,RECCHG      IF RECORD TYPE CHANGED                       
         BO    *+12                                                             
         TM    SDTSSNH+4,X'20'     OR IF SSN NOT PREV VALIDATED                 
         BO    VK10                                                             
         LH    RF,=Y(TIEND-TASYSIOD)  CLEAR SYSIO'S W/S                         
         XCEFL TASYSIOD                                                         
         NI    SDTDUCH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
         LA    R2,SDTSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK1                                                              
         CLI   SDTSSNH+5,0                                                      
         BE    VK1A                                                             
         CLI   SDTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK1                 RECVAL CALL DOES NOT CHECK FOR               
         CLI   SDTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SDTSSN                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    MISSERR                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SDTSSN,TGSSN                                                     
         MVI   SDTSSNH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SDTSSNH),SDTSSNNH  VAL PERF.          
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK5                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDTSSN,SPACES                                                    
         MVC   SDTSSN(L'TGPID),TGPID                                            
         MVI   SDTSSNH+5,6                                                      
         OI    SDTSSNH+6,X'80'                                                  
*                                                                               
VK5      MVC   TIFSSN,TGSSN                                                     
*                                                                               
VK10     MVI   SDTFLG,C'T'                                                      
         OI    SDTFLGH+6,X'80'                                                  
*                                                                               
         LA    R2,SDTDUCH                                                       
         TM    4(R2),X'20'         VALIDATE DUE COMPANY CODE                    
         BO    VKX                                                              
         CLI   5(R2),0                   IF NO INPUT                            
         BNE   VK12                                                             
         GOTO1 RECVAL,DMCB,TLDUCDQ,(R2)  USE GLOBALS                            
         B     VKX                                                              
*                                                                               
VK12     GOTO1 ANY                                                              
         CLI   5(R2),6             TEST LENGTH IS 6                             
         BNE   VK14                                                             
         MVC   DUB(2),WORK+2       TEST IF VALID DATE                           
         MVC   DUB+2(2),=C'01'     CHANGE TO M/D/Y FORMAT                       
         MVC   DUB+4(2),WORK                                                    
         GOTO1 DATVAL,DMCB,(0,DUB),WORK+6                                       
         CLI   3(R1),0                                                          
         BE    VK14                NOT VALID DATE                               
         MVC   WORK(4),WORK+6      YES - SO USE INTERNAL FORMAT                 
*                                                                               
VK14     GOTO1 RECVAL,DMCB,TLDUCDQ,(X'A0',WORK)  NON-SCRN DATA                  
         BNE   ERRNOFND                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TADUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VKX                                                              
         USING TADUD,R4                                                         
         TM    TADUSTAT,TADUSNTR                                                
         BZ    *+8                                                              
         MVI   SDTFLG,C'N'                                                      
         TM    TADUSTA2,TADUSTXR                                                
         BZ    *+8                                                              
         MVI   SDTFLG,C'R'                                                      
VKX      MVC   TIQSTART(L'TGDUC),TGDUC                                          
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         XC    TIQSKEY,TIQSKEY                                                  
         MVI   TIREAD,TLCKDCDQ                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
*        MVC   SYSDIR,=CL8'CHKDIR'                                              
*NO-OP   MVC   SYSFIL,=CL8'CHKFIL'                                              
*        GOTO1 PGCNTL,DMCB,TABLE,TIKEY,TIQSKEY                                  
         MVI   NLISTS,17           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,16           BACK AFTER 1 FULL PAGE                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         BAS   RE,PRNTIT                                                        
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(16,R1),=C'TRACKING RECORDS'                                    
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         L     R4,TIAREC                                                        
         CLI   0(R4),TLDTCDQ       IF THIS IS ADJUSTMENT RECORD                 
         BNE   LRH05                                                            
         MVC   LINCHK(6),=C'ADJUST' DISPLAY SPECIAL LIT IN CHECK# FLD           
         USING TLDTD,R4                                                         
         MVC   TICKDATE,TLDTDTE    MOVE DATE IN KEY TO CHECK DATE               
         XC    TICKDATE,=3X'FF'    (UNCOMPLEMENT)                               
         SPACE 1                                                                
LRH05    GOTO1 DATCON,DMCB,(1,TICKDATE),(8,LINDTE)  CHECK DATE                  
         SPACE 1                                                                
         MVI   ELCODE,TACDELQ      LOOK FOR CHECK DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BNE   LRH10                                                            
         USING TACDD,R4                                                         
         MVC   LINCHK,TACDCHK      CHECK NUMBER                                 
         SPACE 1                                                                
LRH10    MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TADWELQ      LOOK FOR DUE COMPANY WITHHOLD EL.            
         GOTO1 GETL,DMCB,(6,TGDUC)                                              
         MVC   AIO,AIO1                                                         
         BNE   LRH15                                                            
         L     R4,TGELEM                                                        
         USING TADWD,R4                                                         
         EDIT  (4,TADWREC),(12,LINREC),2,MINUS=YES                              
         EDIT  (4,TADWBAL),(12,LINBAL),2,MINUS=YES                              
         SPACE 1                                                                
LRH15    MVC   AIO,TIAREC                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,0,TACMTYPC  CHECK COMMENT                   
         MVC   AIO,AIO1                                                         
         MVC   LINCMNT,TGNAME                                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRH20                                                            
         BAS   RE,PRNTIT                                                        
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHX                                                             
         SPACE 1                                                                
LRH20    CLI   LISTNUM,16          END OF 1 PAGE                                
         BNE   LRH30                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SDTSELH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
LRH30    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
LRHX     B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         MVC   HEAD4+12(9),TGSSN                                                
         MVC   HEAD4+23(16),SDTSSNN                                             
         MVC   HEAD5+12(6),TGDUC                                                
         MVC   HEAD5+28(1),SDTFLG                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
*                                                                               
ERRNOFND MVI   ERROR,NOTFOUND      NOT FOUND                                    
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PFKEY TABLE                                  
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYGLB,L'TGSSN),AL2(TGSSN-TGD)                             
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'DUECOMP ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
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
         SSPEC H1,30,C'DUE COMPANY TRACKING'                                    
         SSPEC H2,30,C'--------------------'                                    
         SPACE 1                                                                
         SSPEC H4,2,C'PERFORMER'                                                
         SSPEC H5,2,C'REF #'                                                    
         SSPEC H5,24,C'FLAG'                                                    
         SPACE 1                                                                
         SSPEC H7,2,C'CHK DATE CHECK#     COLLECTED      BALANCE'               
         SSPEC H8,2,C'-------- ------     ---------      -------'               
         SSPEC H7,46,C'COMMENT'                                                 
         SSPEC H8,46,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 2                                                                
LINED    DSECT                                                                  
LINDTE   DS    CL8                 CHECK DATE                                   
         DS    CL1                                                              
LINCHK   DS    CL8                 CHECK NUMBER                                 
         DS    CL1                                                              
LINREC   DS    CL12                AMOUNT RECOVERED                             
         DS    CL1                                                              
LINBAL   DS    CL12                BALANCE                                      
         DS    CL1                                                              
LINCMNT  DS    CL30                CHECK COMMENT                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA4D                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
         SPACE 3                                                                
* DDGENTWA     (MUST FOLLOW LAST SCREEN)                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035TAGENA4   11/13/14'                                      
         END                                                                    
