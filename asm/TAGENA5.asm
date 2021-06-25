*          DATA SET TAGENA5    AT LEVEL 010 AS OF 06/03/05                      
*PHASE T702A5A                                                                  
         TITLE 'T702A5 - LIEN TRACKING'                                         
T702A5   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A5                                                         
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
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    LIN05                                                            
         MVC   SLTSHED(6),=C'Pid Num'                                           
         OI    SLTSHEDH+6,X'80'                                                 
*                                                                               
LIN05    CLI   MODE,SETFILE        SET FILE FOR PAGING                          
         BNE   LIN10                                                            
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         B     XIT                                                              
         SPACE 3                                                                
LIN10    CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   LIN30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LIN40                                                            
         SPACE 1                                                                
LIN30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             INSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         SPACE 1                                                                
LIN40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         MVC   SYSDIR,SVSYSDIR     RESTORE DEFAULT FILE NAMES                   
         MVC   SYSFIL,SVSYSFIL                                                  
         SPACE 1                                                                
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    SLTSSNH+4,X'20'     OR NOT PREVIOUSLY VALIDATED                  
         BO    VK10                                                             
         NI    SLTLINH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
         LA    R2,SLTSSNH          S/S NUM                                      
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK01                                                             
         CLI   SLTSSNH+5,0                                                      
         BE    VK01A                                                            
         CLI   SLTSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK01                RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLTSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SLTSSN                                                     
VK01A    OC    TGPID,TGPID                                                      
         BZ    MISSERR                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK01                                                             
         MVC   SLTSSN,TGSSN                                                     
         MVI   SLTSSNH+5,9                                                      
*                                                                               
VK01     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SLTSSNH),SLTSSNNH  VAL PERF.          
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK05                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLTSSN,SPACES                                                    
         MVC   SLTSSN(L'TGPID),TGPID                                            
         MVI   SLTSSNH+5,6                                                      
         OI    SLTSSNH+6,X'80'                                                  
*                                                                               
VK05     MVC   TIFSSN,TGSSN                                                     
*                                                                               
VK10     TM    SLTLINH+4,X'20'     VALIDATE LIEN CODE                           
         BO    VKX                                                              
         GOTO1 RECVAL,DMCB,TLLNCDQ,SLTLINH                                      
         MVC   TIQSTART(L'TGLIN),TGLIN                                          
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCKLCDQ                                                  
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
         GOTO1 DATCON,DMCB,(1,TICKDATE),(8,LINDTE)  CHECK DATE                  
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACDELQ      LOOK FOR CHECK DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BNE   LRH10                                                            
         USING TACDD,R4                                                         
         MVC   LINCHK,TACDCHK      CHECK NUMBER                                 
         SPACE 1                                                                
LRH10    L     R4,TIAREC                                                        
         MVI   ELCODE,TALWELQ      LOOK FOR LIEN WITHHOLD EL.                   
         BAS   RE,GETEL                                                         
         BNE   LRH15                                                            
         USING TALWD,R4                                                         
         EDIT  (4,TALWREC),(12,LINREC),2,MINUS=YES                              
         EDIT  (4,TALWBAL),(12,LINBAL),2,MINUS=YES                              
         SPACE 1                                                                
LRH15    CLI   MODE,PRINTREP                                                    
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
         LA    R2,SLTSELH                                                       
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
         MVC   HEAD4+23(16),SLTSSNN                                             
         MVC   HEAD5+12(6),TGLIN                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
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
         DC    CL3' ',CL8'LIEN    ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYGLB,L'TGSSN),AL2(TGSSN-TGD)                             
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'LIEN    ',CL8'DISPLAY '                               
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
         SSPEC H1,33,C'LIEN TRACKING'                                           
         SSPEC H2,33,C'-------------'                                           
         SPACE 1                                                                
         SSPEC H4,2,C'PERFORMER'                                                
         SSPEC H5,2,C'REF #'                                                    
         SPACE 1                                                                
         SSPEC H7,2,C'CHK DATE  CHK NUMBER   COLLECTED      BALANCE'            
         SSPEC H8,2,C'--------  ----------   ---------      ------- '           
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL1                                                              
LINDTE   DS    CL8                 CHECK DATE                                   
         DS    CL2                                                              
LINCHK   DS    CL8                 CHECK NUMBER                                 
         DS    CL3                                                              
LINREC   DS    CL12                AMOUNT RECOVERED                             
         DS    CL1                                                              
LINBAL   DS    CL12                BALANCE                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA5D                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGENA5   06/03/05'                                      
         END                                                                    
