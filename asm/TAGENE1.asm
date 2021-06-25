*          DATA SET TAGENE1    AT LEVEL 001 AS OF 07/06/04                      
*PHASE T702E1A,*                                                                
         TITLE 'T702E1 - MARKET LIST '                                          
T702E1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702E1                                                         
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
         BRAS  RE,SETUP                                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY FIELDS                          
         SPACE 1                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MT30                                                             
         LA    R2,LISTAR           R2=A(DISPLAY LINE)                           
         B     MT40                                                             
         SPACE 1                                                                
MT30     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    RF,HOOK             SET A(HEADLINE HOOK)                         
         ST    RF,HEADHOOK                                                      
         LA    RF,MYSPECS          SET A(SPECS)                                 
         ST    RF,SPECS                                                         
         XC    COUNTER,COUNTER     CLEAR LINE COUNTER                           
         LA    R2,P                R2=A(DISPLAY LINE)                           
         SPACE 1                                                                
MT40     BRAS  RE,LR               GO LIST THE RECORDS                          
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE                                                                  
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,SMTHEADH,H4-1                               
         GOTO1 (RF),(R1),SMTHEADH,SMTSELH,H7-5                                  
         MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              EQUATES FOR MASTER RECORD TYPES                                  
CNET     EQU   C'N'                CABLE NETWORK                                
RMKT     EQU   C'R'                RADIO MARKET                                 
CSYS     EQU   C'S'                CABLE SYSTEM                                 
TMKT     EQU   C'T'                TELEVISION MARKET                            
         SPACE 2                                                                
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H8,5,C'------    --------------------------------'               
         SSPEC H8,48,C'-----  ------    -----------'                            
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              SET UP THE SCREEN                                                
         SPACE 1                                                                
SETUP    NTR1  BASE=*,LABEL=*                                                   
         MVC   TGMTTYPE,CONREC   SET MASTER RECORD TYPE                         
         CLI   TGMTTYPE,C'C'     CABLE NET, RADIO MARKET,                       
         BNE   SETUP10           TELEVISION MARKET OR CABLE SYSTEM              
         MVC   TGMTTYPE,CONREC+1                                                
         SPACE 1                                                                
SETUP10  MVC   SMTHEAD,TKEYHD    SET UP LIST HEADER FIELDS                      
         MVC   SMTALPH,TALPHD                                                   
         MVC   SMTWGHT,TWGTHD                                                   
         MVC   SMTSWGT,TSWTHD                                                   
         CLI   TGMTTYPE,TMKT     FOR TELEVISION MARKET RECORDS                  
         BE    SETUP20                                                          
         SPACE 1                                                                
         MVC   SMTHEAD,RKEYHD    SET UP LIST HEADER FIELD                       
         MVC   SMTALPH,SPACES                                                   
         CLI   TGMTTYPE,RMKT     FOR RADIO MARKET RECORDS                       
         BE    SETUP20                                                          
         SPACE 1                                                                
         MVC   SMTHEAD,NKEYHD    SET UP LIST HEADER FIELDS                      
         MVC   SMTSWGT,SPACES                                                   
         CLI   TGMTTYPE,CNET     FOR CABLE NETWORK RECORDS                      
         BE    SETUP20                                                          
         SPACE 1                                                                
         MVC   SMTHEAD,SKEYHD    SET UP LIST HEADER FIELDS                      
         MVC   SMTWGHT,TSUBHD                                                   
         CLI   TGMTTYPE,CSYS     FOR CABLE SYSTEM RECORDS                       
         BE    SETUP20                                                          
         SPACE 1                                                                
         DC    H'00'                                                            
         SPACE 1                                                                
SETUP20  GOTO1 FLDVAL,DMCB,(X'02',SMTHEADH),SMTSWGTH                            
         SPACE 1                                                                
         CLC   TGMTTYPE,LASTTYPE   IF RECORD TYPE HAS CHANGED                   
         BE    SETUPX              MAKE GENCON THINK SCREEN                     
         NI    SMTSTRTH+1,X'FF'-X'20'           CHANGED TOO                     
         OI    SMTSTRTH+4,X'80'                                                 
         MVC   LASTTYPE,TGMTTYPE                                                
         BRAS  RE,VK                                                            
SETUPX   XIT1                                                                   
         SPACE 2                                                                
*              LITERALS FOR COLUMN HEADERS                                      
NKEYHD   DC    CL(L'SMTHEAD)'Network'                                           
RKEYHD   DC    CL(L'SMTHEAD)'Alpha'                                             
SKEYHD   DC    CL(L'SMTHEAD)'System'                                            
TKEYHD   DC    CL(L'SMTHEAD)'Market#'                                           
TALPHD   DC    CL(L'SMTALPH)'Alpha'                                             
TWGTHD   DC    CL(L'SMTWGHT)'Weight'                                            
TSWTHD   DC    CL(L'SMTSWGT)'Span Weight'                                       
TSUBHD   DC    CL(L'SMTWGHT)'Subscb'                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 1                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         MVI   TIREAD,TLMTNMDQ                                                  
         CLI   SMTFMT,C'C'                                                      
         BNE   VK10                                                             
         MVI   TIREAD,TLMTCDQ                                                   
         SPACE 1                                                                
VK10     TM    SMTSTRTH+4,X'20'    IF START ALEADY VALIDATED                    
         BNO   VK20                                                             
         TM    SMTFMTH+4,X'20'     AND FORMAT ALREADY VALIDATED                 
         BO    VKX                 THEN SKIP THIS ROUTINE                       
         SPACE 1                                                                
VK20     XC    TIQSTART,TIQSTART                                                
         SPACE 1                                                                
         CLI   SMTSTRTH+5,0                                                     
         BE    VK30                                                             
         MVC   TIQSTART(L'SMTSTRT),SMTSTRT                                      
         SPACE 1                                                                
VK30     CLI   SMTFMTH+5,0                                                      
         BE    VK40                                                             
         CLI   SMTFMT,C'A'                                                      
         BE    VK40                                                             
         CLI   SMTFMT,C'C'                                                      
         BNE   VKERRINV                                                         
         SPACE 1                                                                
VK40     OI    SMTSTRTH+4,X'20'    SET FIELD AS VALIDATED                       
         OI    SMTFMTH+4,X'20'                                                  
         XC    KEY,KEY             AND RE-INITIALIZE LIST                       
VKX      XIT1                                                                   
         SPACE 2                                                                
VKERRINV MVI   ERROR,INVALID                                                    
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         OI    TIQFLAGS,TIQFDIR                                                 
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TIQSKEY,TIQSKEY     END OF LIST - CLEAR CONTINUE KEY             
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         OC    COUNTER,COUNTER     IF ANYTHING REPORTED                         
         BZ    LRX                                                              
         BRAS  RE,PRNTIT           SKIP A LINE                                  
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         GOTO1 FLDVAL,DMCB,(X'01',SMTSELH),(X'80',SMTLAST)                      
         B     LRX                                                              
         SPACE 1                                                                
LRYES    XR    RC,RC                                                            
LRNO     LTR   RC,RC                                                            
LRX      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*               PROCESS SYSIO RECORDS TO SCREEN                                 
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR                                                   
         BE    FILTKEY                                                          
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    DISPLAY                                                          
         B     LRHX                                                             
         SPACE 2                                                                
FILTKEY  LA    R3,TIKEY            FILTER ON THE RECORD TYPE                    
         CLI   TIREAD,TLMTNMDQ                                                  
         BNE   FK10                                                             
         SPACE 1                                                                
         USING TLMTPD,R3                                                        
         CLC   TLMTNMTY,TGMTTYPE                                                
         BNE   LRNO                                                             
         OC    TIQSTART,TIQSTART                                                
         BZ    LRYES                                                            
         CLC   TLMTNMNM,TIQSTART                                                
         BL    LRNO                                                             
         B     LRYES                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         USING TLMTD,R3                                                         
FK10     CLC   TLMTTYPE,TGMTTYPE                                                
         BNE   LRNO                                                             
         OC    TIQSTART,TIQSTART                                                
         BZ    LRYES                                                            
         CLC   TLMTCODE,TIQSTART                                                
         BL    LRNO                                                             
         B     LRYES                                                            
         DROP  R3                                                               
         EJECT                                                                  
         USING LINED,R2                                                         
DISPLAY  XC    LINED(LINLNQ),LINED                                              
         SPACE 1                                                                
         USING TLMTD,R4                                                         
         L     R4,TIAREC                                                        
         MVC   LINMKT,TLMTCODE                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP10                                                           
         ZIC   RE,TANALEN                                                       
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LINNAME(0),TANANAME                                              
         MVC   LINWGHT-3(22),SPACES                                             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TASND,R4                                                         
DISP10   L     R4,TIAREC                                                        
         MVI   ELCODE,TASNELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DISP20                                                           
         MVC   LINALPH-3(10),SPACES                                             
         MVC   LINALPH,TASNAME                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMSD,R4                                                         
DISP20   L     R4,TIAREC                                                        
         MVI   ELCODE,TAMSELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP30   BRAS  RE,NEXTEL                                                        
         BNE   DISP40                                                           
         LA    R3,LINWGHT                                                       
         CLI   TAMSTYPE,TAMSTYSP                                                
         BNE   *+8                                                              
         LA    R3,LINSWGT                                                       
         EDIT  TAMSWGHT,(8,(R3)),ZERO=NOBLANK,ALIGN=LEFT                        
         B     DISP30                                                           
         DROP  R2,R4                                                            
         SPACE 1                                                                
DISP40   CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    DISP50                                                           
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             DISPLAY LINE TO SCREEN                       
         B     LRHX                                                             
         SPACE 1                                                                
DISP50   GOTO1 SPOOL,DMCB,(R8)                                                  
         LH    RE,COUNTER          DISPLAY LINE TO REPORT                       
         AHI   RE,1                                                             
         STH   RE,COUNTER                                                       
LRHX     B     LRYES                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL4                                                              
LINMKT   DS    CL6                 MARKET                                       
         DS    CL4                                                              
LINNAME  DS    CL30                MARKET NAME                                  
         DS    CL3                                                              
LINALPH  DS    CL4                 ALPHA                                        
         DS    CL3                                                              
LINWGHT  DS    CL8                 WEIGHT                                       
         DS    CL2                                                              
LINSWGT  DS    CL8                 SPANISH WEIGHT                               
LINLNQ   EQU   *-LINED                                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE1D                                                       
COUNTER  DS    H                   RECORD COUNTER                               
LASTTYPE DS    CL1                                                              
         DS    CL200                                                            
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAGENE1   07/06/04'                                      
         END                                                                    
